library("dplyr")
library("tmap")


## ## heat wave period
## result.file = "sim_result_by_idf_epw.csv"
## annual 2018
year = 2016
result.file = sprintf("intermediate_data/annual_sim_result_by_idf_epw_%d.csv", year)
time.pref = sprintf("annual_%d", year)
result <- readr::read_csv(result.file)

result.by.time <- result %>%
    dplyr::group_by(`Date/Time`) %>%
    dplyr::group_split()

compile.grid.data <- function(grid.level, time.pref) {

    if (grid.level == "coarse") {
        grid.path = "input_data/M02_EnergyPlus_Forcing_Historical_LowRes/meta"
        latlon.path = "input_data/M02_EnergyPlus_Forcing_Historical_LowRes"
        grid.suf = ""
        num.rows = 14
        num.cols = 14
        all.wrf.ids = 0:(num.rows * num.cols - 1)
    } else if (grid.level == "finer"){
        grid.path = "input_data/high res grid for reporting"
        latlon.path = grid.path
        grid.suf = "_finer"
        num.rows = 384
        num.cols = 339
        all.wrf.ids = 0:(num.rows * num.cols - 1)
    } else if (grid.level == "tract") {
        grid.suf = "_tract"
    }

    df.grid = readr::read_csv("intermediate_data/compiled_cell_building.csv")

    if (grid.level == "coarse") {
        df.grid.coarse = df.grid
    } else {
        df.grid.coarse = df.grid
        if (grid.level == "tract") {
            building.to.tract.map <- readr::read_csv("intermediate_data/building_id_to_census_tract.csv")
            df.grid <- df.grid %>%
                dplyr::left_join(building.to.tract.map, by="OBJECTID") %>%
                dplyr::select(-id) %>%
                dplyr::rename(id = id.tract) %>%
                dplyr::select(id, everything())
        } else if (grid.level == "finer") {
            building.to.finer.grid.map <- readr::read_csv("intermediate_data/building_id_to_finer_grid.csv")
            df.grid <- df.grid %>%
                dplyr::left_join(building.to.finer.grid.map, by="OBJECTID") %>%
                dplyr::select(-id) %>%
                dplyr::rename(id = id.grid.finer) %>%
                dplyr::select(id, everything())
        }
    }

    df.grid %>%
        head()

    df.grid.coarse %>%
        head()

    if (!(file.exists(sprintf("intermediate_data/building_info%s.csv", grid.suf)))) {
        df.area <- readr::read_csv("intermediate_data/LA_building_footprint_m2.csv")
        df.vin.type.idf <- readr::read_csv("input_data/type_vintage_to_idf_mapping.csv")
        df.area.prototype = readr::read_csv("input_data/prototype_bldg_area.csv") %>%
            dplyr::mutate(idf.kw = gsub(".idf", "", idf.name, fixed=TRUE)) %>%
            dplyr::mutate(idf.kw = gsub(".", "_", idf.kw, fixed=TRUE)) %>%
            dplyr::select(-idf.name) %>%
            {.}
        building.to.epw.id <- df.grid.coarse %>%
            dplyr::distinct(OBJECTID, id) %>%
            dplyr::rename(epw.id = id)
        building.info <- df.grid %>%
            dplyr::inner_join(df.area, by="OBJECTID") %>%
            dplyr::mutate(building.area.m2 = SQFTmain * 0.0929) %>%
            dplyr::select(id, OBJECTID, vintage, building.type, building.area.m2, FootprintArea.m2) %>%
            dplyr::inner_join(df.vin.type.idf, by=c("building.type", "vintage")) %>%
            dplyr::mutate(idf.kw = gsub(".idf", "", idf.name, fixed=TRUE)) %>%
            dplyr::mutate(idf.kw = gsub(".", "_", idf.kw, fixed=TRUE)) %>%
            dplyr::left_join(df.area.prototype, by="idf.kw") %>%
            dplyr::filter(building.type != "manufactured home") %>%
            dplyr::inner_join(building.to.epw.id, by="OBJECTID") %>%
            {.}
        building.info %>%
          readr::write_csv(sprintf("intermediate_data/building_info%s.csv", grid.suf))
    } else {
        building.info <- readr::read_csv(sprintf("intermediate_data/building_info%s.csv", grid.suf))
    }

    idf.epw.per.grid <- building.info %>%
        dplyr::group_by(id, idf.kw, epw.id, building.type, vintage) %>%
        dplyr::summarise(building.area.m2 = sum(building.area.m2),
                        FootprintArea.m2 = sum(FootprintArea.m2),
                        prototype.m2 = first(prototype.m2)) %>%
        dplyr::ungroup() %>%
        {.}

    idf.epw.per.grid

    ## memory issue with mclapply, gets slower and slower towards the end of the loop
    ## set.seed(0)
    ## ## looping like this to prevent memory from running out
    ## building.heat.grid <- parallel::mclapply(result.by.time, function(df.time.i) {
    ##     print(df.time.i$`Date/Time`[[1]])
    ##     ## print(df.time.i$`Date`[[1]])
    ##     result.time.i <- idf.epw.per.grid %>%
    ##         dplyr::left_join(df.time.i, by=c("idf.kw", "epw.id")) %>%
    ##         ## dplyr::select(-idf.name) %>%
    ##         tidyr::gather(variable, value, emission.exh:energy.gas) %>%
    ##         dplyr::mutate(value = value / prototype.m2 * building.area.m2) %>%
    ##         dplyr::group_by(`Date/Time`, id, variable) %>%
    ##         dplyr::summarise(value = sum(value)) %>%
    ##         dplyr::ungroup() %>%
    ##         {.}
    ##     result.time.i
    ## }, mc.cores = 4) %>%
    ##     dplyr::bind_rows()

    ## looping like this to prevent memory from running out
    for (df.time.i in result.by.time) {
        timestamp = df.time.i$`Date/Time`[[1]]
        print(timestamp)
        ## print(df.time.i$`Date`[[1]])
        result.time.i <- idf.epw.per.grid %>%
            dplyr::left_join(df.time.i, by=c("idf.kw", "epw.id")) %>%
            tidyr::gather(variable, value, emission.exfiltration:energy.gas) %>%
            dplyr::mutate(value = value / prototype.m2 * building.area.m2) %>%
            dplyr::group_by(`Date/Time`, id, variable) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            tidyr::spread(variable, value) %>%
            dplyr::rename(geoid = id, timestamp=`Date/Time`) %>%
            dplyr::select(geoid, timestamp, starts_with("emission"), starts_with("energy")) %>%
            {.}
        result.time.i %>%
            readr::write_csv(sprintf("intermediate_data/hourly_heat_energy/%s%s/%s.csv",
                                    time.pref, grid.suf,
                                    gsub("[/:]", "_", timestamp)))
    }

    months = sprintf("%02d", 1:12)

    ## write to output heat energy file
    if (grid.level != "finer") {
        result.time.files <- list.files(sprintf("intermediate_data/hourly_heat_energy/%s%s", time.pref, grid.suf), "*.csv")
        lapply(result.time.files, function(f) {
            print(f)
            readr::read_csv(sprintf("intermediate_data/hourly_heat_energy/%s%s/%s", time.pref, grid.suf, f),
                            col_types = readr::cols())
        }) %>%
            dplyr::bind_rows() %>%
            readr::write_csv(sprintf("output_data/hourly_heat_energy/%s%s.csv", time.pref, grid.suf))
    } else {
        for (month.str in months) {
            result.time.files <- list.files(sprintf("intermediate_data/hourly_heat_energy/%s%s", time.pref, grid.suf),
                                            pattern=sprintf("^%s_*", month.str))
            df.month <- lapply(result.time.files, function(f) {
                print(f)
                readr::read_csv(sprintf("intermediate_data/hourly_heat_energy/%s%s/%s", time.pref, grid.suf, f),
                                col_types = readr::cols())
            }) %>%
                dplyr::bind_rows() %>%
                {.}
            df.month %>%
                readr::write_csv(sprintf("output_data/hourly_heat_energy/%s%s_%s.csv", time.pref, grid.suf, month.str))
        }
    }

    annual.sim.result <- readr::read_csv(sprintf("intermediate_data/annual_total_result_%d.csv", year))

    idf.epw.per.grid %>%
        dplyr::left_join(annual.sim.result, by=c("idf.kw", "epw.id")) %>%
        tidyr::gather(variable, value, emission.exfiltration:energy.gas) %>%
        dplyr::mutate(value = value / prototype.m2 * building.area.m2) %>%
        tidyr::spread(variable, value) %>%
        readr::write_csv(sprintf("intermediate_data/annual_heat_energy/%d_by_grid_id_type%s.csv", year, grid.suf))

    monthly.sim.result <- readr::read_csv(sprintf("intermediate_data/monthly_total_result_%d.csv", year))

    idf.epw.per.grid %>%
        dplyr::left_join(monthly.sim.result, by=c("idf.kw", "epw.id")) %>%
        tidyr::gather(variable, value, emission.exfiltration:energy.gas) %>%
        dplyr::mutate(value = value / prototype.m2 * building.area.m2) %>%
        tidyr::spread(variable, value) %>%
        readr::write_csv(sprintf("intermediate_data/monthly_heat_energy/%d_by_grid_id_type%s_.csv", year, grid.suf))

}

for (grid.level in c("coarse", "finer", "tract")) {
    compile.grid.data(grid.level, time.pref)

    ## get monthly total, and monthly diurnal profile (month-hour average)
    if (grid.level == "finer") {
        lapply(months, function(month.str) {
            print(month.str)
            df.month.i <-
                readr::read_csv(sprintf("output_data/hourly_heat_energy/%s%s_%s.csv", time.pref, grid.suf, month.str))
            df.month.i %>%
                dplyr::group_by(geoid) %>%
                dplyr::summarise_if(is.numeric, sum) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(month = month.str) %>%
                dplyr::select(month, geoid, everything())
        }) %>%
            dplyr::bind_rows() %>%
            readr::write_csv(sprintf("intermediate_data/monthly_heat_energy/%d_monthly%s.csv", year, grid.suf))
        lapply(months, function(month.str) {
            print(month.str)
            df.month.i <-
                readr::read_csv(sprintf("output_data/hourly_heat_energy/%s%s_%s.csv", time.pref, grid.suf, month.str))
            df.month.i %>%
                dplyr::mutate(month = month.str) %>%
                tidyr::separate(timestamp, into=c("Date", "Hour"), sep="  ") %>%
                dplyr::mutate(Hour=as.numeric(gsub(":00:00", "", Hour))) %>%
                dplyr::group_by(month, Hour, geoid) %>%
                dplyr::summarise_if(is.numeric, mean) %>%
                dplyr::ungroup()
        }) %>%
            dplyr::bind_rows() %>%
            readr::write_csv(sprintf("intermediate_data/diurnal/%s%s_hourly_avg_month.csv", time.pref, grid.suf))
    } else {
        df.year <- readr::read_csv(sprintf("output_data/hourly_heat_energy/%s%s.csv", time.pref, grid.suf))
        df.year %>%
            tidyr::separate(timestamp, into=c("month", "other"), sep="/") %>%
            dplyr::select(-other) %>%
            dplyr::group_by(month, geoid) %>%
            dplyr::summarise_if(is.numeric, sum) %>%
            dplyr::ungroup() %>%
            readr::write_csv(sprintf("intermediate_data/monthly_heat_energy/%d_monthly%s.csv", year, grid.suf))
        print(sprintf("write to: intermediate_data/monthly_heat_energy/%d_monthly%s.csv", year, grid.suf))
        df.year %>%
            tidyr::separate(timestamp, into=c("month", "other"), sep="/") %>%
            tidyr::separate(other, into=c("Date", "Hour"), sep="  ") %>%
            dplyr::mutate(Hour=as.numeric(gsub(":00:00", "", Hour))) %>%
            dplyr::select(-Date) %>%
            dplyr::group_by(month, Hour, geoid) %>%
            dplyr::summarise_if(is.numeric, mean) %>%
            dplyr::ungroup() %>%
            readr::write_csv(sprintf("intermediate_data/diurnal/%s%s_hourly_avg_month.csv", time.pref, grid.suf))
        print(sprintf("write to: intermediate_data/diurnal/%s%s_hourly_avg_month.csv", time.pref, grid.suf))
    }
}

