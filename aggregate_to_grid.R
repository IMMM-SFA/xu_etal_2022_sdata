library("dplyr")
library("tmap")

## grid.level = "finer"
grid.level = "coarse"
## grid.level = "tract"

if (grid.level == "coarse") {
    grid.path = "M02_EnergyPlus_Forcing_Historical_LowRes/meta"
    latlon.path = "M02_EnergyPlus_Forcing_Historical_LowRes"
    grid.suf = ""
    num.rows = 14
    num.cols = 14
} else if (grid.level == "finer"){
    grid.path = "high res grid for reporting"
    latlon.path = grid.path
    grid.suf = "_finer"
    num.rows = 384
    num.cols = 339
} else if (grid.level == "tract") {
    grid.suf = "_tract"
}
all.wrf.ids = 0:(num.rows * num.cols - 1)

## ## heat wave period
## result.file = "sim_result_by_idf_epw.csv"
## annual 2018
result.file = "annual_sim_result_by_idf_epw_2018.csv"
time.pref = "annual_2018"
result <- readr::read_csv(result.file) %>%
    {.}

result.by.time <- result %>%
    dplyr::select(-starts_with("energy"), -emission.add.surf, -emission.exh, -emission.overall) %>%
    dplyr::group_by(`Date/Time`) %>%
    dplyr::group_split()

## result.by.day <- result %>%
##     tidyr::separate(`Date/Time`, into=c("Date", "Hour"), sep="  ", remove = FALSE) %>%
##     dplyr::select(-Hour) %>%
##     dplyr::group_by(`Date`) %>%
##     dplyr::group_split() %>%
##     {.}

df.grid = readr::read_csv(sprintf("compiled_cell_building%s.csv", grid.suf))

if (grid.level == "coarse") {
    df.grid.coarse = df.grid
} else {
    df.grid.coarse = readr::read_csv("compiled_cell_building.csv")
}

df.grid %>%
    head()

df.grid.coarse %>%
    head()

df.area <- readr::read_csv("LA_building_footprint_m2.csv")

df.vin.type.idf <- readr::read_csv("type_vintage_to_idf_mapping.csv") %>%
    dplyr::select(-building.count) %>%
    {.}

df.area.prototype = readr::read_csv("prototype_bldg_area.csv") %>%
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
    summary()

building.info %>%
    head()

building.info %>%
  readr::write_csv(sprintf("building_info%s.csv", grid.suf))

idf.epw.per.grid <- building.info %>%
    dplyr::group_by(id, idf.kw, epw.id, building.type, vintage) %>%
    dplyr::summarise_at(vars(building.area.m2, FootprintArea.m2, prototype.m2), sum) %>%
    dplyr::ungroup() %>%
    {.}

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

data.grids <- idf.epw.per.grid %>%
    dplyr::group_by(id) %>%
    dplyr::group_split()

annual.total <- parallel::mclapply(seq_along(data.grids), function(j) {
    ## if (j %% 100 == 0) {
    ##     print(j)
    ## }
    print(j)
    data.grids[[j]] %>% dplyr::left_join(result, by=c("idf.kw", "epw.id")) %>%
      dplyr::select(-emission.add.surf, -emission.exh, -emission.overall, -energy.overall) %>%
      dplyr::group_by(id, idf.kw, building.type, vintage) %>%
      dplyr::summarise_at(vars(emission.exfiltration:energy.gas), sum) %>%
      dplyr::ungroup() %>%
      {.}
}, mc.cores = 3) %>%
    dplyr::bind_rows()

annual.total %>%
    readr::write_csv(sprintf("annual_heat_by_grid_id_type%s.csv", grid.suf))

result <- result %>%
    tidyr::separate(`Date/Time`, into=c("month", "other"), sep="/") %>%
    dplyr::select(-other) %>%
    {.}

monthly.total <- parallel::mclapply(seq_along(data.grids), function(j) {
    ## if (j %% 100 == 0) {
    ##     print(j)
    ## }
    print(j)
    data.grids[[j]] %>% dplyr::left_join(result, by=c("idf.kw", "epw.id")) %>%
        dplyr::select(-emission.add.surf, -emission.exh, -emission.overall, -energy.overall) %>%
        dplyr::group_by(month, id, idf.kw, building.type, vintage) %>%
        dplyr::summarise_at(vars(emission.exfiltration:energy.gas), sum) %>%
        dplyr::ungroup() %>%
        {.}
}, mc.cores = 3) %>%
    dplyr::bind_rows()

monthly.total %>%
    readr::write_csv(sprintf("monthly_heat_by_grid_id_type%s.csv", grid.suf))

## looping like this to prevent memory from running out
for (df.time.i in result.by.time) {
    timestamp = df.time.i$`Date/Time`[[1]]
    print(timestamp)
    ## print(df.time.i$`Date`[[1]])
    result.time.i <- idf.epw.per.grid %>%
        dplyr::left_join(df.time.i, by=c("idf.kw", "epw.id")) %>%
        ## dplyr::select(-idf.name) %>%
        tidyr::gather(variable, value, emission.exfiltration:emission.surf) %>%
        dplyr::mutate(value = value / prototype.m2 * building.area.m2) %>%
        dplyr::group_by(`Date/Time`, id, variable) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::ungroup() %>%
        {.}
    result.time.i %>%
        readr::write_csv(sprintf("aggregated_heat/%s%s/%s.csv", time.pref, grid.suf, gsub("[/:]", "_", timestamp)))
}

readr::read_csv(sprintf("aggregated_heat/%s%s/01_01  01_00_00.csv", time.pref, grid.suf))

months = sprintf("%02d", 1:12)

grid.suf = "_finer"

monthly.total <- lapply(months, function(month.str) {
    print(month.str)
    heat.by.time.files = list.files(sprintf("aggregated_heat/%s%s/", time.pref, grid.suf),
                                    pattern=sprintf("^%s_*", month.str))
    month.data <- lapply(heat.by.time.files, function(f) {
        print(f)
        df <- readr::read_csv(sprintf("aggregated_heat/%s%s/%s", time.pref, grid.suf, f), col_types = readr::cols())
    }) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(month = month.str) %>%
        dplyr::group_by(month, id, variable) %>%
        dplyr::summarise_if(is.numeric, sum) %>%
        dplyr::ungroup()
    month.data
}) %>%
    dplyr::bind_rows()

monthly.total %>%
    readr::write_csv(sprintf("aggregated_heat/%s%s_monthly.csv", time.pref, grid.suf))

hourly.avg.per.month <- lapply(months, function(month.str) {
    print(month.str)
    heat.by.time.files = list.files(sprintf("aggregated_heat/%s%s/", time.pref, grid.suf),
                                    pattern=sprintf("^%s_*", month.str))
    month.data <- lapply(heat.by.time.files, function(f) {
        print(f)
        df <- readr::read_csv(sprintf("aggregated_heat/%s%s/%s", time.pref, grid.suf, f), col_types = readr::cols())
    }) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(month = month.str) %>%
        tidyr::separate(`Date/Time`, into=c("Date", "Hour"), sep="  ") %>%
        dplyr::mutate(Hour=as.numeric(gsub(":00:00", "", Hour))) %>%
        dplyr::group_by(month, Hour, id, variable) %>%
        dplyr::summarise_if(is.numeric, mean) %>%
        dplyr::ungroup()
    month.data
}) %>%
    dplyr::bind_rows()

hourly.avg.per.month %>%
    readr::write_csv(sprintf("aggregated_heat/%s%s_hourly_avg_month.csv", time.pref, grid.suf))

heat.by.time.files %>%
    head()

building.size.cnt.wrf.grid <- building.info %>%
    dplyr::mutate(building.count = 1) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise_at(vars(building.area.m2, FootprintArea.m2, building.count), sum) %>%
    dplyr::ungroup() %>%
    {.}

building.heat.grid <- building.heat.grid %>%
    dplyr::mutate(energy.GJ = value * 1e-9) %>%
    dplyr::mutate(energy.MJ = value * 1e-6) %>%
    dplyr::select(-value) %>%
    dplyr::mutate(energy.kwh = energy.GJ * 277.778) %>%
    dplyr::left_join(building.size.cnt.wrf.grid, by="id") %>%
    dplyr::mutate(kwh.per.building.area.m2 = energy.kwh / building.area.m2,
                  kwh.per.footprint.area.m2 = energy.kwh / FootprintArea.m2) %>%
    {.}

building.heat.grid %>%
    readr::write_csv(sprintf("%sheat_energy_wrf_grid_time%s.csv", time.pref, grid.suf))

building.heat.grid <- readr::read_csv(sprintf("heat_energy_wrf_grid_time%s.csv", grid.suf))

snapshot <- building.heat.grid %>%
    dplyr::filter(`Date/Time` == "07/11  10:00:00") %>%
    dplyr::filter(variable == "emission.overall") %>%
    {.}

grid.geo <- sf::st_read(sprintf("%s/wrf-grids-origin.geojson", grid.path))

grid.geo %>%
    dplyr::left_join(snapshot, by="id") %>%
    dplyr::select(id, energy.MJ) %>%
    sf::st_as_sf() %>%
    tm_shape() +
    tm_polygons("energy.MJ", n=5, style="quantile", palette="YlOrRd")

output.vars = c("emission.overall", "emission.rej", "emission.exh", "emission.ref")
output.labels = c("BLDGHEATE", "REJ", "EXH", "REF")

## suf.unit = "kwh_per_footprint_m2"
## suf.unit = "kwh_per_building_m2"
suf.unit = "MJ"
if (suf.unit == "kwh_per_footprint_m2") {
    building.heat.grid <- building.heat.grid %>%
        dplyr::mutate(value = kwh.per.footprint.area.m2) %>%
        {.}
} else if (suf.unit == "kwh_per_building_m2") {
    building.heat.grid <- building.heat.grid %>%
        dplyr::mutate(value = kwh.per.building.area.m2) %>%
        {.}
} else if (suf.unit == "MJ") {
    building.heat.grid <- building.heat.grid %>%
        dplyr::mutate(value = energy.MJ) %>%
        {.}
}

for (i in seq_along(output.vars)) {
  output.var = output.vars[[i]]
  output.label = output.labels[[i]]
  print(output.var)
  dfs.heat.grid <- building.heat.grid %>%
    ## dplyr::filter(!is.na(`Date/Time`)) %>%
    dplyr::filter(variable == output.var) %>%
    dplyr::select(`Date/Time`, id, value) %>%
    dplyr::group_by(`Date/Time`) %>%
    dplyr::group_split() %>%
    {.}
  ## export final heat emissoin data to grid: emission.overall
  for (df.i in dfs.heat.grid) {
    df.i <- df.i %>%
        tidyr::separate(`Date/Time`, into=c("Date", "Hour"), sep="  ") %>%
        dplyr::mutate(Hour=as.numeric(gsub(":00:00", "", Hour))) %>%
        dplyr::mutate(Hour = Hour - 1) %>%
        dplyr::mutate(local.time = as.POSIXct(sprintf("2018/%s %s:00:00", `Date`, Hour), tz = "America/Los_Angeles")) %>%
        dplyr::mutate(utc.time = lubridate::with_tz(local.time, tzone="GMT")) %>%
        dplyr::mutate(utc.str = as.character(utc.time, format="%Y%m%d%H")) %>%
        {.}
    datehour.i = df.i$utc.str[[1]]
    print(sprintf("%s", datehour.i))
    mat <- tibble::tibble(wrf.grid.id = all.wrf.ids) %>%
      dplyr::left_join(df.i, by=c("wrf.grid.id" = "id")) %>%
      dplyr::select(wrf.grid.id, value) %>%
      tidyr::replace_na(list(value="NaN")) %>%
      .$value %>%
        matrix(nrow=num.rows, byrow=TRUE) %>%
      {.}
    ## units: kwh/m2 building footprint
    mat %>%
        write.table(file=sprintf("eplus-final-results-matrix_%s%s/Variable_%s_%s.txt",
                                 suf.unit, grid.suf, output.label, datehour.i),
                    row.names = FALSE, col.names = FALSE, sep=",", quote=FALSE)

  }
}
