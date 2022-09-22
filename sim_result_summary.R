library("dplyr")

df = readr::read_csv("intermediate_data/epw_idf_to_simulate.csv")

## df %>%
##   distinct(idf.name) %>%
##   dplyr::arrange(idf.name) %>%
##   readr::write_csv("prototype_bldg_area.csv")

df.area.prototype = readr::read_csv("prototype_bldg_area.csv") %>%
  dplyr::mutate(idf.kw = gsub(".idf", "", idf.name, fixed=TRUE)) %>%
  dplyr::mutate(idf.kw = gsub(".", "_", idf.kw, fixed=TRUE)) %>%
  dplyr::select(-idf.name) %>%
  {.}

dirs <- df %>%
  dplyr::mutate(dirname = paste0(idf.name, "____", id)) %>%
  dplyr::mutate(dirname = gsub(".idf", "", dirname, fixed = TRUE)) %>%
  dplyr::mutate(dirname = gsub(".", "_", dirname, fixed = TRUE)) %>%
  .$dirname

dirs.subset = dirs[which(stringr::str_detect(dirs, "Nursing"))]

## data.res = "July"
data.res = "annual"
if (data.res == "annual") {
  suf = "_ann"
} else {
  suf = ""
}

## result.dir = "result_ann_0520"
## result.csv.dir = sprintf("sim_result%s_0520_csv/%s.csv", suf, dirname)
## annual results with 2018 WRF data
result.dir = "result_ann_WRF_2018"
result.csv.dir = sprintf("sim_result%s_WRF_2018_csv", suf)
## result.dir = "result_ann_WRF_2016"
## result.csv.dir = sprintf("sim_result%s_WRF_2016_csv", suf)

length(dirs)

for (dirname in dirs) {
  print(dirname)
  if (data.res == "annual") {
    output.name = sprintf("%s/%s/eplusout.csv", result.dir, dirname)
    ## output.name = sprintf("result_ann/%s/eplusout.csv", dirname)
    if (file.exists(output.name)) {
      file.copy(output.name, sprintf("%s/%s.csv", result.csv.dir, dirname))
      print(sprintf("copy to %s/%s.csv",  result.csv.dir, dirname))
    }
  } else {
    ## use this for july simulation result
    output.name = sprintf("idf_add_sim_period_output/%s/eplusout.csv", dirname)
    print(output.name)
    if (file.exists(output.name)) {
      file.copy(output.name, sprintf("sim_result%s_csv/%s.csv", suf, dirname))
    }
  }
}

files = list.files(path=result.csv.dir, pattern = "*.csv")
## files = list.files(path=sprintf("sim_result%s_csv", suf), pattern = "*.csv")

files.kw = gsub(".csv", "", files)

setdiff(dirs, files.kw)
## all files are processed

## colname = "Environment:Site Total Zone Exhaust Air Heat Loss [J](Hourly)"
colname = "Environment:Site Total Surface Heat Emission to Air [J](Hourly)"
with.missing.var <- lapply(seq_along(files), function(i) {
  f = files[i]
  ## print(i)
  df = readr::read_csv(sprintf("%s/%s", result.csv.dir, f), col_types = readr::cols()) %>%
    {.}
  if (!(colname %in% names(df))) {
    return(f)
  }
})

unlist(with.missing.var)

## read simulation results for annual
result.ann <- lapply(files, function(f) {
  tokens = unlist(stringr::str_split(f, pattern = "____"))
  idf.kw = tokens[[1]]
  epw.id = gsub(".csv", "", tokens[[2]])
  df = readr::read_csv(sprintf("%s/%s", result.csv.dir, f), col_types = readr::cols()) %>%
    ## df = readr::read_csv(sprintf("sim_result_ann_csv/%s", f), col_types = readr::cols()) %>%
    dplyr::mutate(emission.exh = `Environment:Site Total Zone Exfiltration Heat Loss [J](Hourly)`+
                      `Environment:Site Total Zone Exhaust Air Heat Loss [J](Hourly)`,
                  emission.exfiltration = `Environment:Site Total Zone Exfiltration Heat Loss [J](Hourly)`,
                  emission.exhaust = `Environment:Site Total Zone Exhaust Air Heat Loss [J](Hourly)`,
                  emission.ref = `SimHVAC:Air System Relief Air Total Heat Loss Energy [J](Hourly)`,
                  emission.rej = `SimHVAC:HVAC System Total Heat Rejection Energy [J](Hourly)`,
                  emission.surf = `Environment:Site Total Surface Heat Emission to Air [J](Hourly)`,
                  emission.add.surf = emission.exh + emission.ref + emission.rej + emission.surf,
                  emission.overall = emission.exh + emission.ref + emission.rej) %>%
    dplyr::mutate(energy.elec = `Electricity:Facility [J](Hourly)`) %>%
    dplyr::mutate(energy.overall = energy.elec) %>%
    dplyr::mutate(idf.kw = idf.kw, epw.id = epw.id) %>%
    {.}
  if ("NaturalGas:Facility [J](Hourly)" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(energy.gas = `NaturalGas:Facility [J](Hourly)`) %>%
      dplyr::mutate(energy.overall = energy.elec + energy.gas)
  }
  if (nrow(df) != 8760) {
    print(sprintf("%s: %d", f, nrow(df)))
  }
  df <- df %>%
    dplyr::select(`Date/Time`, idf.kw, epw.id, starts_with("emission."), starts_with("energy"))
  df
}) %>%
  dplyr::bind_rows()

result.ann %>%
    names()

## result <- result %>%
##   dplyr::filter(stringr::str_detect(`Date/Time`, "07/")) %>%
##   dplyr::mutate(epw.id = as.numeric(epw.id)) %>%
##   {.}

result.ann %>%
  dplyr::distinct(idf.kw) %>%
  readr::write_csv("idf_kw.csv")

idf.kw.to.usetype <- readr::read_csv("idf_kw_to_EnergyAtlas_usetype.csv") %>%
  {.}

result.ann %>%
    head()

result.ann %>%
  readr::write_csv("annual_sim_result_by_idf_epw_2016.csv")
## readr::write_csv("annual_sim_result_by_idf_epw_2018.csv")
## readr::write_csv("annual_sim_result_by_idf_epw.csv")

## read simulation results for July
result <- lapply(files, function(f) {
  tokens = unlist(stringr::str_split(f, pattern = "____"))
  idf.kw = tokens[[1]]
  epw.id = gsub(".csv", "", tokens[[2]])
  df = readr::read_csv(sprintf("sim_result_csv/%s", f), col_types = readr::cols()) %>%
    dplyr::mutate(emission.exh = `Environment:Site Total Zone Exfiltration Heat Loss [J](Hourly)`+
                    `Environment:Site Total Zone Exhaust Air Heat Loss [J](Hourly)`,
                  emission.exfiltration = `Environment:Site Total Zone Exfiltration Heat Loss [J](Hourly)`,
                  emission.exhaust = `Environment:Site Total Zone Exhaust Air Heat Loss [J](Hourly)`,
                  emission.ref = `SimHVAC:Air System Relief Air Total Heat Loss Energy [J](Hourly)`,
                  emission.rej = `SimHVAC:HVAC System Total Heat Rejection Energy [J](Hourly)`,
                  emission.surf = `Environment:Site Total Surface Heat Emission to Air [J](Hourly)`,
                  emission.add.surf = emission.exh + emission.ref + emission.rej + emission.surf,
                  emission.overall = emission.exh + emission.ref + emission.rej) %>%
    dplyr::mutate(energy.elec = `Electricity:Facility [J](Hourly)`) %>%
    dplyr::mutate(energy.overall = energy.elec) %>%
    dplyr::mutate(idf.kw = idf.kw, epw.id = epw.id) %>%
    {.}
  if ("NaturalGas:Facility [J](Hourly)" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(energy.gas = `NaturalGas:Facility [J](Hourly)`) %>%
      dplyr::mutate(energy.overall = energy.elec + energy.gas)
    print(sprintf("%s has gas", f))
  }
  ## June 29th to July 31st inclussive
  if (nrow(df) != 792) {
    print(sprintf("%s: %d", f, nrow(df)))
  }
  df <- df %>%
    dplyr::select(`Date/Time`, idf.kw, epw.id, starts_with("emission."), starts_with("energy"))
  df
}) %>%
  dplyr::bind_rows()

result <- result %>%
  ## remove design day data
  dplyr::filter(!stringr::str_detect(`Date/Time`, "01/21")) %>%
  dplyr::filter(!stringr::str_detect(`Date/Time`, "08/21")) %>%
  dplyr::mutate(epw.id = as.numeric(epw.id)) %>%
  {.}

result %>%
  readr::write_csv("sim_result_by_idf_epw.csv")

## fixme: added emission.exfiltration, emission.exhaust columns, adjust the following code accordingly

result <- readr::read_csv("sim_result_by_idf_epw.csv") %>%
    {.}

options(tibble.width=Inf)

df.grid = readr::read_csv("compiled_cell_building.csv")
head(df.grid)

## need to get footprint area

if (!file.exists("LA_building_footprint_m2.csv")) {
    df.building <- sf::st_read("~/Dropbox/workLBNL/CityBES/LA/data-raw/compiled_LA_building.geojson")
    df.building.valid <- sf::st_make_valid(df.building)
    area <- df.building.valid %>%
        sf::st_area()
    df.building$FootprintArea.m2 = as.vector(area)
    df.area = df.building
    sf::st_geometry(df.area) <- NULL
    df.area <- df.area %>%
        tibble::as_tibble() %>%
        dplyr::select(OBJECTID, FootprintArea.m2)
    df.area %>%
        readr::write_csv("LA_building_footprint_m2.csv")
} else {
    df.area <- readr::read_csv("LA_building_footprint_m2.csv")
}


df.vin.type.idf <- readr::read_csv("type_vintage_to_idf_mapping.csv") %>%
  dplyr::select(-building.count) %>%
  {.}

result.by.time <- result %>%
  dplyr::group_by(`Date/Time`) %>%
  dplyr::group_split()

building.info <- df.grid %>%
  dplyr::inner_join(df.area, by="OBJECTID") %>%
  dplyr::mutate(building.area.m2 = SQFTmain * 0.0929) %>%
  dplyr::select(id, OBJECTID, vintage, building.type, building.area.m2, FootprintArea.m2) %>%
  dplyr::rename(epw.id=id) %>%
  dplyr::inner_join(df.vin.type.idf, by=c("building.type", "vintage")) %>%
  dplyr::mutate(idf.kw = gsub(".idf", "", idf.name, fixed=TRUE)) %>%
  dplyr::mutate(idf.kw = gsub(".", "_", idf.kw, fixed=TRUE)) %>%
  dplyr::left_join(df.area.prototype, by="idf.kw") %>%
  dplyr::filter(building.type != "manufactured home") %>%
  {.}

## checks whether there are missing data, if so, resimulate the missing ones
building.heat.grid.sub <- lapply(result.by.time, function(df.time.i) {
  print(df.time.i$`Date/Time`[[1]])
  result.time.i <- building.info %>%
    dplyr::left_join(df.time.i, by=c("idf.kw", "epw.id")) %>%
    dplyr::filter(is.na(`Date/Time`)) %>%
    distinct(epw.id, building.type, vintage) %>%
    {.}
  result.time.i
}) %>%
  dplyr::bind_rows()

## output files not simulated or have missing timestamps
building.heat.grid.sub %>%
  dplyr::left_join(df.vin.type.idf, by=c("building.type", "vintage")) %>%
  readr::write_csv("to_simulate_round2.csv")

building.heat.grid <- lapply(result.by.time, function(df.time.i) {
  print(df.time.i$`Date/Time`[[1]])
  result.time.i <- building.info %>%
    dplyr::left_join(df.time.i, by=c("idf.kw", "epw.id")) %>%
    dplyr::select(-idf.name) %>%
    tidyr::gather(variable, value, emission.exh:energy.gas) %>%
    dplyr::mutate(value = value / prototype.m2 * building.area.m2) %>%
    dplyr::group_by(`Date/Time`, epw.id, variable) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    {.}
  result.time.i
}) %>%
  dplyr::bind_rows()

building.heat.grid %>%
  readr::write_csv("building_heat_grid_original_unit.csv")

building.heat.grid <- readr::read_csv("building_heat_grid_original_unit.csv")

building.info = readr::read_csv("building_info.csv")

building.size.cnt.wrf.grid <- building.info %>%
    dplyr::mutate(building.count = 1) %>%
    dplyr::group_by(epw.id) %>%
    dplyr::summarise_at(vars(building.area.m2, FootprintArea.m2, building.count), sum) %>%
    dplyr::ungroup() %>%
    {.}

building.heat.grid <- building.heat.grid %>%
  dplyr::mutate(energy.GJ = value * 1e-9) %>%
  dplyr::mutate(energy.MJ = value * 1e-6) %>%
  dplyr::select(-value) %>%
  dplyr::mutate(energy.kwh = energy.GJ * 277.778) %>%
  dplyr::left_join(building.size.cnt.wrf.grid, by="epw.id") %>%
  dplyr::mutate(kwh.per.building.area.m2 = energy.kwh / building.area.m2,
                kwh.per.footprint.area.m2 = energy.kwh / FootprintArea.m2) %>%
  {.}

building.heat.grid %>%
  readr::write_csv("heat_energy_wrf_grid_time.csv")

building.heat.grid <- readr::read_csv("heat_energy_wrf_grid_time.csv")

all.wrf.ids = 0:195
num.rows = 14

building.heat.grid %>%
    distinct(variable)

## export small grid level result
## suf = "kwh_per_footprint_m2"
## suf = "kwh_per_building_m2"
suf = "MJ"
if (suf == "kwh_per_footprint_m2") {
    building.heat.grid <- building.heat.grid %>%
        dplyr::mutate(value = kwh.per.footprint.area.m2) %>%
        {.}
} else if (suf == "kwh_per_building_m2") {
    building.heat.grid <- building.heat.grid %>%
        dplyr::mutate(value = kwh.per.building.area.m2) %>%
        {.}
} else if (suf == "MJ") {
    building.heat.grid <- building.heat.grid %>%
        dplyr::mutate(value = energy.MJ) %>%
        {.}
}
output.vars = c("emission.overall", "emission.rej", "emission.exh", "emission.ref")
output.labels = c("BLDGHEATE", "REJ", "EXH", "REF")
for (i in seq_along(output.vars)) {
  output.var = output.vars[[i]]
  output.label = output.labels[[i]]
  dfs.heat.grid <- building.heat.grid %>%
    dplyr::filter(!is.na(`Date/Time`)) %>%
    dplyr::filter(variable == output.var) %>%
    dplyr::select(`Date/Time`, epw.id, value) %>%
    dplyr::group_by(`Date/Time`) %>%
    dplyr::group_split() %>%
    {.}
  ## export final heat emissoin data to grid: emission.overall
  for (df.i in dfs.heat.grid) {
    date.i = gsub("/", "", substr(df.i$`Date/Time`[[1]], 1, 5))
    hour.i = substr(df.i$`Date/Time`[[1]], 8, 9)
    datehour.i = paste0("2018", date.i, hour.i)
    print(sprintf("%s", datehour.i))
    mat <- tibble::tibble(wrf.grid.id = all.wrf.ids) %>%
      dplyr::left_join(df.i, by=c("wrf.grid.id" = "epw.id")) %>%
      dplyr::select(wrf.grid.id, value) %>%
      tidyr::replace_na(list(value=0)) %>%
      .$value %>%
      matrix(nrow=num.rows) %>%
      {.}
    ## flip vertically
    mat <- mat[c(num.rows:1),,drop=FALSE]
    ## units: kwh/m2 building footprint
    mat %>%
      write.table(file=sprintf("eplus-final-results-matrix_%s/Variable_%s_%s.txt", suf,
      output.label, datehour.i), row.names = FALSE, col.names = FALSE, sep=",")
  }
}

to.plot <- building.heat.grid %>%
  dplyr::mutate(datetime = as.POSIXct(sprintf("2018/%s", `Date/Time`), format="%Y/%m/%d  %H:%M:%S")) %>%
  dplyr::mutate(day.of.week = lubridate::wday(datetime, label=TRUE),
                hour.of.day = as.numeric(format(datetime, "%H"))) %>%
  dplyr::mutate(is.weekday = !day.of.week %in% c("Sun", "Sat")) %>%
  dplyr::mutate(is.day.time = (8 < hour.of.day) & (hour.of.day < 18)) %>%
  {.}

pal.n = 5
one.direction.pal.values = RColorBrewer::brewer.pal("YlOrRd", n = pal.n)
two.direction.pal.values = c(one.direction.pal.values, rev(one.direction.pal.values)[2:pal.n])

to.plot %>%
  dplyr::group_by(datetime, variable) %>%
  dplyr::mutate(value = kwh.per.footprint.area.m2) %>%
  dplyr::summarise_at(vars(value),
                      tibble::lst(
                                "10th"=~quantile(., probs=0.1),
                                  "20th"=~quantile(., probs=0.2),
                                  "30th"=~quantile(., probs=0.3),
                                  "40th"=~quantile(., probs=0.4),
                                  "50th"=~quantile(., probs=0.5),
                                  "60th"=~quantile(., probs=0.6),
                                  "70th"=~quantile(., probs=0.7),
                                  "80th"=~quantile(., probs=0.8),
                                  "90th"=~quantile(., probs=0.9)
                                  )) %>%
  dplyr::ungroup() %>%
  tidyr::gather(percentile, value, `10th`:`90th`) %>%
  ## convert to wh per m2
  dplyr::mutate(value = value * 1000) %>%
  dplyr::filter(variable %in% c("emission.overall", "energy.elec", "energy.gas")) %>%
  dplyr::mutate_at(vars(variable), recode, "emission.overall"="heat emission", "energy.elec"="electricity usage", "energy.gas"="gas usage") %>%
  ## dplyr::mutate(variable=factor(variable, levels=c("emission.exh", "emission.ref", "emission.rej", "emission.overall",
  ##                                                   "energy.elec", "energy.gas", "energy.overall"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=datetime, y=value, color=percentile,
                               group=interaction(variable, percentile))) +
  ## ggplot2::scale_color_manual(values=c("#EFF3FF", "#BDD7E7", "#6BAED6",
  ##                                       "#3182BD", "#08519C", "#3182BD",
  ##                                       "#6BAED6", "#BDD7E7", "#EFF3FF")) +
  ggplot2::scale_color_manual(values=two.direction.pal.values) +
  ggplot2::geom_path(size=0.2) +
  ggplot2::ggtitle("Percentile of hourly building emission and energy usage at wrf grid level") +
  ggplot2::ylab("Wh/m2 building footprint area") +
  ggplot2::geom_vline(xintercept = as.POSIXct("2018-07-06 00:00:00"), linetype = "dashed") +
  ggplot2::geom_vline(xintercept = as.POSIXct("2018-07-13 00:00:00"), linetype = "dashed") +
  ggplot2::facet_wrap(.~variable, ncol=1, scales="free_y") +
  ## ggplot2::theme_bw() +
  ggplot2::theme()
ggplot2::ggsave("images/building_percentile_Wperm2.png", width = 15, height=6)

## quartile
to.plot %>%
  dplyr::group_by(datetime, variable) %>%
  dplyr::mutate(value = kwh.per.footprint.area.m2) %>%
  dplyr::summarise_at(vars(value),
                      tibble::lst(
                                  "25th"=~quantile(., probs=0.25),
                                  "50th"=~quantile(., probs=0.5),
                                  "75th"=~quantile(., probs=0.75)
                                  )) %>%
  dplyr::ungroup() %>%
  tidyr::gather(percentile, value, ends_with("th")) %>%
  ## convert to wh per m2
  dplyr::mutate(value = value * 1000) %>%
  dplyr::filter(variable %in% c("emission.exh", "emission.rej", "emission.overall")) %>%
  dplyr::mutate_at(vars(variable), recode, "emission.overall"="overall heat emission", "emission.exh"="zone exfiltration + exhaust", "emission.rej"="HVAC heat rejection") %>%
  dplyr::mutate(variable = factor(variable,
                                  levels = c("HVAC heat rejection",
                                              "zone exfiltration + exhaust",
                                              "overall heat emission"))) %>%
  dplyr::filter(as.POSIXct("2018-07-03 12:00:00") < datetime, datetime < as.POSIXct("2018-07-16 12:00:00")) %>%
  ggplot2::ggplot(ggplot2::aes(x=datetime, y=value, color=percentile,
                               group=interaction(variable, percentile))) +
  ggplot2::geom_path(size=0.2) +
  ggplot2::ggtitle("Distribution of hourly heat emission at WRF grid level") +
  ggplot2::ylab("Wh/m2 building footprint area") +
  ggplot2::geom_vline(xintercept = as.POSIXct("2018-07-06 00:00:00"), linetype = "dashed") +
  ggplot2::geom_vline(xintercept = as.POSIXct("2018-07-13 00:00:00"), linetype = "dashed") +
  ggplot2::facet_wrap(.~variable, ncol=1) +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave("images/building_quartile_emission_2_component_Wperm2.png", width = 7, height=5)

## has neg result
result.exh.ref <- lapply(files, function(f) {
  tokens = unlist(stringr::str_split(f, pattern = "____"))
  idf.kw = tokens[[1]]
  epw.id = gsub(".csv", "", tokens[[2]])
  print(sprintf("epw: %s, idf: %s", epw.id, idf.kw))
  df = readr::read_csv(sprintf("sim_result_csv/%s", f), col_types = readr::cols()) %>%
    dplyr::mutate(emission.exh = `Environment:Site Total Zone Exfiltration Heat Loss [J](Hourly)`+
                    `Environment:Site Total Zone Exhaust Air Heat Loss [J](Hourly)`,
                  emission.ref = `SimHVAC:Air System Relief Air Total Heat Loss Energy [J](Hourly)`) %>%
    dplyr::mutate(idf.kw = idf.kw, epw.id = epw.id) %>%
    {.}
}) %>%
  dplyr::bind_rows()

result.exh.ref %>%
  dplyr::filter(emission.ref < 0) %>%
  ## dplyr::filter(emission.exh < 0) %>%
  ## dplyr::distinct(epw.id, idf.kw) %>%
  dplyr::distinct(idf.kw) %>%
  {.}

time.week.day = "07/11  14:00:00"
time.week.night = "07/11  21:00:00"
time.weekend.day = "07/07  14:00:00"
time.weekend.night = "07/07  21:00:00"

la.boundary <- sf::st_read("domain/la-county-boundary.geojson")

la.boundary.valid = sf::st_make_valid(la.boundary)

## plot map views

wrf.grid.2 <- sf::st_read("M02_EnergyPlus_Forcing_Historical_LowRes/meta/wrf-grids-origin.geojson")

days = c("07/11  14:00:00", "07/11  21:00:00", "07/07  14:00:00", "07/07  21:00:00")
labels = c("week_day", "week_night", "weekend_day", "weekend_night")

to.plot %>%
    dplyr::filter(`Date/Time` %in% days) %>%
    dplyr::mutate(weekday.label = ifelse(is.weekday, "weekday", "weekend")) %>%
    dplyr::mutate(daytime.label = ifelse(is.day.time, "day", "night")) %>%
    dplyr::filter(variable == "emission.overall") %>%
    dplyr::mutate(`Date/Time` = paste0(`Date/Time`, "\n", weekday.label)) %>%
    dplyr::mutate(wh.per.footprint.area.m2 = kwh.per.footprint.area.m2 * 1000) %>%
    dplyr::inner_join(wrf.grid.2, by=c("epw.id"="id")) %>%
    sf::st_as_sf() %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill=wh.per.footprint.area.m2)) +
    ggplot2::geom_sf(data=la.boundary.valid, colour = "gray",fill=NA) +
    viridis::scale_fill_viridis() +
    ggplot2::facet_wrap(.~`Date/Time`, nrow=1) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave("images/grid_building_heat_emissionOverall.png", width=16, height = 5)

for (day.idx in seq_along(days)) {
  to.plot %>%
    dplyr::filter(`Date/Time` == days[[day.idx]]) %>%
    dplyr::left_join(wrf.grid.2, by=c("epw.id"="id")) %>%
    sf::st_as_sf() %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill=kwh.per.footprint.area.m2)) +
    ggplot2::geom_sf(data=la.boundary, colour = "red",fill=NA) +
    ggplot2::facet_wrap(.~variable, nrow = 2) +
    viridis::scale_fill_viridis() +
    ggplot2::ggtitle(sprintf("%s (%s) building emission normalized by footprint kwh/m2",
                             gsub("_", " ", labels[[day.idx]]), days[[day.idx]])) +
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                  axis.text.x=ggplot2::element_blank(),
                  axis.ticks.x=ggplot2::element_blank(),
                  legend.position = "bottom")
  ggplot2::ggsave(sprintf("images/grid_building_heat_%s.png", labels[[day.idx]]), width=9, height = 7)
}

## verify with previous Xuan's results
## wrf grid phase 1
wrf.grid.1 = sf::read_sf("cmp_phase_1_data/wrf-grids-origin-LA.geojson")
wrf.grid.1.valid = sf::st_make_valid(wrf.grid.1)

centroid.wrf.grid.1 <- sf::st_centroid(wrf.grid.1.valid)

wrf.grid.2 <- sf::st_read("M02_EnergyPlus_Forcing_Historical_LowRes/meta/wrf-grids-origin.geojson")

grid.join <- sf::st_join(centroid.wrf.grid.1, wrf.grid.2, join = sf::st_within, suffix = c(".1", ".2"))

wrf.grid.matching <- grid.join %>%
  dplyr::select(-domain_flag.2, -centroid.2) %>%
  dplyr::filter(!is.na(id.2)) %>%
  dplyr::select(id.1, id.2)

wrf.grid.matching.no.geom <- wrf.grid.matching

sf::st_geometry(wrf.grid.matching.no.geom) <- NULL

result.1.files <- list.files(path="cmp_phase_1_data/LA_2009_WRF_AH1_eplus-results", pattern="*.txt")

df.result.files <- tibble::tibble(filename = result.1.files)

## has the "HEATI_AH1" variable
df.result.files %>%
  dplyr::mutate(variable = gsub("Variable_", "", filename)) %>%
  dplyr::mutate(variable = gsub("_2009", "--2009", variable)) %>%
  tidyr::separate(variable, sep="--", into=c("variable", "suf")) %>%
  dplyr::select(-suf) %>%
  dplyr::distinct(variable) %>%
  print(n=Inf)

allAHfiles <- df.result.files$filename

ah.file.i = allAHfiles[[1]]

df.ah.i <- readr::read_delim(sprintf("cmp_phase_1_data/LA_2009_WRF_AH1_eplus-results/%s", ah.file.i), delim=",", col_names = FALSE) %>%
  dplyr::select(-X340) %>%
  ## gather will be X1 concat with X2, ...
  tidyr::gather(col.id, value, X1:X339) %>%
  {.}

wrf.grid.1 %>%
  tibble::as_tibble() %>%
  {.}

tibble::tibble(X1=1:4, X2=5:8, X3=9:12) %>%
  tidyr::gather(key, val, X1:X3)

wrf.grid.1 %>%
  {.}

## verify with Energy Atlas
## version 1, with no id
## la.nb.geo <- sf::st_read("energyAtlas/Neighborhood/la-county-neighborhoods-v6.geojson")
## version 2, also with no id
## la.nb.geo <- sf::st_read("energyAtlas/Neighborhood/energy_atlas_neighborhoods/energy_atlas_neighborhoods.shp")
la.nb.geo <- sf::st_read("energyAtlas/Neighborhood/neighborhoods/neighborhoods.shp")

la.nb.geo %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(size = 0.2)
ggplot2::ggsave("images/la_neighborhood.png", width = 5, height = 5)

la.nb.no.geom <- la.nb.geo

sf::st_geometry(la.nb.no.geom) <- NULL

la.nb.no.geom %>%
  tibble::as_tibble() %>%
  dplyr::mutate_if(is.character, as.factor) %>%
  summary()

la.nb.geo.4326 <- sf::st_transform(la.nb.geo, crs=4326)
la.nb.geo.4326.valid <- sf::st_make_valid(la.nb.geo.4326)

la.nb.geo.4326.valid %>%
  sf::st_join(la.boundary.valid, join=sf::st_intersects) %>%
  dplyr::filter(!is.na(CITY)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data=la.boundary, colour = "red",fill=NA) +
  ggplot2::theme()
ggplot2::ggsave("images/la-neighborhood.png", width=7, height = 9)

## join building to neighborhood
df.building <- sf::st_read("~/Dropbox/workLBNL/CityBES/LA/data-raw/compiled_LA_building.geojson")

df.building.valid <- sf::st_make_valid(df.building)

df.building.centroid <- sf::st_centroid(df.building.valid)

df.building.nb <- sf::st_join(df.building.centroid, la.nb.geo.4326.valid, join = sf::st_within) %>%
  {.}

df.building.nb.no.geom <- df.building.nb

sf::st_geometry(df.building.nb.no.geom) <- NULL

monthly.total.result <- result %>%
  dplyr::group_by(idf.kw, epw.id) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  dplyr::ungroup() %>%
  dplyr::select(idf.kw, epw.id, energy.overall) %>%
  {.}

annual.total.result <- result.ann %>%
  dplyr::group_by(idf.kw, epw.id) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  dplyr::ungroup() %>%
  dplyr::select(idf.kw, epw.id, energy.overall) %>%
  dplyr::mutate(epw.id = as.numeric(epw.id)) %>%
  {.}

sim.nb.join <- df.building.nb.no.geom %>%
  tibble::as_tibble() %>%
  dplyr::filter(!is.na(neighborho)) %>%
  dplyr::select(-(HEIGHT:count), -(pop2014:pct_rent)) %>%
  dplyr::inner_join(building.info, by="OBJECTID") %>%
  dplyr::mutate(idf.kw = gsub(".idf", "", idf.name, fixed=TRUE)) %>%
  dplyr::mutate(idf.kw = gsub(".", "_", idf.kw, fixed=TRUE)) %>%
  dplyr::inner_join(monthly.total.result, by=c("idf.kw", "epw.id")) %>%
  dplyr::mutate(energy.kwh = energy.overall * 1e-9 * 277.778) %>%
  {.}

sim.ann.nb.join <- df.building.nb.no.geom %>%
  tibble::as_tibble() %>%
  dplyr::filter(!is.na(neighborho)) %>%
  dplyr::select(-(HEIGHT:count), -(pop2014:pct_rent)) %>%
  dplyr::inner_join(building.info, by="OBJECTID") %>%
  dplyr::mutate(idf.kw = gsub(".idf", "", idf.name, fixed=TRUE)) %>%
  dplyr::mutate(idf.kw = gsub(".", "_", idf.kw, fixed=TRUE)) %>%
  dplyr::inner_join(annual.total.result, by=c("idf.kw", "epw.id")) %>%
  dplyr::mutate(energy.kwh = energy.overall * 1e-9 * 277.778) %>%
  {.}

sim.ann.agg.to.nb.use <- sim.ann.nb.join %>%
  dplyr::inner_join(idf.kw.to.usetype, by="idf.kw") %>%
  dplyr::group_by(neighborho, usetype) %>%
  summarise_at(vars(energy.kwh, building.area.m2, FootprintArea.m2), sum) %>%
  dplyr::ungroup() %>%
  {.}

sim.agg.to.nb <- sim.nb.join %>%
  dplyr::group_by(neighborho) %>%
  summarise_at(vars(energy.kwh, building.area.m2, FootprintArea.m2), sum) %>%
  dplyr::ungroup() %>%
  {.}

sim.ann.agg.to.nb <- sim.ann.nb.join %>%
  dplyr::group_by(neighborho) %>%
  summarise_at(vars(energy.kwh, building.area.m2, FootprintArea.m2), sum) %>%
  dplyr::ungroup() %>%
  {.}

sim.agg.to.nb.geo <- sim.agg.to.nb %>%
  dplyr::inner_join(la.nb.geo, by="neighborho") %>%
  sf::st_as_sf() %>%
  {.}

sim.ann.agg.to.nb.geo <- sim.ann.agg.to.nb %>%
  dplyr::inner_join(la.nb.geo, by="neighborho") %>%
  sf::st_as_sf() %>%
  {.}

sim.agg.to.nb.geo %>%
  sf::st_write("geo_output/sim_agg_to_neighborhood.geojson")

sim.ann.agg.to.nb.geo %>%
  sf::st_write("geo_output/sim_ann_agg_to_neighborhood.geojson")

sim.agg.to.nb.geo %>%
  ggplot2::ggplot(ggplot2::aes(fill = energy.kwh)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  ggplot2::ggtitle("Neighborhood total electricity + gas consumption (kWh)") +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
ggplot2::ggsave("images/sim_agg_to_nb_energy_kwh.png", width = 5, height = 7)

sim.agg.to.nb.geo %>%
  ggplot2::ggplot(ggplot2::aes(fill = building.area.m2)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::scale_fill_distiller(palette = "purples", direction = 1) +
  ggplot2::ggtitle("total building area (m2)") +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
ggplot2::ggsave("images/sim_agg_to_nb_m2.png", width = 5, height = 7)

sim.agg.to.nb.geo %>%
  dplyr::mutate(kwh.per.m2 = energy.kwh / building.area.m2) %>%
  ggplot2::ggplot(ggplot2::aes(fill = kwh.per.m2)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::geom_sf(data=wrf.grid.with.building, fill=NA, size=0.1) +
  ggplot2::scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  ggplot2::ggtitle("Building size normalized electricity + gas usage (kWh/m2)") +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
ggplot2::ggsave("images/sim_agg_to_nb_kwh_per_m2_building.png", width = 5, height = 5.5)

sim.agg.to.nb.geo %>%
  dplyr::mutate(kwh.per.m2 = energy.kwh / FootprintArea.m2) %>%
  ggplot2::ggplot(ggplot2::aes(fill = kwh.per.m2)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::geom_sf(data=wrf.grid.with.building, fill=NA, size=0.1) +
  ggplot2::scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  ggplot2::ggtitle("Building footprint normalized electricity + gas usage (kWh/m2)") +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
ggplot2::ggsave("images/sim_agg_to_nb_kwh_per_m2_footprint.png", width = 5.5, height = 5.5)

sim.ann.agg.to.nb.geo %>%
  ggplot2::ggplot(ggplot2::aes(fill = energy.kwh)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  ggplot2::ggtitle("Neighborhood annual electricity + gas consumption (kWh)") +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
ggplot2::ggsave("images/sim_ann_agg_to_nb_energy_kwh.png", width = 5, height = 7)

sim.ann.agg.to.nb.geo %>%
  ggplot2::ggplot(ggplot2::aes(fill = building.area.m2)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::scale_fill_distiller(palette = "Purples", direction = 1) +
  ggplot2::ggtitle("total building area (m2)") +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
ggplot2::ggsave("images/sim_ann_agg_to_nb_m2.png", width = 5, height = 7)

sim.ann.agg.to.nb.geo %>%
  dplyr::mutate(W.per.m2 = energy.kwh * 1000 / building.area.m2) %>%
  ggplot2::ggplot(ggplot2::aes(fill = W.per.m2)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::geom_sf(data=wrf.grid.with.building, fill=NA, size=0.1) +
  ggplot2::scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  ggplot2::ggtitle("Building size normalized annual electricity + gas usage (W/m2)") +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
ggplot2::ggsave("images/sim_ann_agg_to_nb_w_per_m2_building.png", width = 6, height = 7)

sim.ann.agg.to.nb.geo %>%
  dplyr::mutate(W.per.m2 = energy.kwh * 1000 / FootprintArea.m2) %>%
  ggplot2::ggplot(ggplot2::aes(fill = W.per.m2)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  ggplot2::ggtitle("Building footprint normalized annual electricity + gas usage (W/m2)") +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
ggplot2::ggsave("images/sim_ann_agg_to_nb_w_per_m2_footprint.png", width = 6, height = 7)

la.energy <- readr::read_csv("energyAtlas/usage_bld_kwh.csv")
## data from dropbox
la.energy.old <- readr::read_csv("energyAtlas/Neighborhood/usage_bld_2016/usage_bld_kwh.csv")

la.energy.old %>%
  select(sqft, usage) %>%
  dplyr::filter(sqft > 0) %>%
  summary()

la.energy %>%
  ## select(sqft, usage) %>%
  ## summary()
  {.}

## neighborhood with building
nb.with.bd <- df.building.nb.no.geom %>%
  tibble::as_tibble() %>%
  dplyr::filter(!is.na(neighborho)) %>%
  dplyr::select(-(HEIGHT:count), -(pop2014:pct_rent)) %>%
  dplyr::inner_join(building.info, by="OBJECTID") %>%
  dplyr::distinct(neighborho) %>%
  {.}

la.energy %>%
  tidyr::separate(geo_id, c("id.type", "id.num")) %>%
  dplyr::mutate(id.num = as.numeric(id.num)) %>%
  dplyr::filter(id.type == "neighborhoods") %>%
  ## restrict to grids with buildings
  dplyr::inner_join(nb.with.bd, by=c("id.num"="neighborho")) %>%
  dplyr::filter(usage != "masked") %>%
  dplyr::mutate(usage = as.numeric(usage)) %>%
  dplyr::filter(sqft > 0) %>%
  ## ## dplyr::mutate_if(is.character, as.factor) %>%
  ## ## summary() %>%
  dplyr::filter(!(usetype %in% c("agriculture", "other"))) %>%
  ## nrow() %>%
  distinct(id.num) %>%
  {.}

## la.energy.nb.old <- la.energy.old %>%
##   tidyr::separate(id, c("id.type", "id.num")) %>%
##   dplyr::filter(id.type == "neighborhoods") %>%
##   dplyr::mutate(id.num = as.numeric(id.num)) %>%
##   {.}

la.energy.nb <- la.energy %>%
  tidyr::separate(geo_id, c("id.type", "id.num")) %>%
  dplyr::filter(id.type == "neighborhoods") %>%
  dplyr::mutate(id.num = as.numeric(id.num)) %>%
  ## restrict to grids with buildings
  dplyr::inner_join(nb.with.bd, by=c("id.num"="neighborho")) %>%
  {.}

## total.by.sum <- la.energy.nb.old %>%
##   dplyr::filter(usetype != "all") %>%
##   dplyr::group_by(id.num) %>%
##   dplyr::summarise(usage = sum(usage)) %>%
##   dplyr::ungroup() %>%
##   dplyr::mutate(approach = "sum")
## total.from.all <- la.energy.nb.old %>%
##   dplyr::filter(usetype == "all") %>%
##   dplyr::select(id.num, usage) %>%
##   dplyr::mutate(approach = "all") %>%
##   {.}
## total.by.sum %>%
##   dplyr::bind_rows(total.from.all) %>%
##   tidyr::spread(approach, usage) %>%
##   dplyr::mutate(sum.div.all = sum / all) %>%
##   summary()
## total.by.sum <- la.energy.nb.old %>%
##   dplyr::filter(usage > 0, sqft > 0) %>%
##   dplyr::filter(usetype != "all") %>%
##   dplyr::group_by(id.num) %>%
##   dplyr::summarise(usage = sum(usage)) %>%
##   dplyr::ungroup() %>%
##   dplyr::mutate(approach = "sum")
## total.from.all <- la.energy.nb.old %>%
##   dplyr::filter(usage > 0, sqft > 0) %>%
##   dplyr::filter(usetype == "all") %>%
##   dplyr::select(id.num, usage) %>%
##   dplyr::mutate(approach = "all") %>%
##   {.}
## total.by.sum %>%
##   dplyr::bind_rows(total.from.all) %>%
##   tidyr::spread(approach, usage) %>%
##   na.omit() %>%
##   dplyr::mutate(sum.div.all = sum / all) %>%
##   summary()

la.energy.nb.filter <- la.energy.nb %>%
  dplyr::filter(usage != "masked") %>%
  dplyr::mutate(usage = as.numeric(usage)) %>%
  dplyr::filter(sqft > 0) %>%
  dplyr::filter(!(usetype %in% c("agriculture", "other"))) %>%
  {.}

la.energy.nb.total <- la.energy.nb.filter %>%
  dplyr::select(id.num, usetype, sqft, usage) %>%
  dplyr::filter(usetype %in% c("res_total", "commercial", "industrial", "institutional")) %>%
  dplyr::group_by(id.num) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  dplyr::ungroup() %>%
  ## here sqft is total building area
  dplyr::mutate(data.source = "energy atlas 2016",
                m2 = sqft * 0.092903) %>%
  ## dplyr::mutate(data.source = "energy atlas monthly average 2016",
  ##               m2 = sqft * 0.092903) %>%
  dplyr::select(id.num, usage, m2, data.source) %>%
  ## ## annual to monthly average
  ## dplyr::mutate(usage = usage / 12) %>%
  {.}

la.energy.nb.use.total <- la.energy.nb.filter %>%
  dplyr::select(id.num, usetype, sqft, usage) %>%
  dplyr::mutate(data.source = "Energy Atlas 2016",
                m2 = sqft * 0.092903) %>%
  dplyr::select(id.num, usetype, usage, m2, data.source) %>%
  {.}

la.energy.nb.filter %>%
  dplyr::select(-(name:usage_percap)) %>%
  dplyr::distinct(usetype) %>%
  {.}

total.cmp <- sim.agg.to.nb %>%
  dplyr::select(neighborho, energy.kwh, building.area.m2) %>%
  dplyr::rename(id.num=neighborho, usage=energy.kwh, m2=building.area.m2) %>%
  dplyr::mutate(data.source = "Simulation July 2018") %>%
  dplyr::bind_rows(la.energy.nb.total) %>%
  {.}

total.ann.cmp <- sim.ann.agg.to.nb %>%
  dplyr::select(neighborho, energy.kwh, building.area.m2) %>%
  dplyr::rename(id.num=neighborho, usage=energy.kwh, m2=building.area.m2) %>%
  dplyr::mutate(data.source = "Simulation 2018") %>%
  dplyr::bind_rows(la.energy.nb.total) %>%
  {.}

total.ann.nb.use.cmp <- sim.ann.agg.to.nb.use %>%
  dplyr::select(neighborho, usetype, energy.kwh, building.area.m2) %>%
  dplyr::rename(id.num=neighborho, usage=energy.kwh, m2=building.area.m2) %>%
  dplyr::mutate(data.source = "Simulation 2018") %>%
  dplyr::bind_rows(la.energy.nb.use.total) %>%
  {.}

total.cmp %>%
  dplyr::mutate(W.per.m2 = usage * 1000 / m2) %>%
  dplyr::group_by(id.num) %>%
  dplyr::filter(n() == 2) %>%
  dplyr::ungroup() %>%
  dplyr::inner_join(la.nb.geo, by=c("id.num"="neighborho")) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  ggplot2::ggplot(ggplot2::aes(fill = W.per.m2)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  ggplot2::ggtitle("Building size normalized electricity + gas usage (W/m2)") +
  ggplot2::facet_wrap(.~data.source) +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
ggplot2::ggsave("images/cmp_w_per_m2_energy_nb.png", width = 8, height = 6)

to.plot.total.ann.use.cmp <- total.ann.nb.use.cmp %>%
  dplyr::mutate(kwh.per.m2 = usage / m2) %>%
  dplyr::group_by(id.num, usetype) %>%
  dplyr::filter(n() == 2) %>%
  dplyr::ungroup() %>%
  tidyr::gather(variable, value, c(usage, m2, kwh.per.m2)) %>%
  tidyr::spread(data.source, value) %>%
  {.}

to.plot.total.ann.cmp <- total.ann.cmp %>%
  dplyr::mutate(kwh.per.m2 = usage / m2) %>%
  dplyr::group_by(id.num) %>%
  dplyr::filter(n() == 2) %>%
  dplyr::ungroup() %>%
  tidyr::gather(variable, value, c(usage, m2, kwh.per.m2)) %>%
  tidyr::spread(data.source, value) %>%
  {.}

to.plot.total.ann.cmp %>%
  dplyr::filter(variable == "kwh.per.m2") %>%
  ggplot2::ggplot(ggplot2::aes(x=`Simulation 2018`, `Energy Atlas 2016`)) +
  ggplot2::coord_cartesian(xlim=c(0, 2500), ylim=c(0, 2500)) +
  ggplot2::geom_point(size = 0.2) +
  ggplot2::geom_smooth(method="lm", size = 0.5) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggplot2::ggtitle("Electricity + gas usage per total building area \n(kWh/m2)")
  ggplot2::theme()
ggplot2::ggsave("images/scatter_kwh_per_m2_energy_nb.png", width = 5, height = 5)

to.plot.total.ann.cmp %>%
  dplyr::filter(variable == "m2") %>%
  ggplot2::ggplot(ggplot2::aes(x=`Simulation 2018`, `Energy Atlas 2016`)) +
  ggplot2::coord_cartesian(xlim=c(0, 2.1e7), ylim=c(0, 2.1e7)) +
  ggplot2::geom_point(size = 0.2) +
  ggplot2::geom_smooth(method="lm", size = 0.5) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggplot2::ggtitle("Total building area (m2)") +
  ggplot2::theme()
ggplot2::ggsave("images/scatter_m2_energy_nb.png", width = 5, height = 5)

to.plot.total.ann.cmp %>%
  dplyr::filter(variable == "usage") %>%
  ggplot2::ggplot(ggplot2::aes(x=`Simulation 2018`, `Energy Atlas 2016`)) +
  ggplot2::coord_cartesian(xlim=c(0, 3.42e9), ylim=c(0, 3.42e9)) +
  ggplot2::geom_point(size = 0.2) +
  ggplot2::geom_smooth(method="lm", size = 0.5) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggplot2::ggtitle("Electricity + gas usage (kWh)") +
  ggplot2::theme()
ggplot2::ggsave("images/scatter_kwh_energy_nb.png", width = 5, height = 5)

usetypes = c("res_total", "commercial", "institutional", "industrial")
labels = c("residential", "commercial", "institutional", "industrial")

upper.lims = c(NA, 1500, 1800, 3500)
for (i in seq_along(usetypes)) {
  to.plot.total.ann.use.cmp %>%
    dplyr::filter(variable == "kwh.per.m2", usetype == usetypes[[i]]) %>%
    ggplot2::ggplot(ggplot2::aes(x=`Simulation 2018`, `Energy Atlas 2016`)) +
    ggplot2::geom_point(size = 0.2) +
    ggplot2::geom_smooth(method="lm", size = 0.5) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::ggtitle(sprintf("%s electricity + gas usage \nper total building area (kWh/m2)", labels[[i]])) +
    ggplot2::coord_cartesian(xlim = c(0, upper.lims[[i]]), ylim = c(0, upper.lims[[i]])) +
    ggplot2::theme()
  ggplot2::ggsave(sprintf("images/scatter_kwh_per_m2_energy_nb_%s.png", usetypes[[i]]), width = 5, height = 5)
}

upper.lims = c(1.5e7, 8e6, 2.5e5, 5e6)
for (i in seq_along(usetypes)) {
  to.plot.total.ann.use.cmp %>%
    dplyr::filter(variable == "m2", usetype == usetypes[[i]]) %>%
    ggplot2::ggplot(ggplot2::aes(x=`Simulation 2018`, `Energy Atlas 2016`)) +
    ggplot2::geom_point(size = 0.2) +
    ggplot2::geom_smooth(method="lm", size = 0.5) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::ggtitle(sprintf("%s total building area (m2)", labels[[i]])) +
    ggplot2::coord_cartesian(xlim = c(0, upper.lims[[i]]), ylim = c(0, upper.lims[[i]])) +
    ggplot2::theme()
  ggplot2::ggsave(sprintf("images/scatter_m2_energy_nb_%s.png", usetypes[[i]]), width = 5, height = 5)
}

upper.lims = c(1e9, 1.5e9, 1.25e8, 2e9)
for (i in seq_along(usetypes)) {
  to.plot.total.ann.use.cmp %>%
    dplyr::filter(variable == "usage", usetype == usetypes[[i]]) %>%
    ggplot2::ggplot(ggplot2::aes(x=`Simulation 2018`, `Energy Atlas 2016`)) +
    ggplot2::geom_point(size = 0.2) +
    ggplot2::geom_smooth(method="lm", size = 0.5) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::ggtitle(sprintf("%s electricity + gas usage (kWh)", labels[[i]])) +
    ggplot2::coord_cartesian(xlim = c(0, upper.lims[[i]]), ylim = c(0, upper.lims[[i]])) +
    ggplot2::theme()
  ggplot2::ggsave(sprintf("images/scatter_kwh_energy_nb_%s.png", usetypes[[i]]), width = 5, height = 5)
}

total.ann.cmp %>%
  dplyr::mutate(kwh.per.m2 = usage / m2) %>%
  dplyr::group_by(id.num) %>%
  dplyr::filter(n() == 2) %>%
  dplyr::ungroup() %>%
  dplyr::inner_join(la.nb.geo, by=c("id.num"="neighborho")) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  ggplot2::ggplot(ggplot2::aes(fill = kwh.per.m2)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::geom_sf(data=wrf.grid.with.building, fill=NA, size=0.1) +
  ggplot2::scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  ggplot2::ggtitle("Building size normalized annual electricity + gas usage (W/m2)") +
  ggplot2::facet_wrap(.~data.source) +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
ggplot2::ggsave("images/cmp_ann_kwh_per_m2_energy_nb.png", width = 8, height = 5)

total.ann.cmp %>%
  dplyr::mutate(kwh.per.m2 = usage / m2) %>%
  dplyr::group_by(id.num) %>%
  dplyr::filter(n() == 2) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(kwh.per.m2.bin = case_when(kwh.per.m2 < 70 ~ "<70",
                                           kwh.per.m2 < 100 ~ "70-100",
                                           kwh.per.m2 < 150 ~ "70-150",
                                           TRUE ~ ">= 150")) %>%
  dplyr::inner_join(la.nb.geo, by=c("id.num"="neighborho")) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  ggplot2::ggplot(ggplot2::aes(fill = kwh.per.m2.bin)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::geom_sf(data=wrf.grid.with.building, fill=NA, size=0.1) +
  ggplot2::scale_fill_brewer(palette = "YlOrBr", direction = 1) +
  ggplot2::ggtitle("Building size normalized annual electricity + gas usage (kWh/m2)") +
  ggplot2::facet_wrap(.~data.source) +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
ggplot2::ggsave("images/cmp_ann_kwh_per_m2_5_bin_energy_nb.png", width = 8, height = 5)

## density
total.ann.cmp %>%
  dplyr::mutate(kwh.per.m2 = usage / m2) %>%
  dplyr::group_by(id.num) %>%
  dplyr::filter(n() == 2) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(x=kwh.per.m2, fill=data.source)) +
  ggplot2::geom_histogram(alpha=0.5, binwidth=10) +
  ggplot2::theme()

## voilin
total.ann.cmp %>%
  dplyr::mutate(kwh.per.m2 = usage / m2) %>%
  dplyr::group_by(id.num) %>%
  dplyr::filter(n() == 2) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(x=data.source, y=kwh.per.m2)) +
  ggplot2::ggtitle("Building size normalized \nannual electricity + gas usage (kWh/m2)") +
  ggplot2::geom_violin() +
  ggplot2::geom_boxplot(width=0.05, outlier.shape = NA) +
  ggplot2::theme()
ggplot2::ggsave("images/cmp_ann_kwh_per_m2_violin_energy_nb.png", width = 4, height = 5)

total.ann.cmp %>%
  dplyr::group_by(id.num) %>%
  dplyr::filter(n() == 2) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(data.source) %>%
  dplyr::summarise_at(vars(m2), tibble::lst(min, q1=~quantile(., probs=0.25),
                                            median, q3=~quantile(., probs=0.75),
                                            max)) %>%
  dplyr::ungroup() %>%
  {.}

total.ann.cmp %>%
  dplyr::group_by(id.num) %>%
  dplyr::filter(n() == 2) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(m2.bin = case_when(kwh.per.m2 < ~ "<500,000 m2",
                                   kwh.per.m2 < 1e6 ~ "< 1 million m2",
                                   kwh.per.m2 < 1.5e6 ~ "< 1.5 million m2",
                                   kwh.per.m2 < 2e6 ~ "< 2 million m2",
                                   TRUE ~ ">= 2 million m2")) %>%
  ggplot2::scale_fill_distiller(palette = "Purples", direction = 1) +
  ggplot2::ggtitle("Total building area (m2)") +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())

ggplot2::ggsave("images/cmp_ann_kwh_per_m2_violin_energy_nb.png", width = 4, height = 5)

## compare total building sqft
total.ann.cmp %>%
  dplyr::group_by(id.num) %>%
  dplyr::filter(n() == 2) %>%
  dplyr::ungroup() %>%
  dplyr::inner_join(la.nb.geo, by=c("id.num"="neighborho")) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  ggplot2::ggplot(ggplot2::aes(fill = m2)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::scale_fill_distiller(palette = "Purples", direction = 1) +
  ggplot2::ggtitle("Building size (m2)") +
  ggplot2::facet_wrap(.~data.source) +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
ggplot2::ggsave("images/cmp_ann_building_m2_energy_nb.png", width = 8, height = 6)

total.ann.cmp %>%
  dplyr::group_by(id.num) %>%
  dplyr::filter(n() == 2) %>%
  dplyr::ungroup() %>%
  dplyr::select(-usage) %>%
  tidyr::spread(data.source, m2) %>%
  dplyr::mutate(ratio = ifelse(`Simulation 2018` > `Energy Atlas 2016`, `Simulation 2018` / `Energy Atlas 2016`,
  (-1) * `Energy Atlas 2016` / `Simulation 2018`)) %>%
  dplyr::mutate(ratio.abs = abs(ratio)) %>%
  ## remove one outlier ratio
  dplyr::filter(abs(ratio) < 100) %>%
  ## summary()
  dplyr::inner_join(la.nb.geo, by=c("id.num"="neighborho")) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  ggplot2::ggplot(ggplot2::aes(fill = ratio)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::scale_fill_gradient2(low="blue", high="red") +
  ggplot2::ggtitle("Simulation 2018 / Energy Atlas 2016", subtitle="before removing outlier larger than 100") +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())

ggplot2::ggsave("images/ratio_ann_w_per_m2_energy_nb_alldata.png", width = 5, height = 6)

total.cmp %>%
  dplyr::mutate(W.per.m2 = usage * 1000 / m2) %>%
  dplyr::group_by(id.num) %>%
  dplyr::filter(n() == 2) %>%
  dplyr::ungroup() %>%
  dplyr::select(-(usage:m2)) %>%
  tidyr::spread(data.source, W.per.m2) %>%
  dplyr::mutate(ratio = `Simulation July 2018` / `Energy Atlas monthly average 2016`) %>%
  ## summary()
  dplyr::inner_join(la.nb.geo, by=c("id.num"="neighborho")) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  ggplot2::ggplot(ggplot2::aes(fill = ratio)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::scale_fill_gradient2(low="blue", high="red") +
  ggplot2::ggtitle("Simulation July 2018 / Energy Atlas monthly average 2016") +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
ggplot2::ggsave("images/ratio_w_per_m2_energy_nb.png", width = 5, height = 6)

total.ann.cmp %>%
  dplyr::mutate(W.per.m2 = usage * 1000 / m2) %>%
  dplyr::group_by(id.num) %>%
  dplyr::filter(n() == 2) %>%
  dplyr::ungroup() %>%
  dplyr::select(-(usage:m2)) %>%
  tidyr::spread(data.source, W.per.m2) %>%
  dplyr::mutate(ratio = ifelse(`Simulation 2018` > `Energy Atlas 2016`, `Simulation 2018` / `Energy Atlas 2016`,
                               (-1) * `Energy Atlas 2016` / `Simulation 2018`)) %>%
  dplyr::mutate(ratio.abs = abs(ratio)) %>%
  ## remove one outlier ratio
  dplyr::filter(abs(ratio) < 100) %>%
  ## summary()
  dplyr::inner_join(la.nb.geo, by=c("id.num"="neighborho")) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  ggplot2::ggplot(ggplot2::aes(fill = ratio)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::scale_fill_gradient2(low="blue", high="red") +
  ggplot2::ggtitle("Simulation 2018 / Energy Atlas 2016", subtitle="before removing outlier of -838") +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
ggplot2::ggsave("images/ratio_ann_w_per_m2_energy_nb_alldata.png", width = 5, height = 6)
## ggplot2::ggsave("images/ratio_ann_w_per_m2_energy_nb.png", width = 5, height = 6)

  dplyr::inner_join(la.nb.geo, by=c("id.num"="neighborho")) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  ggplot2::ggplot(ggplot2::aes(fill = W.per.m2)) +
  ggplot2::geom_sf(size = 0.2) +
  ggplot2::scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  ggplot2::ggtitle("Building footprint normalized electricity + gas usage (W/m2)") +
  ggplot2::facet_wrap(.~data.source) +
  ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
ggplot2::ggsave("images/cmp_w_per_m2_energy_nb.png", width = 8, height = 6)


readr::write_csv("usage_bld_kwh_one_name_many_id.csv")

nb.energy.geo.valid <- nb.energy.geo %>%
  sf::st_as_sf() %>%
  sf::st_make_valid() %>%
  {.}

wrf.grid.centroid <- sf::st_centroid(wrf.grid.2)

wrf.grid.with.building <- to.plot %>%
  distinct(epw.id) %>%
  dplyr::inner_join(wrf.grid.2, by=c("epw.id"="id")) %>%
  sf::st_as_sf() %>%
  {.}

my.breaks = 10**(1:6)

nb.energy.geo.valid.no.geom <- nb.energy.geo.valid 

sf::st_geometry(nb.energy.geo.valid.no.geom ) <- NULL

nb.energy.geo.valid.no.geom %>%
  distinct(usetype) %>%
  {.}

types = c("all", "res", "single_family", "multi_family", "commercial", "industrial")

for (type.i in types) {
  nb.energy.geo.valid %>%
    dplyr::mutate(w.per.m2 = usage * 1000 / (sqft * 0.092903)) %>%
    dplyr::filter(usetype == type.i) %>%
    sf::st_join(wrf.grid.with.building, join = sf::st_covered_by) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = w.per.m2), size = 0.1) +
    ## ggplot2::facet_wrap(.~usetype, scales = "free_y") +
    ggplot2::geom_sf(data=wrf.grid.with.building, fill=NA) +
    ## ggplot2::scale_fill_continuous(breaks = my.breaks, labels = my.breaks) +
    ggplot2::ggtitle(gsub("_", " ", type.i)) +
    ggplot2::theme()
  ggplot2::ggsave(sprintf("images/nb_energy_%s.png", type.i), width=9, height = 9)
}


la.energy.name <- la.energy %>%
  dplyr::select(name, sqft, usage, usetype) %>%
  dplyr::filter(usage > 0, sqft > 0) %>%
  distinct(name) %>%
  {.}

la.energy.name %>%
  readr::write_csv("energyAtlas/Neighborhood/name_from_kwh.csv")

la.nb.geo.name.no.geom <- la.nb.geo.name.no.geom %>%
  tibble::as_tibble() %>%
  {.}

match.to.geo <- la.energy.name %>%
  dplyr::left_join(la.nb.geo.name.no.geom %>% mutate(found = 1), by="name") %>%
  dplyr::filter(!is.na(found)) %>%
  {.}

match.to.geo %>%
  dplyr::inner_join()

la.energy %>%
  dplyr::select(name, sqft, usage, usetype) %>%
  dplyr::filter(usage > 0, sqft > 0) %>%
  dplyr::mutate(kwh.per.m2 = usage / (sqft * 0.0929)) %>%
  {.}

  dplyr::distinct(name) %>%
  dplyr::

la.tract.geo <- sf::st_read("energyAtlas/Census Tract/la-county-census-tracts.geojson")

la.tract.geo.valid <- sf::st_make_valid(la.tract.geo)
sf::st_is_valid(la.tract.geo.valid)
la.boundary.valid <- sf::st_make_valid(la.boundary)
sf::st_is_valid(la.boundary.valid)

la.tract.geo.valid %>%
  sf::st_join(la.boundary.valid, join=sf::st_intersects) %>%
  dplyr::filter(!is.na(CITY)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(size=0.1) +
  ggplot2::geom_sf(data=la.boundary, colour = "red",fill=NA) +
  ggplot2::theme()
ggplot2::ggsave("images/la-tract.png", width=7, height = 5)
