library("dplyr")
library("readr")

## for july data
## climate.folder = "input_data/M02_EnergyPlus_Forcing_Historical_LowRes"
## for 2018 annual
climate.folder = "input_data/annual_WRF/M02_EnergyPlus_Forcing_Historical_LowRes_ann_2018"

col.names = c("year","month","day","hour","minute",
              "Datasource","DryBulb.C","DewPoint.C",
              "RelHum.percent","AtmosPressure.Pa","ExtHorzRad.Wh/m2",
              "ExtDirRad.Wh/m2","HorzIRSky.Wh/m2","GloHorzRad.Wh/m2",
              "DirNormRad.Wh/m2","DifHorzRad.Wh/m2","GloHorzIllum.lux",
              "DirNormIllum.lux","DifHorzIllum.lux", "ZenLum.Cd/m2",
              "WindDir.deg","WindSpd.m/s","TotSkyCvr.0.1","OpaqSkyCvr.0.1",
              "Visibility.km","CeilingHgt.m","PresWeathObs","PresWeathCodes",
              "PrecipWtr.mm","AerosolOptDepth.0.001","SnowDepth.cm",
              "DaysLastSnow","Albedo.0.01","Rain.mm","RainQuantity.hr")

files = list.files(path=sprintf("%s/wrf_epw/", climate.folder), pattern = "*.epw")

head(files)

result <- lapply(files, function(f) {
  print(f)
  ## skip 8 non-data rows
  df = readr::read_csv(sprintf("%s/wrf_epw/%s", climate.folder, f), skip=8,
                       col_names=col.names,
                       col_types=readr::cols(year=col_integer(),
                                             month=col_integer(),
                                             day=col_integer(),
                                             hour=col_integer(),
                                             minute=col_integer())) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(filename = f) %>%
    {.}
})

df.all.weather <- result %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(cell.id = as.numeric(gsub(".epw", "", filename))) %>%
  {.}

df.all.weather %>%
    {.}

## building.info = readr::read_csv("building_info.csv")

## df.cell.with.building <- building.info %>%
df.cell.with.building <- readr::read_csv("intermediate_data/summary/building_type_vintage_count_per_grid.csv") %>%
  dplyr::distinct(id)

df.weather.with.cell <- df.all.weather %>%
  dplyr::inner_join(df.cell.with.building, by=c("cell.id"="id"))

df.weather.with.cell %>%
    dplyr::select(month:hour, cell.id, DryBulb.C, RelHum.percent, `WindSpd.m/s`) %>%
    readr::write_csv("intermediate_data/weather_2018.csv")

df.weather.with.cell %>%
  dplyr::mutate(cell.id = factor(cell.id)) %>%
  dplyr::filter((month == 6 & day > 25) | (month == 7) | (month == 8 & day < 7)) %>%
  dplyr::select(cell.id, year:minute, DryBulb.C, RelHum.percent, `WindSpd.m/s`
                ) %>%
  dplyr::mutate(timestamp = sprintf("2018-%02d-%02d %02d:%02d:00", month, day, hour, minute)) %>%
  dplyr::mutate(timestamp.pox = as.POSIXct(timestamp)) %>%
  tidyr::gather(variable, value, DryBulb.C:`WindSpd.m/s`) %>%
  dplyr::mutate_at(vars(variable), recode, "DryBulb.C"="Dry Bulb Temperature (C)",
                   "RelHum.percent" = "Relative Humidity (%)",
                    "WindSpd.m/s"="Wind Speed (m/s)") %>%
  ggplot2::ggplot(ggplot2::aes(x=timestamp.pox, y=value, color=cell.id, group=cell.id)) +
  ggplot2::geom_line(size = 0.2) +
  ggplot2::facet_wrap(.~variable, ncol=1, scales = "free_y") +
  ggplot2::geom_vline(xintercept = as.POSIXct("2018-07-06 00:00:00"), linetype = "dashed") +
  ggplot2::xlab("Time") +
  ggplot2::ylab("") +
  ggplot2::theme()
ggplot2::ggsave("figures/compiled_epw_weather.png", width = 14, height = 7)

df.weather.with.cell %>%
    readr::write_csv("wrf_cell_weather_with_bldg.csv")

df.weather.with.cell %>%
    dplyr::mutate(cell.id = factor(cell.id)) %>%
    dplyr::filter((month == 6 & day > 25) | (month == 7) | (month == 8 & day < 7)) %>%
    dplyr::select(cell.id, year:minute, DryBulb.C, RelHum.percent, `WindSpd.m/s`) %>%
    tidyr::gather(variable, value, DryBulb.C:`WindSpd.m/s`) %>%
    dplyr::group_by(month, day, hour, variable) %>%
    dplyr::summarise(value = mean(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(timestamp = sprintf("2018-%02d-%02d %02d:00:00", month, day, hour)) %>%
    dplyr::mutate(timestamp.pox = as.POSIXct(timestamp)) %>%
    dplyr::mutate_at(vars(variable), recode, "DryBulb.C"="Dry Bulb Temperature (C)",
                     "RelHum.percent" = "Relative Humidity (%)",
                     "WindSpd.m/s"="Wind Speed (m/s)") %>%
    ggplot2::ggplot(ggplot2::aes(x=timestamp.pox, y=value)) +
    ggplot2::geom_line(size = 0.2) +
    ggplot2::facet_wrap(.~variable, ncol=1, scales = "free_y") +
    ggplot2::geom_vline(xintercept = as.POSIXct("2018-07-06 00:00:00"), linetype = "dashed") +
    ggplot2::theme()
ggplot2::ggsave("figures/compiled_epw_weather_agg_over_grid.png", width = 14, height = 7)
