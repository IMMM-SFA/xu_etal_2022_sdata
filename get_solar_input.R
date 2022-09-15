library("dplyr")

## for july
## climate.data.path = "input_data/M02_EnergyPlus_Forcing_Historical_LowRes"
## for annual 2018
## climate.data.path = "input_data/annual_WRF/M02_EnergyPlus_Forcing_Historical_LowRes_ann_2018"
## for annual 2016
climate.data.path = "input_data/annual_WRF/M02_EnergyPlus_Forcing_Historical_LowRes_ann_2016"

location = "LA"

if (location %in% c("SF", "LA")) {
    tz.string = "Etc/GMT+8"
} else if (location == "chicago") {
    tz.string = "Etc/GMT+6"
}

filenames <- list.files(path=sprintf("%s/SWDOWN/", climate.data.path), pattern = "*.txt")

timestamps <- substr(filenames, 17, 26)

utc.time <- as.POSIXct(timestamps, tz="GMT", format="%Y%m%d%H")

local.time <- lubridate::with_tz(utc.time, tz=tz.string)

head(utc.time)
head(local.time)

target.centroid = "65"

solar.data <- readr::read_csv(sprintf("%s/time_series/SWDOWN.csv", climate.data.path)) %>%
  dplyr::select(target.centroid) %>%
  dplyr::mutate(time.local = local.time) %>%
  dplyr::mutate(mon = as.numeric(format(time.local, format="%m"))) %>%
  dplyr::mutate(day = as.numeric(format(time.local, format="%d"))) %>%
  dplyr::mutate(hour = as.numeric(format(time.local, format="%H"))) %>%
  dplyr::select(time.local:hour, everything()) %>%
  dplyr::rename(solar.rad:=!!rlang::sym(target.centroid)) %>%
  {.}

solar.data %>%
  dplyr::select(-time.local) %>%
  readr::write_csv(sprintf("%s/time_series/LA-SWDOWN_input_to_excel.csv",
                           climate.data.path))

utc.time %>% tail()

solar.data
