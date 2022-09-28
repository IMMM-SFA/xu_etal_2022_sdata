library("dplyr")

## use finer resolution to compare against paper (Zheng & Weng, 2018), as theirs is 120m res
grid.finer <- sf::st_read("output_data/geo_data/finer_grid.geojson") %>%
    tibble::as_tibble() %>%
    dplyr::select(id.grid.finer, area.m2)

df.month.hour.avg.per.grid <- readr::read_csv("intermediate_data/diurnal/annual_2018_finer_hourly_avg_month.csv")

df.month.hour.avg.wperm2 <- df.month.hour.avg.per.grid %>%
    dplyr::select(month, Hour, geoid, emission.overall) %>%
    dplyr::left_join(grid.finer, by=c("geoid"="id.grid.finer")) %>%
    dplyr::mutate(emission.overall = emission.overall * 0.000277778 / area.m2) %>%
    dplyr::group_by(month, Hour) %>%
    dplyr::summarise_if(is.numeric, mean) %>%
    dplyr::ungroup() %>%
    {.}

days = tibble::tibble(month = 1:12,
                      days.in.month = lubridate::days_in_month(as.POSIXct(sprintf("2018-%02d-01", 1:12))))

df.season.avg <- df.month.hour.avg.wperm2 %>%
    dplyr::mutate(month = as.numeric(month)) %>%
    dplyr::mutate(season = case_when(month %in% c(12, 1, 2) ~ "winter",
                                     month %in% c(3, 4, 5) ~ "spring",
                                     month %in% c(6, 7, 8) ~ "summer",
                                     month %in% c(9, 10, 11) ~ "fall")) %>%
    dplyr::left_join(days, by="month") %>%
    dplyr::select(-geoid, -area.m2) %>%
    dplyr::group_by(season, Hour) %>%
    dplyr::summarise_if(is.numeric, ~weighted.mean(., w = days.in.month)) %>%
    dplyr::ungroup() %>%
    {.}

df.season.avg %>%
    ggplot2::ggplot(ggplot2::aes(x = Hour, y = emission.overall, color = season)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::ylab("Heat emission (W/m2)") +
    ggplot2::ggtitle("Average hourly total heat emission (W/m2) across finer grids") +
    ggplot2::theme()
ggplot2::ggsave("figures/diurnal_by_season_finer.png", width = 8, height = 6)

## check one time snapshot
## date = "08_15"
## filepath = "intermediate_data/hourly_heat_energy/annual_2018_finer"
## files = list.files(filepath,
##                    sprintf("%s  *", date))
## one.snapshot = lapply(files, function(f) {
##     readr::read_csv(sprintf("%s/%s", filepath, f)) %>%
##         dplyr::select(geoid, timestamp, emission.overall) %>%
##         dplyr::left_join(grid.finer, by=c("geoid"="id.grid.finer")) %>%
##         dplyr::mutate(emission.overall = emission.overall * 0.000277778 / area.m2) %>%
##         {.}
## }) %>%
##     dplyr::bind_rows() %>%
##     dplyr::select(-area.m2, -geoid) %>%
##     dplyr::group_by(timestamp) %>%
##     dplyr::summarise_if(is.numeric, mean) %>%
##     dplyr::ungroup() %>%
##     {.}

## city total
building.metadata <- sf::st_read("output_data/building_metadata.geojson")

## city boundary shape
## downloaded from: https://geohub.lacity.org/datasets/city-boundary/explore?location=34.019779%2C-118.412043%2C10.90
city.boundary <- 
