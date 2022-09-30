library("dplyr")

## use finer resolution to compare against paper (Zheng & Weng, 2018), as theirs is 120m res
grid.finer <- sf::st_read("output_data/geo_data/finer_grid.geojson")

## city boundary shape
## downloaded from: https://geohub.lacity.org/datasets/city-boundary/explore?location=34.019779%2C-118.412043%2C10.90
city.boundary <- sf::st_read("input_data/domain/City_Boundary.geojson") %>%
    dplyr::select(-OBJECTID)

grid.centroid <- sf::st_centroid(grid.finer)

grid.in.city <- sf::st_join(grid.centroid, city.boundary, join = sf::st_within) %>%
    dplyr::filter(!is.na(CITY)) %>%
    tibble::as_tibble() %>%
    dplyr::select(id.grid.finer, area.m2, FootprintArea.m2)

grid.finer <- grid.finer %>%
    tibble::as_tibble() %>%
    dplyr::select(id.grid.finer, area.m2, FootprintArea.m2)

df.month.hour.avg.per.grid <- readr::read_csv("intermediate_data/diurnal/annual_2018_finer_hourly_avg_month.csv")

df.month.hour.avg.wperm2 <- df.month.hour.avg.per.grid %>%
    dplyr::select(month, Hour, geoid, emission.overall) %>%
    dplyr::left_join(grid.finer, by=c("geoid"="id.grid.finer")) %>%
    dplyr::mutate(emission.overall = emission.overall * 0.000277778 / area.m2) %>%
    dplyr::select(-geoid, -area.m2, -FootprintArea.m2) %>%
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
ggplot2::ggsave("figures/diurnal_by_season_finer.png", width = 8, height = 4)

## normalize by building footprint, restrict to LA city
df.month.hour.avg.wperm2.by.footprint <- df.month.hour.avg.per.grid %>%
    dplyr::select(month, Hour, geoid, starts_with("emission")) %>%
    dplyr::inner_join(grid.in.city, by=c("geoid"="id.grid.finer")) %>%
    dplyr::select(-area.m2) %>%
    tidyr::gather(variable, value, emission.exfiltration:emission.surf) %>%
    dplyr::mutate(value = value * 0.000277778 / FootprintArea.m2) %>%
    tidyr::spread(variable, value) %>%
    dplyr::group_by(month, Hour) %>%
    dplyr::summarise_if(is.numeric, mean) %>%
    dplyr::ungroup() %>%
    {.}

## comparing with Xuan's paper
df.month.hour.avg.wperm2.by.footprint %>%
    dplyr::filter(month == "09") %>%
    dplyr::mutate(emission.no.surf = emission.exfiltration + emission.exhaust + emission.ref + emission.rej) %>%
    ## dplyr::select(month, Hour, emission.no.surf) %>%
    ggplot2::ggplot(ggplot2::aes(x = Hour, y = emission.no.surf)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::expand_limits(y = 0) +
    ggplot2::ggtitle("September diurnal profile for LA city (no surface component)") +
    ggplot2::ylab("Heat emission (W/m2)") +
    ggplot2::theme()
ggplot2::ggsave("figures/diurnal_sep_city_div_footprint.png", width = 8, height = 4)

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

building.city.join <- sf::st_join(building.metadata, city.boundary, join=sf::st_within)

building.in.city <- building.city.join %>%
    dplyr::filter(!is.na(CITY)) %>%
    {.}

## 577802 buildings in the city
building.in.city %>%
    nrow()

annual.sim.hourly.idf.epw.2018 <- readr::read_csv("intermediate_data/annual_sim_result_by_idf_epw_2018.csv")

prototype.area <- readr::read_csv("input_data/prototype_bldg_area.csv") %>%
    dplyr::mutate(idf.kw = gsub(".idf", "", idf.name, fixed=TRUE)) %>%
    dplyr::mutate(idf.kw = gsub(".", "_", idf.kw, fixed=TRUE)) %>%
    {.}

idf.epw.area <- building.in.city %>%
    tibble::as_tibble() %>%
    dplyr::rename(epw.id = id.grid.coarse) %>%
    dplyr::group_by(idf.kw, epw.id) %>%
    dplyr::summarise(building.area.m2 = sum(building.area.m2)) %>%
    dplyr::ungroup() %>%
    {.}

city.hourly.result <- idf.epw.area %>%
    dplyr::inner_join(annual.sim.hourly.idf.epw.2018, by=c("idf.kw", "epw.id")) %>%
    dplyr::inner_join(prototype.area, by="idf.kw") %>%
    tidyr::gather(variable, value, emission.exfiltration:energy.gas) %>%
    dplyr::mutate(value = value / prototype.m2 * building.area.m2) %>%
    tidyr::spread(variable, value) %>%
    {.}

city.hourly.result %>%
    dplyr::select(idf.kw, epw.id, building.area.m2, `Date/Time`, starts_with("emission"), starts_with("energy")) %>%
    readr::write_csv("intermediate_data/city_hourly_idf_epw_result.csv")

city.hourly.result <- readr::read_csv("intermediate_data/city_hourly_idf_epw_result.csv")

city.area = as.numeric(sf::st_area(city.boundary))

city.hourly.heat.wperm2 <- city.hourly.result %>%
    dplyr::select(`Date/Time`, starts_with("emission")) %>%
    dplyr::group_by(`Date/Time`) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, function(x) x * 0.000277778 / city.area) %>%
    {.}

## note season here is different from above
city.diurnal.by.season <- city.hourly.heat.wperm2 %>%
    dplyr::mutate(month = as.numeric(substr(`Date/Time`, 1, 2))) %>%
    dplyr::mutate(hour = as.numeric(substr(`Date/Time`, 8, 9))) %>%
    dplyr::mutate(season = case_when(month %in% c(12, 1, 2) ~ "winter",
                                     month %in% c(7, 8, 9) ~ "summer",
                                     TRUE ~ "transition")) %>%
    dplyr::select(-month) %>%
    dplyr::group_by(season, hour) %>%
    dplyr::summarise_if(is.numeric, mean) %>%
    dplyr::ungroup() %>%
    {.}

city.diurnal.by.season %>%
    ggplot2::ggplot(ggplot2::aes(x = hour, y = emission.overall, color = season)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::ylab("Heat emission (W/m2)") +
    ggplot2::ggtitle("Hourly total heat emission (W/m2) across the city") +
    ggplot2::expand_limits(y = 77) +
    ggplot2::theme(plot.margin = ggplot2::margin(0.5,0.5,1.5,0.5, "cm"))
ggplot2::ggsave("figures/diurnal_by_season_city.png", width = 8, height = 4)

city.boundary %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf()
