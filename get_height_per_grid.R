## missed height in building metadata in the previous version. This script adds
## it back. HEIGHT is in ft
library("dplyr")

df.building <- readr::read_csv("output_data/building_metadata.csv")

df.footprint <- sf::st_read("LARIAC6_LA_County.geojson") %>%
    {.}

df.height <- df.footprint %>%
    dplyr::select(OBJECTID, HEIGHT) %>%
    {.}

sf::st_geometry(df.height) <- NULL

df.height <- df.height %>%
    tibble::as_tibble()

df.building.geo <- sf::st_read("output_data/building_metadata.geojson")

df.building.geo %>%
    dplyr::left_join(df.height, by = "OBJECTID") %>%
    sf::st_write("output_data/building_metadata.geojson")

df.building.nogeom <- df.building.geo

sf::st_geometry(df.building.nogeom) <- NULL

df.building.nogeom %>%
    tibble::as_tibble() %>%
    dplyr::left_join(df.height, by = "OBJECTID") %>%
    readr::write_csv("output_data/building_metadata.csv")

df.building <- readr::read_csv("output_data/building_metadata.csv")

height.stats <- df.building %>%
    dplyr::select(id.grid.finer, HEIGHT) %>%
    na.omit() %>%
    dplyr::group_by(id.grid.finer) %>%
    dplyr::summarise_at(vars(HEIGHT), tibble::lst(min, median, mean, max, sd)) %>%
    dplyr::ungroup() %>%
    {.}

count.stats <- df.building %>%
    dplyr::select(id.grid.finer, HEIGHT) %>%
    dplyr::mutate(with.height = ifelse(is.na(HEIGHT), 0, 1)) %>%
    dplyr::group_by(id.grid.finer) %>%
    dplyr::summarise(n.building = n(), n.with.height = sum(with.height)) %>%
    dplyr::ungroup() %>%
    {.}

height.stats %>%
    dplyr::inner_join(count.stats, by = "id.grid.finer") %>%
    readr::write_csv("intermediate_data/building_height_stats_finer_grid.csv")
