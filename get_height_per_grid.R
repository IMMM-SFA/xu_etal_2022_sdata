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

df.building %>%
    dplyr::group_by(id.grid.finer) %>%
    dplyr::summarise(HEIGHT = mean(HEIGHT)) %>%
    dplyr::ungroup() %>%
    readr::write_csv("intermediate_data/building_avg_height_finer_grid.csv")
