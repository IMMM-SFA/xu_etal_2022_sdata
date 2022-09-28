library("dplyr")

sf::sf_use_s2(FALSE)

if (!file.exists("intermediate_data/compiled_LA_building_centroid.geojson")) {
    df.building <- sf::st_read("intermediate_data/compiled_LA_building.geojson")
    df.building.centroid <- sf::st_centroid(df.building)
    df.building.centroid %>%
        sf::st_write("intermediate_data/compiled_LA_building_centroid.geojson")
} else {
    df.building.centroid <- sf::st_read("intermediate_data/compiled_LA_building_centroid.geojson")
}

tract.ca <- sf::st_read("input_data/domain/tl_2018_06_tract/tl_2018_06_tract.shp") %>%
    dplyr::select(GEOID, NAME) %>%
    {.}
tract.ca.4326 <- sf::st_transform(tract.ca, crs=4326)

if (!file.exists("intermediate_data/building_id_to_census_tract.csv")) {
    tract.ca.4326 <- sf::st_transform(tract.ca, crs=4326)
    df.building.tract <- sf::st_join(df.building.centroid, tract.ca.4326, join = sf::st_within)
    df.building.tract.no.geom <- df.building.tract
    sf::st_geometry(df.building.tract.no.geom) <- NULL
    df.building.to.tract <- df.building.tract.no.geom %>%
        tibble::as_tibble() %>%
        dplyr::filter(!is.na(GEOID)) %>%
        dplyr::distinct(OBJECTID, GEOID) %>%
        dplyr::rename(id.tract = GEOID) %>%
        {.}
    df.building.to.tract %>%
        readr::write_csv("intermediate_data/building_id_to_census_tract.csv")
} else {
    df.building.to.tract <-
        readr::read_csv("intermediate_data/building_id_to_census_tract.csv")
}

grid.finer <- sf::st_read("input_data/high res grid for reporting/wrf-grids-origin.geojson")

if (!file.exists("intermediate_data/building_id_to_finer_grid.csv")) {
    df.building.finer.grid <- sf::st_join(df.building.centroid, grid.finer, join = sf::st_within)
    df.building.finer.grid.nogeom <- df.building.finer.grid
    sf::st_geometry(df.building.finer.grid.nogeom) <- NULL
    df.building.to.finer.grid <- df.building.finer.grid.nogeom %>%
        tibble::as_tibble() %>%
        dplyr::filter(!is.na(id)) %>%
        dplyr::distinct(OBJECTID, id) %>%
        dplyr::rename(id.grid.finer = id) %>%
        {.}
    df.building.to.finer.grid %>%
        readr::write_csv("intermediate_data/building_id_to_finer_grid.csv")
} else {
    df.building.to.finer.grid <-
        readr::read_csv("intermediate_data/building_id_to_finer_grid.csv")
}

df.type.vintage.to.idf <- readr::read_csv("input_data/type_vintage_to_idf_mapping.csv")

df.type.recode <- readr::read_csv("input_data/building_type_recode.csv") %>%
    dplyr::rename(usetype = `remap Energy Atlas`) %>%
    dplyr::select(-starts_with("remap"), -count)

df.building.footprint.m2 <- readr::read_csv("intermediate_data/LA_building_footprint_m2.csv")

df.compiled.building <- readr::read_csv("intermediate_data/compiled_cell_building.csv")

df.compiled.building <- df.compiled.building %>%
    dplyr::select(OBJECTID, id, num.floor:vintage) %>%
    dplyr::rename(id.grid.coarse = id) %>%
    {.}

df.building.centroid.nogeom <- df.building.centroid

sf::st_geometry(df.building.centroid.nogeom) <- NULL

## check builidngs unmatched to finer grid
df.compiled.building %>%
    dplyr::select(OBJECTID) %>%
    dplyr::left_join(df.building.to.finer.grid, by="OBJECTID") %>%
    dplyr::filter(is.na(id.grid.finer)) %>%
    dplyr::left_join(df.building.centroid) %>%
    sf::st_write("building_unmatched_to_finer.geojson")

df.building.meta <- df.building.centroid.nogeom %>%
    tibble::as_tibble() %>%
    ## remove the ones needing height for sub-classification
    dplyr::inner_join(df.compiled.building, by="OBJECTID") %>%
    dplyr::inner_join(df.type.vintage.to.idf, by=c("building.type", "vintage")) %>%
    dplyr::mutate(idf.kw = gsub(".idf", "", idf.name, fixed=TRUE)) %>%
    dplyr::mutate(idf.kw = gsub(".", "_", idf.kw, fixed=TRUE)) %>%
    dplyr::inner_join(df.type.recode, by=c("GeneralUseType", "SpecificUseType")) %>%
    dplyr::inner_join(df.building.footprint.m2, by="OBJECTID") %>%
    dplyr::mutate(building.area.m2 = SQFTmain * 0.0929) %>%
    dplyr::left_join(df.building.to.tract, by="OBJECTID") %>%
    ## some buildings are not matched to a finer grid as the finer grids do not cover the whole range of coarse grid
    dplyr::left_join(df.building.to.finer.grid, by="OBJECTID") %>%
    dplyr::select(OBJECTID, GeneralUseType, SpecificUseType, EffectiveYearBuilt, building.type, vintage, idf.name, idf.kw, usetype, FootprintArea.m2, building.area.m2, id.grid.coarse, id.grid.finer, id.tract) %>%
    {.}

df.building.centroid %>%
    dplyr::select(OBJECTID) %>%
    dplyr::inner_join(df.building.meta, by="OBJECTID") %>%
    sf::st_write("output_data/building_metadata.geojson")

df.building.meta %>%
    readr::write_csv("output_data/building_metadata.csv")

df.building.meta.no.geom <- df.building.meta %>%
    tibble::as_tibble() %>%
    {.}

grid.coarse <- sf::st_read("input_data/M02_EnergyPlus_Forcing_Historical_LowRes/meta/wrf-grids-origin.geojson")

tract.ca <- tract.ca %>%
    dplyr::rename(id = GEOID)

get.grid.size.from.geometry <- function(grid.geo) {
    grid.geo$area.m2 <- sf::st_area(grid.geo)
    grid.no.geo <- grid.geo
    sf::st_geometry(grid.no.geo) <- NULL
    grid.no.geo <- grid.no.geo %>%
        tibble::as_tibble() %>%
        dplyr::mutate(area.m2 = as.numeric(area.m2)) %>%
        dplyr::select(id, area.m2)
    grid.no.geo
}

write.geo.data.with.area <- function(grid.geo, id.colname) {
    df.area <- get.grid.size.from.geometry(grid.geo)
    df.building.meta.no.geom %>%
        dplyr::group_by_at(id.colname) %>%
        dplyr::summarise_at(vars(FootprintArea.m2, building.area.m2), sum) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(df.area, by=c(rlang::chr(!!id.colname := "id"))) %>%
        dplyr::left_join(grid.geo %>% dplyr::select(id),
                         by=c(rlang::chr(!!id.colname := "id"))) %>%
        sf::st_as_sf() %>%
        {.}
}

write.geo.data.with.area(grid.coarse, "id.grid.coarse") %>%
    sf::st_write("output_data/geo_data/coarse_grid.geojson")

write.geo.data.with.area(grid.finer, "id.grid.finer") %>%
    sf::st_write("output_data/geo_data/finer_grid.geojson")

write.geo.data.with.area(tract.ca.4326, "id.tract") %>%
    sf::st_write("output_data/geo_data/tract.geojson")

## continue eval aggregate_to_grid
