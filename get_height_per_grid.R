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


mat <- tibble::tibble(wrf.grid.id = all.wrf.ids) %>%
    dplyr::left_join(df.i, by=c("wrf.grid.id" = "id")) %>%
    dplyr::select(wrf.grid.id, value) %>%
    tidyr::replace_na(list(value="NaN")) %>%
    .$value %>%
    matrix(nrow=num.rows, byrow=TRUE) %>%
    {.}

get.all.wrf.ids <- function(grid.level) {
    if (grid.level == "coarse") {
        num.rows = 14
        num.cols = 14
        all.wrf.ids = 0:(num.rows * num.cols - 1)
    } else if (grid.level == "finer"){
        num.rows = 384
        num.cols = 339
        all.wrf.ids = 0:(num.rows * num.cols - 1)
    }
    all.wrf.ids
}

## rename the column to export in df to "value" first
## rename grid id column to "id.grid"
df.to.matrix <- function(grid.level, df) {
    all.wrf.ids <- get.all.wrf.ids(grid.level)
    mat <- tibble::tibble(wrf.grid.id = all.wrf.ids) %>%
        dplyr::left_join(df,
                         by=c("wrf.grid.id" = "id.grid")) %>%
        dplyr::select(wrf.grid.id, value) %>%
        tidyr::replace_na(list(value="NaN")) %>%
        .$value %>%
        matrix(nrow=num.rows, byrow=TRUE) %>%
        {.}
    mat
}

df.to.matrix("finer", height.stats %>%
                             dplyr::rename(value = mean,
                                           id.grid = id.grid.finer)) %>%
    write.table("intermediate_data/mean_height.txt", row.names = FALSE,
                col.names = FALSE, sep=",", quote=FALSE)

df.to.matrix("finer", height.stats %>%
                      dplyr::rename(value = sd,
                                    id.grid = id.grid.finer)) %>%
    write.table("intermediate_data/sd_height.txt", row.names = FALSE,
                col.names = FALSE, sep=",", quote=FALSE)
