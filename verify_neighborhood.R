library("dplyr")
library("tmap")
library("ggpmisc")

sf::sf_use_s2(FALSE)


la.nb.geo <- sf::st_read("input_data/EnergyAtlas/neighborhoods/neighborhoods.shp")

la.nb.geo.4326 <- sf::st_transform(la.nb.geo, crs=4326)

la.nb.geo.4326.id <- la.nb.geo.4326 %>%
    dplyr::select(neighborho)

if (!file.exists("intermediate_data/building_to_neighborhood_join.geojson")) {
    df.centroid <- sf::st_read("output_data/building_metadata.geojson")
    df.building.nb <- sf::st_join(df.centroid, la.nb.geo.4326, join = sf::st_within)
    df.centroid %>%
        tibble::as_tibble() %>%
        nrow()
    la.nb.geo.4326 %>%
        tibble::as_tibble() %>%
        nrow()
    df.building.nb %>%
        sf::st_write("intermediate_data/building_to_neighborhood_join.geojson")
} else {
    df.building.nb <- sf::st_read("intermediate_data/building_to_neighborhood_join.geojson")
}

df.building.nb %>%
    tibble::as_tibble() %>%
    distinct(OBJECTID, neighborho) %>%
    readr::write_csv("intermediate_data/building_id_to_neighborhood.csv")

joined <- df.building.nb %>%
    tibble::as_tibble() %>%
    dplyr::filter(!is.na(neighborho)) %>%
    {.}

joined %>%
    readr::write_csv("intermediate_data/building_id_to_nb_with_all_cols.csv")

joined %>%
    distinct(neighborho) %>%
    {.}
joined %>%
    distinct(OBJECTID) %>%
    {.}

annual.sim.result.idf.epw.2016 <- readr::read_csv("intermediate_data/annual_total_result_2016.csv")

prototype.area <- readr::read_csv("input_data/prototype_bldg_area.csv") %>%
    dplyr::mutate(idf.kw = gsub(".idf", "", idf.name, fixed=TRUE)) %>%
    dplyr::mutate(idf.kw = gsub(".", "_", idf.kw, fixed=TRUE)) %>%
    {.}

sim.by.nb.2016 <- joined %>%
    dplyr::rename(epw.id=id.grid.coarse) %>%
    dplyr::group_by(neighborho, usetype, idf.kw, epw.id) %>%
    dplyr::summarise(building.area.m2 = sum(building.area.m2)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(annual.sim.result.idf.epw.2016, by=c("idf.kw", "epw.id")) %>%
    dplyr::inner_join(prototype.area, by="idf.kw") %>%
    tidyr::gather(variable, value, emission.exfiltration:energy.gas) %>%
    dplyr::mutate(value = value / prototype.m2 * building.area.m2) %>%
    tidyr::spread(variable, value) %>%
    {.}

sim.by.nb.2016 %>%
    readr::write_csv("intermediate_data/annual_sim_result_by_neighborhood_2016.csv")

nb.with.simulation.data <- sim.by.nb.2016 %>%
    dplyr::distinct(neighborho)

source("read_energyAtlas.R")

df.energy.county.nb <- lapply(c("electricity", "gas", "total"), function(energy.type){
    read.energy.atlas(energy.type) %>%
        dplyr::filter(id.type == "neighborhoods") %>%
        dplyr::select(id.num, usetype, usage, sqft) %>%
        dplyr::mutate(variable = energy.type) %>%
        {.}
}) %>%
    {.}

sapply(df.energy.county.nb, function(x) x %>% distinct(id.num) %>% nrow())
sapply(df.energy.county.nb, function(x) x %>% nrow())

df.energy.county.nb.remove.masked <- lapply(c("electricity", "gas", "total"), function(energy.type){
    read.energy.atlas(energy.type, remove.masked.data = TRUE) %>%
        dplyr::filter(id.type == "neighborhoods") %>%
        dplyr::select(id.num, usetype, usage, sqft) %>%
        dplyr::mutate(usage = as.numeric(usage)) %>%
        dplyr::mutate(variable = energy.type) %>%
        {.}
}) %>%
    dplyr::bind_rows() %>%
    {.}

df.remove.na <- df.energy.county.nb.remove.masked %>%
    dplyr::filter(!is.na(usage))

df.keep.data.with.sim <- df.remove.na %>%
    dplyr::mutate(id.num = as.numeric(id.num)) %>%
    dplyr::inner_join(nb.with.simulation.data, by=c("id.num"="neighborho")) %>%
    {.}

df.keep.data.with.sim %>%
    dplyr::filter(variable == "total")

dfs <- list(df.energy.county.nb.remove.masked, df.remove.na, df.keep.data.with.sim)
step.labels <- c("remove masked data",
                 "remove missing data",
                 "keep neighborhoods with simulation results")

lapply(seq_along(dfs), function(i) {
    dfs[[i]] %>%
        dplyr::group_by(variable) %>%
        dplyr::summarise(num.neighborhood = length(unique(id.num)),
                         num.record = n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(step = step.labels[[i]]) %>%
        {.}
}) %>%
    dplyr::bind_rows() %>%
    readr::write_csv("intermediate_data/neighborhood_atlas_filter_step.csv")

sim.by.nb.usetype.2016 <- sim.by.nb.2016 %>%
    dplyr::select(neighborho, usetype, building.area.m2, starts_with("energy")) %>%
    ## J to kwh
    dplyr::mutate(electricity = energy.elec * 2.77778e-7,
                  ## J to therm
                  gas = energy.gas * 9.48043e-9,
                  ## J to btu
                  total = energy.overall * 0.000947817) %>%
    dplyr::select(-starts_with("energy")) %>%
    dplyr::group_by(neighborho, usetype) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::ungroup() %>%
    {.}

cmp.energy <- sim.by.nb.usetype.2016 %>%
    tidyr::gather(variable, usage, electricity:total) %>%
    dplyr::select(-building.area.m2) %>%
    dplyr::rename(id.num=neighborho) %>%
    dplyr::mutate(source = "simulation 2016") %>%
    dplyr::bind_rows(df.keep.data.with.sim %>%
                     dplyr::select(-sqft) %>%
                     dplyr::mutate(source = "atlas 2016") %>%
                     {.}) %>%
    {.}

cmp.energy %>%
    readr::write_csv("neighborhood_usage_by_type_simulation_atlas_cmp_2016.csv")

cmp.by.type <- cmp.energy %>%
    dplyr::group_by(id.num, usetype, variable) %>%
    dplyr::filter(n() == 2) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(id.num, usetype, variable, source) %>%
    dplyr::filter(variable == "total") %>%
    dplyr::mutate(usage = usage * 1e-9) %>%
    dplyr::group_by(usetype) %>%
    dplyr::group_split()

plot.map.cmp <- function(df, fill.col, palette.name, title.str, output.name) {
    p <- df %>%
        dplyr::inner_join(la.nb.geo.4326.id, by=c("id.num"="neighborho")) %>%
        sf::st_as_sf() %>%
        tm_shape() +
        tm_polygons(fill.col, n=10, style="quantile", palette=palette.name) +
        tm_facets(by="source") +
        tm_layout(main.title = title.str,
                  legend.position=c("left", "bottom"), legend.outside=FALSE)
    tmap_save(p, output.name, height=5)
}

## plot by each usetype
for (df.i in cmp.by.type) {
    type.i = df.i$usetype[[1]]
    print(type.i)
    plot.map.cmp(df.i, "usage", "YlOrRd",
                 sprintf("%s building electricity + gas (GBtu)", type.i),
                 sprintf("figures/cmp_neighbor_btu_total_%s.png", type.i))
}

plot.scatter.cmp <- function(df, title.str, output.name) {
    limit = max(max(df$`atlas 2016`, na.rm = TRUE), max(df$`simulation 2016`, na.rm = TRUE))
    print(limit)
    df %>%
        ggplot2::ggplot(ggplot2::aes(x=`atlas 2016`, y=`simulation 2016`)) +
        ggplot2::geom_point(size = 0.2) +
        ggpmisc::stat_poly_line(formula = y ~ 0 + x) +
        ggpmisc::stat_poly_eq(formula = y ~ 0 + x, aes(label = paste(after_stat(eq.label),
                                                                     after_stat(rr.label), sep = "*\", \"*"))) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        ggplot2::ggtitle(title.str) +
        ggplot2::coord_cartesian(x = c(0, limit), y = c(0, limit)) +
        ggplot2::theme()
    ggplot2::ggsave(output.name, width = 6, height = 6)
}

for (df.i in cmp.by.type) {
    type.i = df.i$usetype[[1]]
    print(type.i)
    df.i %>%
        dplyr::filter(variable == "total") %>%
        dplyr::mutate(usage = usage * 1e-9) %>%
        tidyr::spread(source, usage) %>%
        plot.scatter.cmp(sprintf("Electricity + Gas (GBtu) comparison for %s building", type.i),
                         sprintf("figures/scatter_neighbor_btu_total_%s.png", type.i))
}

## removed type
df.atlas.nb.total <- df.keep.data.with.sim %>%
    dplyr::filter(usetype %in% c("res_total", "commercial", "industrial", "institutional")) %>%
    dplyr::group_by(id.num, variable) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::ungroup() %>%
    {.}

df.atlas.nb.total

cmp.energy.total <- sim.by.nb.usetype.2016 %>%
    dplyr::group_by(neighborho) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::ungroup() %>%
    tidyr::gather(variable, usage, electricity:total) %>%
    dplyr::select(-building.area.m2) %>%
    dplyr::rename(id.num=neighborho) %>%
    dplyr::mutate(source = "simulation 2016") %>%
    dplyr::bind_rows(df.atlas.nb.total %>%
                     dplyr::select(-sqft) %>%
                     dplyr::mutate(source = "atlas 2016") %>%
                     {.}) %>%
    {.}

cmp.energy.total %>%
    readr::write_csv("neighborhood_usage_simulation_atlas_cmp_2016.csv")

cmp.total.by.fuel <- cmp.energy.total %>%
    ## kwh to TJ
    dplyr::mutate(usage = case_when(variable == "electricity" ~ usage * 0.0036 * 1e-3,
                                    variable == "gas" ~ usage * 0.10548 * 1e-3,
                                    variable == "total" ~ usage * 1.05506e-6 * 1e-3)) %>%
    dplyr::group_by(id.num, variable) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(variable) %>%
    dplyr::group_split()

for (df.j in cmp.total.by.fuel) {
    fuel.j = df.j$variable[[1]]
    plot.map.cmp(df.j, fill.col="usage", palette.name="YlOrRd",
                 title.str=sprintf(sprintf("%s (GBtu)", fuel.j)),
                 output.name=sprintf("figures/cmp_neighbor_total_by_fuel_%s.png", fuel.j))
}

for (df.j in cmp.total.by.fuel) {
    fuel.j = df.j$variable[[1]]
    df.j %>%
        tidyr::spread(source, usage) %>%
        plot.scatter.cmp(sprintf("%s (GBtu) comparison", fuel.j),
                         sprintf("figures/scatter_neighbor_total_by_fuel_%s.png", fuel.j))
}

cmp.size <- df.keep.data.with.sim %>%
    dplyr::select(-usage) %>%
    dplyr::filter(variable == "total") %>%
    dplyr::select(-variable) %>%
    dplyr::mutate(building.area.m2 = sqft * 0.0929) %>%
    dplyr::select(-sqft) %>%
    dplyr::mutate(source = "atlas 2016") %>%
    dplyr::bind_rows(sim.by.nb.usetype.2016 %>%
                     dplyr::select(neighborho, usetype, building.area.m2) %>%
                     dplyr::rename(id.num = neighborho) %>%
                     dplyr::mutate(source = "simulation 2016") %>%
                     {.}) %>%
    dplyr::group_by(id.num, usetype) %>%
    dplyr::filter(n() == 2) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(id.num, usetype, source) %>%
    {.}

size.by.type <- cmp.size %>%
    dplyr::mutate(building.area.thousand.m2 = building.area.m2 * 1e-3) %>%
    dplyr::select(-building.area.m2) %>%
    dplyr::group_by(usetype) %>%
    dplyr::group_split()

## plot by each usetype
for (df.i in size.by.type) {
    type.i = df.i$usetype[[1]]
    print(type.i)
    plot.map.cmp(df.i, "building.area.thousand.m2", "Purples",
                 sprintf("%s building size (thousand m2)", type.i),
                 sprintf("figures/cmp_neighbor_bld_size_%s.png", type.i))
}

for (df.i in size.by.type) {
    type.i = df.i$usetype[[1]]
    print(type.i)
    df.i %>%
        tidyr::spread(source, building.area.thousand.m2) %>%
        plot.scatter.cmp(
            sprintf("%s building size compare (thousand m2)", type.i),
            sprintf("figures/scatter_neighbor_bld_size_%s.png", type.i))
}
