library("dplyr")
library("janitor")

usetypes = c("commercial", "industrial", "institutional", "res_total")

source("read_energyAtlas.R")

df.sqft.cmp.by.fuel.atlas <- lapply(c("electricity", "gas", "total"), function(energy.type){
    read.energy.atlas(energy.type) %>%
        dplyr::filter(id.type == "counties") %>%
        dplyr::filter(name == "Los Angeles County") %>%
        select(usetype, sqft) %>%
        dplyr::mutate(variable = energy.type) %>%
        {.}
}) %>%
    dplyr::bind_rows() %>%
    ## million sqft
    dplyr::mutate(sqft = sqft * 1e-6) %>%
    tidyr::spread(variable, sqft)

df.sqft.cmp.by.fuel.atlas %>%
    readr::write_csv("intermediate_data/energyAtlas_county_milsqft_by_fuel.csv")

## this is before joining to grid
df.la.building <- sf::st_read("intermediate_data/compiled_LA_building.geojson")

df.la.building.remove.geo <- df.la.building

sf::st_geometry(df.la.building.remove.geo) <- NULL

size.type.summary <- df.la.building.remove.geo %>%
    dplyr::group_by(GeneralUseType, SpecificUseType) %>%
    dplyr::summarise(total.building.size.mil.sqft = sum(SQFTmain) * 1e-6,
                     mean.EffectiveYearBuilt = mean(EffectiveYearBuilt)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(building.size.percent = total.building.size.mil.sqft / sum(total.building.size.mil.sqft)) %>%
    dplyr::arrange(desc(building.size.percent)) %>%
    {.}

quartile.stats.by.type <- df.la.building.remove.geo %>%
    tibble::as_tibble() %>%
    dplyr::select(-(Shape_Area:remap.Energy.Atlas)) %>%
    tidyr::gather(variable, value, HEIGHT:SQFTmain) %>%
    na.omit() %>%
    dplyr::group_by(GeneralUseType,SpecificUseType, remap.EP.ref.building, variable) %>%
    dplyr::summarise_at(vars(value), tibble::lst(min, Q1=~quantile(., probs=0.25), median, Q3=~quantile(., probs=0.75), mean, max)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(remap.EP.ref.building != "?") %>%
    readr::write_csv("intermediate_data/quartile_summary_by_type.csv")

quartile.stats.by.ep.type <- df.la.building.remove.geo %>%
    tibble::as_tibble() %>%
    dplyr::select(-(Shape_Area:remap.Energy.Atlas)) %>%
    tidyr::gather(variable, value, HEIGHT:SQFTmain) %>%
    na.omit() %>%
    dplyr::group_by(remap.EP.ref.building, variable) %>%
    dplyr::summarise_at(vars(value), tibble::lst(min, Q1=~quantile(., probs=0.25), median, Q3=~quantile(., probs=0.75), mean, max)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(remap.EP.ref.building != "?") %>%
    readr::write_csv("intermediate_data/quartile_summary_by_EP_type.csv")

size.type.summary %>%
    dplyr::mutate(cum.total.percent = cumsum(building.size.percent)) %>%
    dplyr::filter(cum.total.percent < 0.95) %>%
    readr::write_csv("paper_table/top_size_year_by_type_LA_building.csv")

type.recode <- readr::read_csv("input_data/building_type_recode.csv") %>%
    dplyr::select(-count) %>%
    {.}

options(tibble.width = Inf)

to.plot.pie <- size.type.summary %>%
    dplyr::left_join(type.recode, by=c("GeneralUseType", "SpecificUseType")) %>%
    dplyr::group_by(`remap EP ref building`) %>%
    dplyr::summarise_at(vars(total.building.size.mil.sqft, building.size.percent), sum) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(building.size.percent)) %>%
    dplyr::mutate(type = ifelse(building.size.percent > 0.03, `remap EP ref building`, "other")) %>%
    dplyr::group_by(type) %>%
    dplyr::summarise_at(vars(total.building.size.mil.sqft, building.size.percent), sum) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(building.size.percent)) %>%
    {.}

to.plot.pie <- to.plot.pie %>%
    arrange(desc(type)) %>%
    mutate(prop = building.size.percent / sum(to.plot.pie$building.size.percent) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop ) %>%
    {.}

ggplot2::ggplot(to.plot.pie, ggplot2::aes(x="", y=prop, fill=type)) +
    ggplot2::geom_bar(stat="identity", width=1, color="white") +
    ggplot2::coord_polar("y", start=0) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position="none") +
    ggrepel::geom_text_repel(ggplot2::aes(y = ypos, label = type), color = "white", size=6) +
    ggplot2::scale_fill_brewer(palette="Set2")
ggplot2::ggsave("figures/building_type_pie.png")

la.building.summary <- df.la.building.remove.geo %>%
    tibble::as_tibble() %>%
    dplyr::filter(GeneralUseType != "(missing)") %>%
    dplyr::select(-remap.Energy.Atlas, -remap.EP.ref.building) %>%
    dplyr::left_join(type.recode, by=c("GeneralUseType", "SpecificUseType")) %>%
    dplyr::mutate(`remap Energy Atlas` = tolower(`remap Energy Atlas`)) %>%
    dplyr::group_by(`remap Energy Atlas`) %>%
    dplyr::summarise(SQFTmain = sum(SQFTmain)) %>%
    dplyr::ungroup() %>%
    {.}

res.total.summary <- la.building.summary %>%
    dplyr::filter(`remap Energy Atlas` %in% c("single_family", "multi_family", "residential_other", "residential_uncat")) %>%
    dplyr::summarise(SQFTmain = sum(SQFTmain)) %>%
    dplyr::mutate(`remap Energy Atlas` = "res_total")

df.sqft.total <- df.sqft.cmp.by.fuel.atlas %>%
    dplyr::select(usetype, total) %>%
    dplyr::rename(mil.sqft = total) %>%
    dplyr::mutate(source = "Energy Atlas")

la.building.summary %>%
    dplyr::bind_rows(res.total.summary) %>%
    dplyr::rename(sqft = SQFTmain, usetype = `remap Energy Atlas`) %>%
    ## million sqft
    dplyr::mutate(mil.sqft = sqft * 1e-6) %>%
    dplyr::select(-sqft) %>%
    dplyr::mutate(source = "compiled_LA_building") %>%
    dplyr::bind_rows(df.sqft.total) %>%
    tidyr::spread(source, mil.sqft) %>%
    dplyr::mutate(percent = `compiled_LA_building` / `Energy Atlas` * 100) %>%
    readr::write_csv("intermediate_data/cmp_compiled_LA_geojson_with_EnergyAtlas_sqft.csv")

df.assessor <- readr::read_csv("intermediate_data/Assessor_Parcels_Data_-_2019_col_subset.csv")

df.assessor.sqft <- df.assessor %>%
    dplyr::filter(GeneralUseType != "(missing)") %>%
    dplyr::left_join(type.recode, by=c("GeneralUseType", "SpecificUseType")) %>%
    dplyr::group_by(`remap Energy Atlas`) %>%
    dplyr::summarise(SQFTmain = sum(SQFTmain)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(`remap Energy Atlas` = tolower(`remap Energy Atlas`)) %>%
    {.}

df.assessor.res.sqft <- df.assessor.sqft %>%
    dplyr::filter(`remap Energy Atlas` %in% c("single_family", "multi_family", "residential_other", "residential_uncat")) %>%
    dplyr::summarise(SQFTmain = sum(SQFTmain)) %>%
    dplyr::mutate(`remap Energy Atlas` = "res_total")

df.assessor.sqft %>%
    dplyr::bind_rows(df.assessor.res.sqft) %>%
    dplyr::rename(sqft = SQFTmain, usetype = `remap Energy Atlas`) %>%
    ## million sqft
    dplyr::mutate(mil.sqft = sqft * 1e-6) %>%
    dplyr::select(-sqft) %>%
    dplyr::mutate(source = "Assessor") %>%
    dplyr::bind_rows(df.sqft.total) %>%
    tidyr::spread(source, mil.sqft) %>%
    readr::write_csv("intermediate_data/cmp_Assesor_with_EnergyAtlas_sqft.csv")

type.order.all = tibble::tibble(usetype=c("res_total", "single_family",
                                          "multi_family", "condo",
                                          "residential_other",
                                          "residential_uncat", "commercial",
                                          "industrial", "institutional",
                                          "agriculture", "uncat", "other"),
                                order=1:12) %>%
    {.}

readr::read_csv("intermediate_data/cmp_compiled_LA_geojson_with_EnergyAtlas_sqft.csv") %>%
    dplyr::select(-percent) %>%
    dplyr::left_join(
               readr::read_csv("intermediate_data/cmp_Assesor_with_EnergyAtlas_sqft.csv") %>%
               dplyr::select(-`Energy Atlas`) %>%
               {.}
           , by="usetype") %>%
    dplyr::rename(`this study`=`compiled_LA_building`) %>%
    dplyr::mutate(this.over.assessor = `this study` / Assessor,
                  atlas.over.assessor = `Energy Atlas` / Assessor) %>%
    dplyr::inner_join(type.order.all, by="usetype") %>%
    dplyr::arrange(order) %>%
    dplyr::select(-order) %>%
    janitor::adorn_totals() %>%
    readr::write_csv("intermediate_data/cmp_this_atlas_assessor_mil_sqft.csv")

df.energy.county.atlas <- lapply(c("electricity", "gas", "total"), function(energy.type){
    read.energy.atlas(energy.type, do.unit.conversion = TRUE) %>%
        dplyr::filter(id.type == "counties") %>%
        dplyr::filter(name == "Los Angeles County") %>%
        dplyr::select(usetype, usage) %>%
        dplyr::mutate(variable = energy.type) %>%
        {.}
}) %>%
    dplyr::bind_rows() %>%
    tidyr::spread(variable, usage)

df.energy.county.atlas

annual.sim.result.idf.epw.2016 <- readr::read_csv("intermediate_data/annual_total_result_2016.csv")

prototype.area <- readr::read_csv("input_data/prototype_bldg_area.csv") %>%
    dplyr::mutate(idf.kw = gsub(".idf", "", idf.name, fixed=TRUE)) %>%
    dplyr::mutate(idf.kw = gsub(".", "_", idf.kw, fixed=TRUE)) %>%
    {.}

## get idf.kw to EnergyAtlas type map
building.metadata <- readr::read_csv("output_data/building_metadata.csv")

get.agg.sim.energy.by.atlas.type <- function(df) {
    building.metadata %>%
        tibble::as_tibble() %>%
        dplyr::select(OBJECTID, GeneralUseType, SpecificUseType, usetype, building.area.m2, idf.kw, id.grid.coarse) %>%
        dplyr::rename(epw.id = id.grid.coarse) %>%
        dplyr::group_by(usetype, idf.kw, epw.id) %>%
        dplyr::summarise(building.area.m2 = sum(building.area.m2)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(df, by=c("idf.kw", "epw.id")) %>%
        dplyr::left_join(prototype.area, by="idf.kw") %>%
        tidyr::gather(variable, value, energy.elec:energy.gas) %>%
        dplyr::mutate(value = value / prototype.m2 * building.area.m2) %>%
        tidyr::spread(variable, value) %>%
        dplyr::select(-epw.id, -prototype.m2) %>%
        dplyr::group_by(usetype) %>%
        dplyr::summarise_if(is.numeric, sum) %>%
        dplyr::ungroup() %>%
        {.}
}

agg.by.energy.atlas.type.2018 <- get.agg.sim.energy.by.atlas.type(annual.sim.result.idf.epw.2018)

cec.2018 <- readr::read_csv("input_data/CECdata/ElectricityByCounty_2018.csv") %>%
    dplyr::select(-`Total Usage`, -County) %>%
    dplyr::rename(electricity.Gwh = `2018`) %>%
    dplyr::mutate(source = "CEC") %>%
    {.}

cec.2018 

agg.by.energy.atlas.type.2018 %>%
    dplyr::select(usetype, energy.elec) %>%
    dplyr::mutate(energy.elec = energy.elec * 2.77778e-7 * 1e-6) %>%
    dplyr::mutate(Sector = ifelse(usetype %in% c("single_family", "multi_family", "residential_other"), "Residential", "Non-Residential")) %>%
    dplyr::group_by(Sector) %>%
    dplyr::summarise(energy.elec = sum(energy.elec)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(electricity.Gwh = energy.elec) %>%
    janitor::adorn_totals() %>%
    dplyr::mutate(source = "this study") %>%
    dplyr::bind_rows(cec.2018) %>%
    tidyr::spread(source, electricity.Gwh) %>%
    dplyr::mutate(ratio = `this study` / CEC) %>%
readr::write_csv("intermediate_data/sim_elec_cmp_CEC_2018.csv")

get.agg.sim.energy.by.atlas.type(annual.sim.result.idf.epw.2016)
    readr::write_csv("intermediate_data/energy_emission_by_EnergyAtlas_type_2016.csv")

convert.unit.sim.summary <- function(df) {
    df %>%
        ## J to Gwh
        dplyr::mutate(energy.elec = energy.elec * 2.77778e-7 * 1e-6) %>%
        ## J to Mega therm
        dplyr::mutate(energy.gas = energy.gas * 9.48043e-9 * 1e-6) %>%
        ## J to Tbtu
        dplyr::mutate(energy.overall = energy.overall * 0.000947817 * 1e-12) %>%
        dplyr::rename(elec.Gwh=energy.elec,
                      gas.Gtherm=energy.gas,
                      total.Gbtu=energy.overall) %>%
        dplyr::mutate(building.area.mil.m2 = building.area.m2 * 1e-6) %>%
        dplyr::select(-building.area.m2) %>%
        {.}
}

county.sim.result.by.usetype <- agg.by.energy.atlas.type %>%
    dplyr::select(-starts_with("emission")) %>%
    convert.unit.sim.summary() %>%
    {.}
county.sim.result.by.usetype %>%
    readr::write_csv("intermediate_data/county_sim_result_by_usetype.csv")

county.atlas.result.by.usetype <- df.energy.county.atlas %>%
    dplyr::rename(elec.Gwh=electricity,
                  gas.Gtherm=gas,
                  total.Gbtu=total) %>%
    dplyr::left_join(df.sqft.cmp.by.fuel.atlas %>%
                     dplyr::mutate(building.area.mil.m2 = total * 0.0929) %>%
                     dplyr::select(usetype, building.area.mil.m2))
county.atlas.result.by.usetype %>%
    readr::write_csv("intermediate_data/county_atlas_result_by_usetype.csv")

cmp.no.res_total <- county.sim.result.by.usetype %>%
    dplyr::mutate(source = "simulation 2016") %>%
    dplyr::bind_rows(county.atlas.result.by.usetype %>%
                     dplyr::mutate(source = "atlas 2016")) %>%
    dplyr::mutate_at(vars(usetype), tolower) %>%
    dplyr::group_by(usetype) %>%
    dplyr::filter(n()>1) %>%
    dplyr::ungroup() %>%
    {.}

cmp.res_total <- county.sim.result.by.usetype %>%
    dplyr::filter(usetype %in% c("single_family", "multi_family", "residential_other")) %>%
    dplyr::mutate(usetype = "res_total") %>%
    dplyr::group_by(usetype) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(source = "simulation 2016") %>%
    dplyr::bind_rows(county.atlas.result.by.usetype %>%
                     dplyr::filter(usetype == "res_total") %>%
                     dplyr::mutate(source = "atlas 2016")) %>%
    {.}

type.order = tibble::tibble(usetype=c("single_family", "multi_family",
                                      "residential_other", "res_total",
                                      "commercial", "industrial",
                                      "institutional"),
                            order=1:7) %>%
    {.}

## table used in paper
cmp.no.res_total %>%
    dplyr::bind_rows(cmp.res_total) %>%
    tidyr::gather(variable, value, elec.Gwh:building.area.mil.m2) %>%
    tidyr::spread(source, value) %>%
    dplyr::mutate(ratio = `simulation 2016` / `atlas 2016`) %>%
    dplyr::inner_join(type.order, by="usetype") %>%
    dplyr::arrange(order, variable) %>%
    readr::write_csv("intermediate_data/county_cmp_sim_atlas.csv")
