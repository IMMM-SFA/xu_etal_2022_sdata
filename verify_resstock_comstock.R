prototype.area <- readr::read_csv("input_data/prototype_bldg_area.csv") %>%
    dplyr::mutate(idf.kw = gsub(".idf", "", idf.name, fixed=TRUE)) %>%
    dplyr::mutate(idf.kw = gsub(".", "_", idf.kw, fixed=TRUE)) %>%
    {.}

df.restock = readr::read_tsv("input_data/resstock/metadata.tsv")

df.restock.la <- df.restock %>%
  dplyr::filter(in.resstock_county_id == "CA, Los Angeles County") %>%
  dplyr::filter(stringr::str_detect(in.geometry_building_type_recs, "-Family")) %>%
  dplyr::select(bldg_id,
                in.county,
                out.electricity.total.energy_consumption_intensity,
                out.natural_gas.total.energy_consumption_intensity,
                in.geometry_building_type_recs,
                in.vintage_acs,
                in.weather_file_TMY3) %>%
  dplyr::rename(elec = out.electricity.total.energy_consumption_intensity,
                gas = out.natural_gas.total.energy_consumption_intensity) %>%
  tidyr::separate(in.geometry_building_type_recs, into=c("type", "suf", "suf2"), sep="-") %>%
  dplyr::select(-suf, -suf2) %>%
  dplyr::mutate(type = sprintf("%sFamily", type)) %>%
  {.}

df.resstock.mod.vintage <- df.restock.la %>%
  dplyr::rename(vintage = in.vintage_acs) %>%
  {.}

get.resstock.summary <- function(df) {
  df %>%
    tidyr::gather(variable, kwh.per.sqft, elec:gas) %>%
    ## convert kwh/sqft to GJ/m2
    dplyr::mutate(EUI.GJ.per.m2 = kwh.per.sqft * 0.0036 / 0.092903) %>%
    dplyr::group_by(type, vintage, variable) %>%
    dplyr::summarise_at(vars(EUI.GJ.per.m2),
                        tibble::lst(min, Q1=~quantile(., probs=0.25), mean, median, Q3=~quantile(., probs=0.75), `90th`=~quantile(., probs=0.90), max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(type, vintage, variable) %>%
    {.}
}

get.resstock.summary.percentile <- function(df, prob) {
  df %>%
    tidyr::gather(variable, kwh.per.sqft, elec:gas) %>%
    ## convert kwh/sqft to GJ/m2
    dplyr::mutate(EUI.GJ.per.m2 = kwh.per.sqft * 0.0036 / 0.092903) %>%
    dplyr::group_by(type, vintage) %>%
    dplyr::summarise_at(vars(EUI.GJ.per.m2),
                        tibble::lst(percentile=~quantile(., probs=prob))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(type, vintage) %>%
    {.}
}

resstock.summary <- get.resstock.summary(df.resstock.mod.vintage)

get.resstock.summary.percentile(df.resstock.mod.vintage, 0.85) %>%
  filter(type == "MultiFamily")

resstock.summary %>%
  readr::write_csv("intermediate_data/resstock_summary_stats.csv")

## get epw used in resstock for LA county

source("read_epw.R")

if (!file.exists("intermediate_data/resstock/G0600370_tmy3.epw")) {
  la.epw.base <- read.epw("input_data/M02_EnergyPlus_Forcing_Historical_LowRes/USA_CA_Los.Angeles.Intl.AP.722950_TMY3.epw")

  ## read resstock weather for LA county
  la.weather.to.replace <- readr::read_csv("input_data/resstock/G0600370_tmy3.csv")

  la.epw.base$DryBulb.C = la.weather.to.replace$`Dry Bulb Temperature [°C]`
  la.epw.base$RelHum.percent = la.weather.to.replace$`Relative Humidity [%]`
  la.epw.base$`WindSpd.m/s` = la.weather.to.replace$`Wind Speed [m/s]`
  la.epw.base$WindDir.deg = la.weather.to.replace$`Wind Direction [Deg]`
  la.epw.base$`GloHorzRad.Wh/m2` = la.weather.to.replace$`Global Horizontal Radiation [W/m2]`
  la.epw.base$`DirNormRad.Wh/m2` = la.weather.to.replace$`Global Horizontal Radiation [W/m2]`
  la.epw.base$`DifHorzRad.Wh/m2` = la.weather.to.replace$`Diffuse Horizontal Radiation [W/m2]`

  la.epw.base %>%
    readr::write_csv("intermediate_data/resstock_weather_temp.csv")

  lines = readLines("input_data/M02_EnergyPlus_Forcing_Historical_LowRes/USA_CA_Los.Angeles.Intl.AP.722950_TMY3.epw")
  data.lines = readLines("intermediate_data/resstock_weather_temp.csv")

  newlines = c(lines[1:8], data.lines[2:length( data.lines)])
  writeLines(newlines, "intermediate_data/resstock/G0600370_tmy3.epw")
}

result.G0600370 <- compile.sim.result("intermediate_data/EP_output/using_G0600370", prototype.area)

result.G0600370.compile <- result.G0600370 %>%
  dplyr::mutate(vintage = ifelse(is.na(c), b, c)) %>%
  dplyr::mutate(vintage = ifelse(vintage == "LA", "-", vintage)) %>%
  dplyr::mutate(vintage = ifelse(vintage == "1980", "pre-1980", vintage)) %>%
  dplyr::select(vintage, type, EUI.GJ.per.m2) %>%
  na.omit() %>%
  {.}

result.G0600370.compile %>%
  readr::write_csv("intermediate_data/sim_EUI_with_resstock_comstock_weather.csv")

df.comstock = readr::read_tsv("input_data/comstock/metadata.tsv")

df.comstock.la <- df.comstock %>%
  dplyr::filter(in.state_abbreviation == "CA") %>%
  dplyr::filter(in.resstock_county_id == "CA, Los Angeles County") %>%
  dplyr::select(bldg_id,
                in.county,
                out.electricity.total.energy_consumption_intensity,
                out.natural_gas.total.energy_consumption_intensity,
                in.building_type,
                in.energy_code_followed_during_original_building_construction,
                in.weather_file_TMY3) %>%
  dplyr::rename(elec = out.electricity.total.energy_consumption_intensity,
                gas = out.natural_gas.total.energy_consumption_intensity,
                type = in.building_type) %>%
  dplyr::mutate(vintage = gsub("ComStock DEER ", "", in.energy_code_followed_during_original_building_construction)) %>%
  {.}

comstock.summary <- get.resstock.summary(df.comtock.la)

comstock.summary %>%
  readr::write_csv("intermediate_data/comstock_summary_stats.csv")

comstock.summary %>%
  dplyr::rename(vintage.comstock = vintage) %>%
  dplyr::mutate(vintage = case_when(vintage.comstock %in% c("1985", "1996", "2003", "2007") ~ "2004",
                                    vintage.comstock %in% c("2011", "2014", "2015", "2017") ~ "2013",
                                    (vintage.comstock == "Pre-1975") ~ "pre-1980")) %>%
  dplyr::inner_join(result.G0600370.compile, by=c("type", "vintage")) %>%
  readr::write_csv("intermediate_data/cmp_comstock_this_study_EUI.csv")

result.G0600370.compile.elec.gas <- compile.sim.result.elec.gas("intermediate_data/EP_output/using_G0600370", prototype.area) %>%
  dplyr::mutate(vintage = ifelse(is.na(c), b, c)) %>%
  dplyr::mutate(vintage = ifelse(vintage == "LA", "-", vintage)) %>%
  dplyr::mutate(vintage = ifelse(vintage == "1980", "pre 1980", vintage)) %>%
  dplyr::select(vintage, type, variable, EUI.GJ.per.m2) %>%
  na.omit() %>%
  dplyr::mutate(source = "this study") %>%
  {.}

result.G0600370.compile.elec.gas.res <- result.G0600370.compile.elec.gas %>%
  dplyr::filter(type %in% c("SingleFamily", "MultiFamily")) %>%
  {.}

get.plot.df.cmp.res.com.stock.elec.gas <- function(df.res.com, source.label, df.sim) {
  df.res.com %>%
    tidyr::gather(variable, kwh.per.sqft, elec:gas) %>%
    ## convert kwh/sqft to GJ/m2
    dplyr::mutate(EUI.GJ.per.m2 = kwh.per.sqft * 0.0036 / 0.092903) %>%
    dplyr::select(vintage, type, variable, EUI.GJ.per.m2) %>%
    dplyr::mutate(source = source.label) %>%
    dplyr::bind_rows(df.sim) %>%
    dplyr::group_by(type, vintage, variable) %>%
    dplyr::filter(length(unique(source)) > 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(vintage = factor(vintage, levels = c("pre 1980", "2004", "2013"))) %>%
    dplyr::mutate_at(vars(variable), recode, "elec"="Electricity", "gas"="Gas") %>%
    {.}
}

get.plot.df.cmp.res.com.stock.total <- function(df.res.com, source.label, df.sim) {
  df.res.com %>%
    ## convert kwh/sqft to GJ/m2
    dplyr::mutate(EUI.GJ.per.m2 = (elec + gas) * 0.0036 / 0.092903) %>%
    dplyr::select(vintage, type, EUI.GJ.per.m2) %>%
    dplyr::mutate(source = source.label) %>%
    dplyr::bind_rows(df.sim) %>%
    dplyr::group_by(type, vintage) %>%
    dplyr::filter(length(unique(source)) > 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(vintage = factor(vintage, levels = c("pre 1980", "2004", "2013"))) %>%
    {.}
}

df.res.to.plot <- df.resstock.mod.vintage %>%
  dplyr::mutate(vintage = case_when(vintage == "2010s" ~ "2013",
                                    vintage %in% c("2000-09", "1980-99") ~ "2004",
                                    TRUE ~ "pre 1980")) %>%
  {.}

get.plot.df.cmp.res.com.stock.elec.gas(df.res.to.plot, "ResStock", result.G0600370.compile.elec.gas.res) %>%
  ggplot2::ggplot(ggplot2::aes(x = vintage, y = EUI.GJ.per.m2, fill = source)) +
  ggplot2::geom_boxplot(outlier.size = 0.2, lwd = 0.2) +
  ggplot2::facet_grid(type~variable) +
  ggplot2::xlab("vintage") +
  ggplot2::ylab("GJ/m2") +
  ggplot2::theme()
ggplot2::ggsave("figures/cmp_resstock_this_study_elecgas.png", width = 7, height = 5)

result.G0600370.compile.elec.gas.res.total <- result.G0600370.compile.elec.gas.res %>%
  dplyr::group_by(vintage, type) %>%
  dplyr::summarise(EUI.GJ.per.m2 = sum(EUI.GJ.per.m2),
                   source = first(source)) %>%
  dplyr::ungroup() %>%
  {.}

df.res.to.plot %>%
  get.plot.df.cmp.res.com.stock.total(source.label = "ResStock", df.sim = result.G0600370.compile.elec.gas.res.total) %>%
  ggplot2::ggplot(ggplot2::aes(x = type, y = EUI.GJ.per.m2, fill = source)) +
  ggplot2::geom_boxplot(outlier.size = 0.2, lwd = 0.2) +
  ggplot2::facet_wrap(vintage~., nrow = 1) +
  ggplot2::ylab("GJ/m2") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), legend.position = "bottom")
ggplot2::ggsave("figures/cmp_resstock_this_study.png", width = 6, height = 6)

result.G0600370.compile.elec.gas.nonres.total <- result.G0600370.compile.elec.gas %>%
  dplyr::filter(!(type %in% c("SingleFamily", "MultiFamily"))) %>%
  dplyr::group_by(vintage, type) %>%
  dplyr::summarise(EUI.GJ.per.m2 = sum(EUI.GJ.per.m2),
                   source = first(source)) %>%
  dplyr::ungroup() %>%
  {.}

df.com.to.plot <- df.comstock.la %>%
  dplyr::mutate(vintage = case_when(vintage %in% c("2011", "2014", "2015", "2017") ~ "2013",
                                    vintage %in% c("1985", "1996", "2003", "2007") ~ "2004",
                                    TRUE ~ "pre 1980")) %>%
  {.}

df.com.to.plot %>%
  dplyr::filter(type != "SecondarySchool") %>%
  get.plot.df.cmp.res.com.stock.total(source.label = "ComStock", df.sim = result.G0600370.compile.elec.gas.nonres.total) %>%
  ggplot2::ggplot(ggplot2::aes(x = type, y = EUI.GJ.per.m2, fill = source)) +
  ggplot2::geom_boxplot(outlier.size = 0.2, lwd = 0.2) +
  ggplot2::facet_wrap(vintage~., nrow = 1) +
  ggplot2::ylab("GJ/m2") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), legend.position = "bottom")
ggplot2::ggsave("figures/cmp_comstock_this_study.png", width = 8, height = 6)