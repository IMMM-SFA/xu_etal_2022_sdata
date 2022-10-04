library("dplyr")

extract.float <- function(str) {
  stringr::str_extract(str, "([0-9]*[.])?[0-9]+")
}
extract.unit <- function(str) {
  stringr::str_extract(str, "\\[.*\\]")
}

read.area.energy.in.path <- function(filepath, suf, testing=FALSE) {
  files = list.files(filepath, pattern=suf)
  if (testing) {
    files = files[[1]]
  }
  results <- lapply(files, function(f) {
    lines = readLines(sprintf("%s/%s", filepath, f))
    area.line = which(stringr::str_detect(lines, "\\>Total Building Area\\<"))[[1]]
    area = as.numeric(extract.float(lines[[area.line + 1]]))
    energy.line = which(stringr::str_detect(lines, "\\>Total End Uses\\<"))[[1]]
    elec = as.numeric(extract.float(lines[[energy.line + 1]]))
    gas = as.numeric(extract.float(lines[[energy.line + 2]]))
    energy.unit = extract.unit(lines[(which(stringr::str_detect(lines, "End Uses"))[[1]] + 4)])
    area.unit = extract.unit(lines[(which(stringr::str_detect(lines, "\\>Total Building Area\\<"))[[1]] - 3)])
    tibble::tibble(area = area, area.unit = area.unit, elec = elec, gas = gas, energy.unit = energy.unit, filename = f)
  }) %>%
    dplyr::bind_rows() %>%
    {.}
  results
}

read.area.energy.commercial <- function(filepath, suf) {
  read.area.energy.in.path(filepath, suf) %>%
    tidyr::separate(filename, into=c("standard", "type", "version", "suf"), sep="_") %>%
    dplyr::select(-suf)
}

read.area.energy.residential <- function(filepath, suf) {
  read.area.energy.in.path(filepath, suf) %>%
    tidyr::separate(filename, into=c("country", "type", "climate", "system", "space", "suf"), sep="\\+") %>%
    dplyr::select(-country, -suf) %>%
    {.}
}

read.area.energy.ref.bld <- function(filepath, suf) {
  read.area.energy.in.path(filepath, suf) %>%
    tidyr::separate(filename, into=letters[1:8], sep="_") %>%
    dplyr::select(-(b:h)) %>%
    dplyr::rename(type = a) %>%
    dplyr::mutate(type = gsub("Pre1980", "", type)) %>%
    dplyr::mutate(type = gsub("RefBldg", "", type)) %>%
    {.}
}

results.ashrae.2013 = read.area.energy.commercial("input_data/scorecards/ASHRAE901_STD2013",
                                                  suf="*_ElPaso.table.htm")
results.iecc.2012 = read.area.energy.commercial("input_data/scorecards/IECC_STD2012",
                                                 suf="*_ElPaso.table.htm")

results.res = read.area.energy.residential("input_data/scorecards/resstd_CZ3B_IECC_2015", suf="2015.table.htm")

results.com.compile.2013 <- results.ashrae.2013 %>%
  dplyr::bind_rows(results.iecc.2012) %>%
  dplyr::mutate(elec.GJ.per.m2 = elec / area,
                gas.GJ.per.m2 = gas / area) %>%
  dplyr::select(type, elec.GJ.per.m2, gas.GJ.per.m2, standard, version) %>%
  dplyr::mutate(EUI.GJ.per.m2 = elec.GJ.per.m2 + gas.GJ.per.m2) %>%
  dplyr::mutate(source = "PNNL") %>%
  {.}

results.com.compile.2013 %>%
  readr::write_csv("intermediate_data/scorecards_GJ_m2_commercial_PNNL_2013.csv")

results.res.compile <- results.res %>%
  dplyr::mutate(elec.GJ.per.m2 = elec * 0.00105485 / (area * 0.092903),
                gas.GJ.per.m2 = gas  * 0.00105485 / (area * 0.092903)) %>%
  dplyr::mutate(standard = "IECC2015", version = sprintf("%s+%s", system, space)) %>%
  dplyr::select(type, elec.GJ.per.m2, gas.GJ.per.m2, standard, version) %>%
  dplyr::mutate_at(vars(type), recode, "SF"="SingleFamily", "MF"="MultiFamily") %>%
  dplyr::mutate(EUI.GJ.per.m2 = elec.GJ.per.m2 + gas.GJ.per.m2) %>%
  dplyr::mutate(source = "PNNL") %>%
  {.}

results.res.compile %>%
  dplyr::group_by(type) %>%
  dplyr::summarise_at(vars(EUI.GJ.per.m2),
                      tibble::lst(min, Q1=~quantile(., probs=0.25), mean, median, Q3=~quantile(., probs=0.75), max)) %>%
  dplyr::ungroup()

results.res.compile %>%
  readr::write_csv("intermediate_data/scorecards_GJ_m2_residential_PNNL_2015.csv")

scorecards.compile <- results.res.compile %>%
  dplyr::filter(version == "gasfurnace+slab") %>%
  dplyr::bind_rows(results.com.compile.2013) %>%
  ## tidyr::unite(source, standard:version) %>%
  dplyr::select(type, source, EUI.GJ.per.m2) %>%
  dplyr::mutate_at(vars(type), recode,
                   "RestaurantSitDown"="FullServiceRestaurant",
                   "HotelSmall"="SmallHotel",
                   "HotelLarge"="LargeHotel",
                   "OfficeSmall"="SmallOffice",
                   "OfficeMedium"="MediumOffice",
                   "OfficeLarge"="LargeOffice",
                   "ApartmentMidRise"="MidriseApartment",
                   "SchoolPrimary"="PrimarySchool",
                   "SchoolSecondary"="SecondarySchool") %>%
  {.}

if (!file.exists("intermediate_data/annual_agg_by_time_by_idf_epw_2018.csv")) {
    df.sim.result <- readr::read_csv("intermediate_data/annual_sim_result_by_idf_epw_2018.csv")
    annual.sim.result <- df.sim.result %>%
        dplyr::group_by(idf.kw, epw.id) %>%
        dplyr::summarise_if(is.numeric, sum) %>%
        dplyr::ungroup() %>%
        {.}
    annual.sim.result %>%
        readr::write_csv("intermediate_data/annual_agg_by_time_by_idf_epw_2018.csv")
} else {
    annual.sim.result <- readr::read_csv("intermediate_data/annual_agg_by_time_by_idf_epw_2018.csv")
}

df.area.prototype = readr::read_csv("input_data/prototype_bldg_area.csv") %>%
    dplyr::mutate(idf.kw = gsub(".idf", "", idf.name, fixed=TRUE)) %>%
    dplyr::mutate(idf.kw = gsub(".", "_", idf.kw, fixed=TRUE)) %>%
    dplyr::select(-idf.name) %>%
    {.}

annual.sim.EUI <- annual.sim.result %>%
    dplyr::left_join(df.area.prototype, by="idf.kw") %>%
    dplyr::mutate(EUI.kbtu.per.sqft = energy.overall * 1e-9 * 947.817 / (prototype.m2 * 10.7639)) %>%
    dplyr::mutate(EUI.GJ.per.m2 = energy.overall * 1e-9 / prototype.m2) %>%
    dplyr::mutate(emission.GJ.per.m2 = emission.overall * 1e-9 / prototype.m2) %>%
    {.}

annual.sim.EUI.avg <- annual.sim.EUI %>%
    dplyr::select(-epw.id) %>%
    dplyr::group_by(idf.kw) %>%
    dplyr::summarise_if(is.numeric, mean) %>%
    dplyr::ungroup() %>%
    dplyr::select(idf.kw, EUI.GJ.per.m2, emission.GJ.per.m2) %>%
    tidyr::gather(variable, value, ends_with("per.m2"))

annual.sim.EUI.avg.compile.2013 <- annual.sim.EUI.avg %>%
  tidyr::spread(variable, value) %>%
  dplyr::select(-emission.GJ.per.m2) %>%
  tidyr::separate(idf.kw, into=letters[1:6], sep="\\-") %>%
  dplyr::filter((b == "2013") | (c == "2013")) %>%
  dplyr::select(a, EUI.GJ.per.m2) %>%
  dplyr::rename(type = a) %>%
  dplyr::mutate(source = "this study") %>%
  {.}

prototype.area <- readr::read_csv("input_data/prototype_bldg_area.csv") %>%
  dplyr::mutate(idf.kw = gsub(".idf", "", idf.name, fixed=TRUE)) %>%
  dplyr::mutate(idf.kw = gsub(".", "_", idf.kw, fixed=TRUE)) %>%
  {.}

source("compile_sim_result.R")

result.el.paso <- compile.sim.result("intermediate_data/EP_output/using_el_paso", prototype.area)

annual.sim.EUI.avg.compile.el.paso.2013 <- result.el.paso %>%
  dplyr::filter((b == "2013") | (c == "2013")) %>%
  dplyr::select(type, EUI.GJ.per.m2) %>%
  dplyr::mutate(source = "this study el paso") %>%
  na.omit() %>%
  {.}

scorecards.compile %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(EUI.GJ.per.m2 = mean(EUI.GJ.per.m2)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(source = "scorecards") %>%
  dplyr::bind_rows(annual.sim.EUI.avg.compile.2013) %>%
  dplyr::bind_rows(annual.sim.EUI.avg.compile.el.paso.2013) %>%
  tidyr::spread(source, EUI.GJ.per.m2) %>%
  readr::write_csv("intermediate_data/cmp_sim_avg_scorecard_avg_2013.csv")

## single and multifamily has really large results

results.ref.pre1980 = read.area.energy.ref.bld("input_data/scorecards/refbldg_3b_usa_ca_los_angeles_pre1980_v1.3_5.0", suf=".htm")

results.com.compile.pre1980 <- results.ref.pre1980 %>%
  dplyr::mutate(elec.GJ.per.m2 = elec / area,
                gas.GJ.per.m2 = gas / area) %>%
  dplyr::select(type, elec.GJ.per.m2, gas.GJ.per.m2) %>%
  dplyr::mutate(EUI.GJ.per.m2 = elec.GJ.per.m2 + gas.GJ.per.m2) %>%
  dplyr::mutate(source = "NREL") %>%
  {.}

annual.sim.EUI.avg.compile.pre1980 <- annual.sim.EUI.avg %>%
  tidyr::spread(variable, value) %>%
  dplyr::select(-emission.GJ.per.m2) %>%
  tidyr::separate(idf.kw, into=letters[1:6], sep="\\-") %>%
  dplyr::filter(c == "1980") %>%
  dplyr::select(a, EUI.GJ.per.m2) %>%
  dplyr::rename(type = a) %>%
  dplyr::filter(type != "Religious") %>%
  dplyr::mutate(source = "this study") %>%
  {.}

result.la <- compile.sim.result("intermediate_data/EP_output/using_LA_tmy2", prototype.area)

annual.sim.EUI.avg.compile.la.pre1980 <- result.la %>%
  dplyr::filter(c == "1980") %>%
  dplyr::select(type, EUI.GJ.per.m2) %>%
  dplyr::mutate(source = "this study LA tmy2") %>%
  na.omit() %>%
  {.}

results.com.compile.pre1980 %>%
  dplyr::mutate_at(vars(type), recode,
                   "Stand-aloneRetail"="RetailStandalone") %>%
  dplyr::select(type, EUI.GJ.per.m2, source) %>%
  dplyr::bind_rows(annual.sim.EUI.avg.compile.pre1980) %>%
  dplyr::bind_rows(annual.sim.EUI.avg.compile.la.pre1980) %>%
  tidyr::spread(source, EUI.GJ.per.m2) %>%
  dplyr::filter(!is.na(`this study`)) %>%
  readr::write_csv("intermediate_data/cmp_sim_avg_scorecard_com_pre1980.csv")
