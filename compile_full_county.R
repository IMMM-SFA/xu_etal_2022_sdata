## copied in from CityBES/LA/data-raw/
## modified filepath to suit the current folder setting, added some comments
## data files will be ignored and uploaded
## fixme: run through and verify

library("dplyr")
library("tibble")

## set work folder to be inside "code"
## setwd("code")

## read building characteristics starts -------------------------------------------
if (!file.exists("intermediate_data/Assessor_Parcels_Data_-_2019_col_subset.csv")) {
    ## had some parsing failure in HouseFraction. This column is not used, so should be fine
    df = readr::read_csv("input_data/Assessor_Parcels_Data_-_2019.csv")
    df.select.col <- df %>%
        dplyr::select(AssessorID, GeneralUseType, SpecificUseType, SQFTmain, YearBuilt, EffectiveYearBuilt, CENTER_LAT, CENTER_LON) %>%
        {.}
    df.select.col %>%
        readr::write_csv("intermediate_data/Assessor_Parcels_Data_-_2019_col_subset.csv")
} else {
    ## can load this smaller data file when not run the first time
    df.select.col <- readr::read_csv("intermediate_data/Assessor_Parcels_Data_-_2019_col_subset.csv")
}

## invalid building size
df.select.col %>%
  dplyr::filter(SQFTmain == 0) %>%
  {.}

## invalid built year
df.select.col %>%
  dplyr::filter(EffectiveYearBuilt == 0) %>%
  {.}

df.select.col.geo <- df.select.col %>%
  dplyr::filter(!is.na(CENTER_LON)) %>%
  sf::st_as_sf(coords=c("CENTER_LON", "CENTER_LAT"), crs=4326)
## read building characteristics ends  -------------------------------------------

df.geo <- sf::st_read("input_data/LARIAC6_LA_County.geojson")

df.geo %>%
  nrow()

df.geo.nogeom <- df.geo

sf::st_geometry(df.geo.nogeom) <- NULL

df.geo.nogeom %>%
  dplyr::mutate_if(is.character, as.factor) %>%
  summary()

df.geo.valid <- sf::st_make_valid(df.geo)

df.select.col.geo.valid <- sf::st_make_valid(df.select.col.geo)

## one geometry has invalid shape, taken out of the spatial join
df.join <- sf::st_join(df.geo.valid %>% dplyr::slice(1:329811, 329813:3293177),
                       df.select.col.geo.valid, join=sf::st_contains)

head(df.join)

df.join.nogeom <- df.join

sf::st_geometry(df.join.nogeom) <- NULL

df.join.nogeom.sel.col <- df.join.nogeom %>%
  dplyr::select(OBJECTID, HEIGHT, Shape_Area, AssessorID:EffectiveYearBuilt) %>%
  dplyr::filter(!is.na(AssessorID)) %>%
  tibble::as_tibble() %>%
  {.}

df.join.nogeom.sel.col %>%
  dplyr::distinct(AssessorID)

df.join.nogeom.sel.col %>%
  dplyr::distinct(OBJECTID)

## one polygon to many assessor point
df.join.nogeom.sel.col %>%
  dplyr::group_by(OBJECTID) %>%
  dplyr::filter(n()>1) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(OBJECTID)

## one assessor point to many polygon
df.join.nogeom.sel.col %>%
  dplyr::group_by(AssessorID) %>%
  dplyr::filter(n()>1) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(AssessorID)

join.unique <- df.join.nogeom.sel.col %>%
  dplyr::group_by(OBJECTID) %>%
  dplyr::filter(n() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(AssessorID) %>%
  dplyr::filter(n() == 1) %>%
  dplyr::ungroup() %>%
  {.}

## one footprint matched to multiple accessor data points
df.join.nogeom.sel.col.one.many <- df.join.nogeom.sel.col %>%
  dplyr::group_by(OBJECTID) %>%
  dplyr::filter(n()>1) %>%
  dplyr::ungroup()

df.join.nogeom.sel.col.one.many %>%
  distinct(OBJECTID)

df.join.nogeom.sel.col.one.many.clean <- df.join.nogeom.sel.col.one.many %>%
  dplyr::filter(EffectiveYearBuilt > 0) %>%
  dplyr::filter(SQFTmain > 0) %>%
  dplyr::filter(GeneralUseType != "(missing)") %>%
  {.}

df.one.many.nontype <- df.join.nogeom.sel.col.one.many.clean %>%
  dplyr::group_by(OBJECTID) %>%
  dplyr::summarise(HEIGHT = mean(HEIGHT),
                   EffectiveYearBuilt = max(EffectiveYearBuilt),
                   SQFTmain = mean(SQFTmain)) %>%
  dplyr::ungroup() %>%
  {.}

set.seed(0)

df.one.many.type.count <- df.join.nogeom.sel.col.one.many.clean %>%
  dplyr::group_by(OBJECTID, GeneralUseType, SpecificUseType) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  {.}

## select the most common type
df.one.many.type.count.majority <- df.one.many.type.count %>%
  dplyr::group_by(OBJECTID) %>%
  dplyr::filter(length(unique(count)) > 1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(OBJECTID, desc(count)) %>%
  dplyr::group_by(OBJECTID) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  {.}

## if all the same frequency, select a random one
df.one.many.type.count.random <- df.one.many.type.count %>%
  dplyr::group_by(OBJECTID) %>%
  dplyr::filter(length(unique(count)) == 1) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(OBJECTID) %>%
  dplyr::sample_n(size = 1) %>%
  dplyr::ungroup() %>%
  {.}

df.one.many.type <- df.one.many.type.count.majority %>%
  dplyr::bind_rows(df.one.many.type.count.random) %>%
  dplyr::select(-count)

df.one.many.clean <- df.one.many.nontype %>%
  dplyr::left_join(df.one.many.type, by="OBJECTID")

df.clean <- join.unique %>%
  dplyr::select(OBJECTID, HEIGHT, EffectiveYearBuilt, SQFTmain, GeneralUseType, SpecificUseType) %>%
  dplyr::bind_rows(df.one.many.clean) %>%
  dplyr::filter(EffectiveYearBuilt > 0) %>%
  dplyr::filter(SQFTmain > 0) %>%
  {.}

df.clean %>%
    distinct(OBJECTID)

df.clean %>%
  dplyr::mutate_at(vars(OBJECTID, GeneralUseType, SpecificUseType), as.factor) %>%
  summary() %>%
  {.}

df.clean %>%
    dplyr::select(OBJECTID, HEIGHT, EffectiveYearBuilt, SQFTmain) %>%
    tidyr::gather(variable, value, -OBJECTID) %>%
    na.omit() %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise_at(vars(value), tibble::lst(min, Q1=~quantile(., probs=0.25), median, mean, Q3=~quantile(., probs=0.75), max)) %>%
    dplyr::ungroup() %>%
    readr::write_csv("paper_table/LA_building_geojson_summary.csv")

df.clean %>%
  dplyr::group_by(GeneralUseType, SpecificUseType) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  readr::write_csv("intermediate_data/building_type_count.csv")

## join df.clean with building geometry or centroid

df.type.recode = readr::read_csv("input_data/building_type_recode.csv") %>%
  dplyr::mutate(`remap EP ref building` = ifelse(is.na(`remap EP ref building`), SpecificUseType, `remap EP ref building`)) %>%
  {.}

df.geo.compile <- df.geo %>%
  dplyr::select(OBJECTID) %>%
  dplyr::inner_join(df.clean, by="OBJECTID") %>%
  dplyr::left_join(df.type.recode, by=c("GeneralUseType", "SpecificUseType")) %>%
  {.}

df.geo.compile %>%
  sf::st_write("intermediate_data/compiled_LA_building.geojson")
