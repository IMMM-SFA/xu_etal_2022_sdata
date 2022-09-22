## copied in from CityBES/LA/data-raw/
## modified filepath to suit the current folder setting, added some comments

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

if (!file.exists("intermediate_data/Assessor_point.geojson")) {
    df.select.col.geo %>%
        sf::st_write("intermediate_data/Assessor_point.geojson")
}
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

unmatched.round.1 <- df.join.nogeom %>%
    dplyr::select(OBJECTID, AssessorID) %>%
    dplyr::filter(is.na(AssessorID)) %>%
    dplyr::distinct(OBJECTID) %>%
    tibble::as_tibble() %>%
    {.}

df.geo.unmatched = df.geo %>%
    dplyr::inner_join(unmatched.round.1, by="OBJECTID") %>%
    {.}

df.geo.unmatched.6423 <- sf::st_transform(df.geo.unmatched, crs=6423)

df.geo.unmatched.6423 %>%
    dplyr::select(OBJECTID) %>%
    sf::st_write("intermediate_data/not_matched_bld_footprint.geojson")

## import not_matched_bld_footprint.geojson to qgis, generate the buffered
## polygon: not_matched_bld_footprint_buffer4m.geojson

df.geo.unmatched.6423.buffer <- sf::st_read("intermediate_data/not_matched_bld_footprint_buffer4m.geojson")

df.geo.unmatched.6423.buffer.valid = sf::st_make_valid(df.geo.unmatched.6423.buffer)

matched.assessor.round.1 <- df.join.nogeom %>%
    distinct(AssessorID)

## unmatched assessor point
df.select.col.geo.valid.not.matched = df.select.col.geo.valid %>%
    dplyr::filter(!(AssessorID %in% matched.assessor.round.1$AssessorID))

df.select.col.geo.valid.not.matched.6423 <- sf::st_transform(df.select.col.geo.valid.not.matched, crs=6423)

## one geometry has invalid shape, taken out of the spatial join
df.join.round.2 <- sf::st_join(df.geo.unmatched.6423.buffer.valid,
                               df.select.col.geo.valid.not.matched.6423, join=sf::st_contains)

df.join.round.2.no.geom <- df.join.round.2

sf::st_geometry(df.join.round.2.no.geom) <- NULL

df.join.round.2.no.geom %>%
    tibble::as_tibble() %>%
    dplyr::filter(!is.na(AssessorID)) %>%
    {.}

## matched assessor point after buffer
matched.assessor.round.2 <- df.join.round.2.no.geom %>%
    tibble::as_tibble() %>%
    dplyr::filter(!is.na(AssessorID)) %>%
    distinct(AssessorID)

## unmatched polygon after buffering
unmatched.round.2 <- df.join.round.2.no.geom %>%
    tibble::as_tibble() %>%
    dplyr::select(OBJECTID, AssessorID) %>%
    dplyr::filter(is.na(AssessorID)) %>%
    dplyr::distinct(OBJECTID) %>%
    {.}

df.geo.unmatched.round.2 = df.geo.unmatched.6423.buffer.valid %>%
    dplyr::inner_join(unmatched.round.2, by="OBJECTID") %>%
    {.}

df.geo.unmatched.round.2 %>%
    dplyr::select(OBJECTID) %>%
    sf::st_write("intermediate_data/not_matched_bld_footprint_after_buffer.geojson")

df.select.col.geo.valid.not.matched.2 = df.select.col.geo.valid %>%
    dplyr::filter(!(AssessorID %in% matched.assessor.round.1$AssessorID)) %>%
    dplyr::filter(!(AssessorID %in% matched.assessor.round.2$AssessorID))

assessor.unmatched.round.1.and.2 = df.select.col %>%
    dplyr::filter(!(AssessorID %in% matched.assessor.round.1$AssessorID)) %>%
    dplyr::filter(!(AssessorID %in% matched.assessor.round.2$AssessorID))

df.select.col.geo.valid.not.matched.2.6423 <- sf::st_transform(df.select.col.geo.valid.not.matched.2, crs=6423)

df.select.col.geo.valid.not.matched.2.6423 %>%
    dplyr::select(AssessorID) %>%
    sf::st_write("intermediate_data/not_matched_assessor_data_after_buffer.geojson")

df.join.nogeom.sel.col <- df.join.nogeom %>%
  tibble::as_tibble() %>%
  dplyr::filter(!is.na(AssessorID)) %>%
  dplyr::select(OBJECTID, HEIGHT, Shape_Area, AssessorID:EffectiveYearBuilt) %>%
  {.}

polygon.metadata <- df.geo.nogeom %>%
    tibble::as_tibble() %>%
    select(OBJECTID, HEIGHT, Shape_Area) %>%
    {.}

## 468,936 polygon joined
df.join.round.2.no.geom.sel.col <- df.join.round.2.no.geom %>%
    tibble::as_tibble() %>%
    dplyr::filter(!is.na(AssessorID)) %>%
    dplyr::left_join(polygon.metadata, by="OBJECTID") %>%
    dplyr::select(OBJECTID, HEIGHT, Shape_Area, AssessorID:EffectiveYearBuilt) %>%
    {.}

df.join.nogeom.sel.col %>%
  dplyr::distinct(AssessorID)

df.join.nogeom.sel.col %>%
  dplyr::distinct(OBJECTID)

df.join.round.2.no.geom.sel.col %>%
    dplyr::distinct(AssessorID)

df.join.round.2.no.geom.sel.col %>%
    dplyr::distinct(OBJECTID)

df.join.two.rounds <- df.join.nogeom.sel.col %>%
    dplyr::bind_rows(df.join.round.2.no.geom.sel.col)

## one polygon to many assessor point
df.join.two.rounds %>%
  dplyr::group_by(OBJECTID) %>%
  dplyr::filter(n()>1) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(OBJECTID)

## one assessor point to many polygon
df.join.two.rounds %>%
  dplyr::group_by(AssessorID) %>%
  dplyr::filter(n()>1) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(AssessorID)

join.unique <- df.join.two.rounds %>%
  dplyr::group_by(OBJECTID) %>%
  dplyr::filter(n() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(AssessorID) %>%
  dplyr::filter(n() == 1) %>%
  dplyr::ungroup() %>%
  {.}

## one footprint matched to multiple accessor data points
df.join.nogeom.sel.col.one.many <- df.join.two.rounds %>%
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
                   SQFTmain = sum(SQFTmain)) %>%
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

df.many.one.join <- df.join.two.rounds %>%
  dplyr::group_by(OBJECTID) %>%
  dplyr::filter(n() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(AssessorID) %>%
  dplyr::filter(n() > 1) %>%
  dplyr::ungroup() %>%
  {.}

df.many.one.clean <- df.many.one.join %>%
  dplyr::group_by(AssessorID) %>%
  dplyr::sample_n(size = 1) %>%
  dplyr::ungroup() %>%
  {.}

df.clean <- join.unique %>%
  dplyr::select(OBJECTID, HEIGHT, EffectiveYearBuilt, SQFTmain, GeneralUseType, SpecificUseType) %>%
  dplyr::bind_rows(df.one.many.clean) %>%
  dplyr::bind_rows(df.many.one.clean) %>%
  dplyr::filter(EffectiveYearBuilt > 0) %>%
  dplyr::filter(SQFTmain > 0) %>%
  {.}

df.clean %>%
  nrow()

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

type.count <- df.clean %>%
  dplyr::group_by(GeneralUseType, SpecificUseType) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  {.}

type.count %>%
  readr::write_csv("intermediate_data/building_type_count.csv")

## update counts in type recode
df.type.recode <- readr::read_csv("input_data/building_type_recode.csv") %>%
  select(-count) %>%
  dplyr::left_join(type.count, by=c("GeneralUseType", "SpecificUseType")) %>%
  tidyr::replace_na(list(count = 0)) %>%
  {.}

df.type.recode %>%
  readr::write_csv("input_data/building_type_recode.csv")

df.geo.compile <- df.geo %>%
  dplyr::select(OBJECTID) %>%
  dplyr::inner_join(df.clean, by="OBJECTID") %>%
  dplyr::left_join(df.type.recode, by=c("GeneralUseType", "SpecificUseType")) %>%
  {.}

df.geo.compile %>%
  sf::st_write("intermediate_data/compiled_LA_building.geojson")

## update the type recoding if needed
if (FALSE) {
  df.type.recode = readr::read_csv("input_data/building_type_recode.csv") %>%
      dplyr::select(-count) %>%
      {.}

  df.geo.compile <- sf::st_read("intermediate_data/compiled_LA_building.geojson")

  file.remove("intermediate_data/compiled_LA_building.geojson")

  df.geo.compile %>%
      dplyr::select(-starts_with("remap")) %>%
      dplyr::left_join(df.type.recode, by=c("GeneralUseType", "SpecificUseType")) %>%
      sf::st_write("intermediate_data/compiled_LA_building.geojson")
}
