## Generates the epw_idf_to_simulate.csv that lists the epw-idf combination to be simulated
library("dplyr")

df = readr::read_csv("intermediate_data/summary/building_type_vintage_count_per_grid.csv") %>%
  dplyr::select(-building.count)

df.idf.lookup = readr::read_csv("input_data/type_vintage_to_idf_mapping.csv") %>%
  dplyr::select(-building.count) %>%
  ## do not differ small large restaurant and retail
  dplyr::mutate_at(vars(building.type), recode, "large retail"="retail", "small retail"="retail",
                    "large restaurant"="full service restaurant", "small restaurant"="full service restaurant") %>%
  na.omit()

df %>%
  ## to exclude manufactured home
  dplyr::inner_join(df.idf.lookup, by=c("building.type", "vintage")) %>%
  dplyr::distinct(id, idf.name) %>%
  readr::write_csv("intermediate_data/epw_idf_to_simulate.csv")

df %>%
  dplyr::inner_join(df.idf.lookup, by=c("building.type", "vintage")) %>%
  distinct(idf.name) %>%
  print(n=Inf)

df %>%
  dplyr::distinct(building.type) %>%
  print(n=Inf)

df
