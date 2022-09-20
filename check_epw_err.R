library("dplyr")
library("readr")

source("read_epw.R")

## for july data
## climate.folder = "input_data/M02_EnergyPlus_Forcing_Historical_LowRes"
## for 2018 annual
climate.folder = "input_data/annual_WRF/M02_EnergyPlus_Forcing_Historical_LowRes_ann_2018"

f.err = "78.epw"
f.good = "79.epw"

result <- lapply(c(f.err, f.good), function(f) {
  print(f)
  ## skip 8 non-data rows
  df = read.epw(sprintf("%s/wrf_epw/%s", climate.folder, f)) %>%
      dplyr::mutate(filename = f) %>%
      {.}
})

result[[1]] %>%
    ## dplyr::filter(is.na(DewPoint.C)) %>%
    summary() %>%
    {.}
## 78 epw in 2018 has a NA due point.

files = list.files(sprintf("%s/wrf_epw", climate.folder), pattern="*.epw")

df.all.epw <- lapply(files, function(f){
    print(f)
    df = read.epw(sprintf("%s/wrf_epw/%s", climate.folder, f)) %>%
        dplyr::mutate(filename = f) %>%
        {.}
    df
}) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(cell.id = as.numeric(gsub(".epw", "", filename))) %>%
    {.}

df.all.epw %>%
    summary()

df.all.epw %>%
    dplyr::filter(is.na(DewPoint.C)) %>%
    distinct(filename) %>%
    {.}
