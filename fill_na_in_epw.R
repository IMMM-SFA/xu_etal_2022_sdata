library("dplyr")
library("readr")

source("read_epw.R")

climate.folder = "input_data/annual_WRF/M02_EnergyPlus_Forcing_Historical_LowRes_ann_2018"
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

## [1] "119.epw" "78.epw"  "91.epw"  "97.epw"
epw.need.fill.na <- df.all.epw %>%
    dplyr::filter(if_any(everything(), ~ is.na(.))) %>%
    distinct(filename) %>%
    .$filename %>%
    {.}

filled.dfs <- df.all.epw %>%
    dplyr::filter(filename %in% epw.need.fill.na) %>%
    tidyr::fill(everything(), .direction = "downup") %>%
    dplyr::group_by(filename) %>%
    dplyr::group_split()

for (df in filled.dfs) {
    filename = df$filename[[1]]
    df %>%
        readr::write_csv(sprintf("%s/wrf_epw/%s", climate.folder, gsub("epw", "csv", filename)))
}

for (f in epw.need.fill.na) {
    print(f)
    err.epw = readLines(sprintf("%s/wrf_epw/%s", climate.folder, f))
    filled.csv = readLines(sprintf("%s/wrf_epw/%s", climate.folder, gsub("epw", "csv", f)))
    newlines = c(err.epw[1:8], filled.csv[2:length(filled.csv)])
    writeLines(newlines, sprintf("%s/wrf_epw/%s", climate.folder, f))
}

## remove temp csv files
for (f in epw.need.fill.na) {
    filled.csv = sprintf("%s/wrf_epw/%s", climate.folder, gsub("epw", "csv", f))
    file.remove(filled.csv)
}
