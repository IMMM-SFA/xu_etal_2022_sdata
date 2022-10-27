library("dplyr")

df = readr::read_csv("intermediate_data/epw_idf_to_simulate.csv")

## df %>%
##   distinct(idf.name) %>%
##   dplyr::arrange(idf.name) %>%
##   readr::write_csv("prototype_bldg_area.csv")

df.area.prototype = readr::read_csv("input_data/prototype_bldg_area.csv") %>%
  dplyr::mutate(idf.kw = gsub(".idf", "", idf.name, fixed=TRUE)) %>%
  dplyr::mutate(idf.kw = gsub(".", "_", idf.kw, fixed=TRUE)) %>%
  dplyr::select(-idf.name) %>%
  {.}

dirs <- df %>%
  dplyr::mutate(dirname = paste0(idf.name, "____", id)) %>%
  dplyr::mutate(dirname = gsub(".idf", "", dirname, fixed = TRUE)) %>%
  dplyr::mutate(dirname = gsub(".", "_", dirname, fixed = TRUE)) %>%
  .$dirname

## data.res = "July"
data.res = "annual"
suf = "_ann"
pref = "annual"

## result.dir = "result_ann_0520"
## result.csv.dir = sprintf("sim_result%s_0520_csv/%s.csv", suf, dirname)
## annual results
year = 2018
result.dir = sprintf("output_data/EP_output/result%s_WRF_%d", suf, year)
result.csv.dir = sprintf("output_data/EP_output_csv/sim_result%s_WRF_%d_csv", suf, year)

## year = 2016
## result.dir = sprintf("intermediate_data/EP_output/result%s_WRF_%d", suf, year)
## result.csv.dir = sprintf("intermediate_data/EP_output_csv/sim_result%s_WRF_%d_csv", suf, year)

length(dirs)

for (dirname in dirs) {
  print(dirname)
  output.name = sprintf("%s/%s/eplusout.csv", result.dir, dirname)
  ## output.name = sprintf("result_ann/%s/eplusout.csv", dirname)
  if (file.exists(output.name)) {
    print(sprintf("copy to %s/%s.csv",  result.csv.dir, dirname))
    print(file.copy(output.name, sprintf("%s/%s.csv", result.csv.dir, dirname)))
  }
}

files = list.files(path=result.csv.dir, pattern = "*.csv")

files.kw = gsub(".csv", "", files)

setdiff(dirs, files.kw)
## all files are processed

check.missing.var = FALSE
if (check.missing.var) {
  ## colname = "Environment:Site Total Zone Exhaust Air Heat Loss [J](Hourly)"
  colname = "Environment:Site Total Surface Heat Emission to Air [J](Hourly)"
  with.missing.var <- lapply(seq_along(files), function(i) {
      f = files[i]
      ## print(i)
      df = readr::read_csv(sprintf("%s/%s", result.csv.dir, f), col_types = readr::cols()) %>%
          {.}
      if (!(colname %in% names(df))) {
          return(f)
      }
  })
}

unlist(with.missing.var)

## read simulation results for annual
result.ann <- lapply(files, function(f) {
  tokens = unlist(stringr::str_split(f, pattern = "____"))
  idf.kw = tokens[[1]]
  epw.id = gsub(".csv", "", tokens[[2]])
  df = readr::read_csv(sprintf("%s/%s", result.csv.dir, f), col_types = readr::cols()) %>%
    ## df = readr::read_csv(sprintf("sim_result_ann_csv/%s", f), col_types = readr::cols()) %>%
    dplyr::mutate(emission.exfiltration = `Environment:Site Total Zone Exfiltration Heat Loss [J](Hourly)`,
                  emission.exhaust = `Environment:Site Total Zone Exhaust Air Heat Loss [J](Hourly)`,
                  emission.ref = `SimHVAC:Air System Relief Air Total Heat Loss Energy [J](Hourly)`,
                  emission.rej = `SimHVAC:HVAC System Total Heat Rejection Energy [J](Hourly)`,
                  emission.surf = `Environment:Site Total Surface Heat Emission to Air [J](Hourly)`,
                  emission.overall = emission.exfiltration + emission.exhaust + emission.ref + emission.rej + emission.surf) %>%
    dplyr::mutate(energy.elec = `Electricity:Facility [J](Hourly)`) %>%
    dplyr::mutate(energy.overall = energy.elec) %>%
    dplyr::mutate(idf.kw = idf.kw, epw.id = epw.id) %>%
    {.}
  if ("NaturalGas:Facility [J](Hourly)" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(energy.gas = `NaturalGas:Facility [J](Hourly)`) %>%
      dplyr::mutate(energy.overall = energy.elec + energy.gas)
  }
  if (nrow(df) != 8760) {
    print(sprintf("%s: %d", f, nrow(df)))
  }
  df <- df %>%
    dplyr::select(`Date/Time`, idf.kw, epw.id, starts_with("emission."), starts_with("energy"))
  df
}) %>%
  dplyr::bind_rows()

result.ann %>%
    names()

result.ann %>%
  dplyr::distinct(idf.kw) %>%
  readr::write_csv("intermediate_data/idf_kw.csv")

result.ann %>%
    head()

result.ann %>%
  readr::write_csv(sprintf("intermediate_data/%s_sim_result_by_idf_epw_%d.csv", pref, year))
## readr::write_csv("annual_sim_result_by_idf_epw.csv")

result.ann %>%
    tidyr::separate(`Date/Time`, into = c("month", "suffix"), sep = "/") %>%
    dplyr::select(-suffix) %>%
    dplyr::group_by(idf.kw, epw.id, month) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::ungroup() %>%
    readr::write_csv(sprintf("intermediate_data/monthly_total_result_%d.csv", year))

result.ann %>%
    dplyr::group_by(idf.kw, epw.id) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::ungroup() %>%
    readr::write_csv(sprintf("intermediate_data/annual_total_result_%d.csv", year))

result.ann %>%
    tidyr::separate(`Date/Time`, into=c("month", "suf"), sep="/") %>%
    dplyr::group_by(month, idf.kw, epw.id) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::ungroup() %>%
    readr::write_csv(sprintf("intermediate_data/monthly_total_result_%d.csv", year))
