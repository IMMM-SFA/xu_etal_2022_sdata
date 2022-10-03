compile.sim.result <- function(result.path, df.prototype.area) {
  folders <- list.files(result.path)
  result <- lapply(folders, function(folder) {
    df <- readr::read_csv(sprintf("%s/%s/eplusout.csv", result.path, folder)) %>%
      dplyr::mutate(energy.elec = `Electricity:Facility [J](Hourly)`) %>%
      dplyr::mutate(energy.overall = energy.elec) %>%
      {.}
    if ("NaturalGas:Facility [J](Hourly)" %in% names(df)) {
      df <- df %>%
        dplyr::mutate(energy.gas = `NaturalGas:Facility [J](Hourly)`) %>%
        dplyr::mutate(energy.overall = energy.elec + energy.gas)
    }
    df %>%
      dplyr::select(starts_with("energy")) %>%
      dplyr::summarise_if(is.numeric, sum) %>%
      dplyr::mutate(idf.kw = folder) %>%
      {.}
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(energy.GJ = energy.overall * 1e-9) %>%
    dplyr::select(idf.kw, energy.GJ) %>%
    dplyr::left_join(df.prototype.area, by="idf.kw") %>%
    dplyr::mutate(EUI.GJ.per.m2 = energy.GJ / prototype.m2) %>%
    tidyr::separate(idf.kw, into=letters[1:6], sep="\\-") %>%
    dplyr::rename(type = a) %>%
    {.}
  result
}

compile.sim.result.elec.gas <- function(result.path, df.prototype.area) {
  folders <- list.files(result.path)
  result <- lapply(folders, function(folder) {
    df <- readr::read_csv(sprintf("%s/%s/eplusout.csv", result.path, folder)) %>%
      dplyr::mutate(energy.elec = `Electricity:Facility [J](Hourly)`) %>%
      dplyr::mutate(energy.overall = energy.elec) %>%
      {.}
    if ("NaturalGas:Facility [J](Hourly)" %in% names(df)) {
      df <- df %>%
        dplyr::mutate(energy.gas = `NaturalGas:Facility [J](Hourly)`) %>%
        dplyr::mutate(energy.overall = energy.elec + energy.gas)
    }
    df %>%
      dplyr::select(starts_with("energy")) %>%
      dplyr::summarise_if(is.numeric, sum) %>%
      dplyr::mutate(idf.kw = folder) %>%
      {.}
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(elec = energy.elec * 1e-9) %>%
    dplyr::mutate(gas = energy.gas * 1e-9) %>%
    dplyr::select(idf.kw, elec, gas) %>%
    dplyr::left_join(df.prototype.area, by="idf.kw") %>%
    tidyr::gather(variable, energy.GJ, elec:gas) %>%
    dplyr::mutate(EUI.GJ.per.m2 = energy.GJ / prototype.m2) %>%
    tidyr::separate(idf.kw, into=letters[1:6], sep="\\-") %>%
    dplyr::rename(type = a) %>%
    {.}
  result
}
