get.param.reading.energy.atlas <- function(energy.type) {
  if (energy.type == "electricity") {
      suf = "kwh"
      masked.value = "masked"
      multiplier = 1e-6
      unit = "Gwh"
  } else if (energy.type == "gas") {
      suf = "therms"
      masked.value = c(-7777, -8888, -9999)
      multiplier = 1e-6
      unit = "Mtherm"
  } else if (energy.type == "total") {
      suf = "btu"
      masked.value = "masked"
      multiplier = 1e-12
      unit = "TBtu"
  }
  list(suf, masked.value, multiplier, unit)
}

read.energy.atlas <- function(energy.type, keep.main.usetype=FALSE,
                              remove.masked.data=FALSE, do.unit.conversion=FALSE) {
    usetypes = c("commercial", "industrial", "institutional", "res_total")
    results = get.param.reading.energy.atlas(energy.type)
    suf = results[[1]]
    masked.value = results[[2]]
    multiplier = results[[3]]
    unit.measure = results[[4]]
    energy.atlas.data <- readr::read_csv(sprintf("input_data/EnergyAtlas/usage_bld_%s.csv", suf)) %>%
        tidyr::separate(geo_id, into=c("id.type", "id.num")) %>%
        dplyr::select(id.type, id.num, name, sqft, usage, usetype, year) %>%
        dplyr::mutate(energy.type = energy.type) %>%
        {.}
    if (keep.main.usetype) {
        energy.atlas.data <- energy.atlas.data %>%
            dplyr::filter(usetype %in% usetypes) %>%
            {.}
    }
    if (remove.masked.data) {
        energy.atlas.data <- energy.atlas.data %>%
            dplyr::filter(!(usage %in% masked.value)) %>%
            {.}
    }
    if (do.unit.conversion) {
        energy.atlas.data <- energy.atlas.data %>%
            dplyr::mutate(usage = as.numeric(usage) * multiplier) %>%
            dplyr::mutate(unit = unit.measure) %>%
            {.}
    }
    energy.atlas.data
}
