## read .epw files
## filepath: path to file
read.epw <- function(filepath) {
  col.names = c("year","month","day","hour","minute",
                "Datasource","DryBulb.C","DewPoint.C",
                "RelHum.percent","AtmosPressure.Pa","ExtHorzRad.Wh/m2",
                "ExtDirRad.Wh/m2","HorzIRSky.Wh/m2","GloHorzRad.Wh/m2",
                "DirNormRad.Wh/m2","DifHorzRad.Wh/m2","GloHorzIllum.lux",
                "DirNormIllum.lux","DifHorzIllum.lux", "ZenLum.Cd/m2",
                "WindDir.deg","WindSpd.m/s","TotSkyCvr.0.1","OpaqSkyCvr.0.1",
                "Visibility.km","CeilingHgt.m","PresWeathObs","PresWeathCodes",
                "PrecipWtr.mm","AerosolOptDepth.0.001","SnowDepth.cm",
                "DaysLastSnow","Albedo.0.01","Rain.mm","RainQuantity.hr")

  df = readr::read_csv(filepath, skip=8,
                      col_names=col.names,
                      col_types=readr::cols(year=col_integer(),
                                            month=col_integer(),
                                            day=col_integer(),
                                            hour=col_integer(),
                                            minute=col_integer())) %>%
      tibble::as_tibble() %>%
      {.}
  df
}
