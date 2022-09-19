library("dplyr")

filepath = "intermediate_data/idf_add_sim_period_output_versionUpdate"

allfiles <- list.files(filepath, "*.idf")

## year = 2018
year = 2016

## check runperiods
for (f in allfiles) {
    print(sprintf("----%s----", f))
    lines = readLines(sprintf("%s/%s", filepath, f))
    runperiod.start = which(stringr::str_detect(lines, "RunPeriod,"))[[1]]
    if (length(runperiod.start) > 1) {
        print("multiple runperiod!!")
    } else {
        print(lines[runperiod.start:(runperiod.start + 15)])
    }
    print("------------------------------------------------")
}

## assume annual simulation
start.sim.day = paste0(as.character(lubridate::wday(as.POSIXct(sprintf("%d-01-01", year)), label = TRUE)), "day")

start.sim.day

for (f in allfiles) {
    lines = readLines(sprintf("%s/%s", filepath, f))
    ## checked, no file has multiple run periods
    runperiod.start = which(stringr::str_detect(lines, "RunPeriod,"))[[1]]
    begin.year.line.idx = which(stringr::str_detect(lines[runperiod.start:(runperiod.start + 20)], "!- Begin Year"))
    end.year.line.idx = which(stringr::str_detect(lines[runperiod.start:(runperiod.start + 20)], "!- End Year"))
    day.of.week.line.idx = which(stringr::str_detect(lines[runperiod.start:(runperiod.start + 20)], "!- Day of Week for Start Day"))
    runperiod.start + begin.year.line.idx - 1
    lines[[runperiod.start + begin.year.line.idx - 1]] = sprintf("    %d,                    !- Begin Year",
                                                                 year)
    lines[[runperiod.start + end.year.line.idx - 1]] = sprintf("    %d,                    !- End Year",
                                                               year)
    lines[[runperiod.start + day.of.week.line.idx - 1]] = sprintf("    %s,                  !- Day of Week for Start Day",
                                                                  start.sim.day)
    writeLines(lines, sprintf("intermediate_data/idf_to_sim_%d/%s", year, f))
}
