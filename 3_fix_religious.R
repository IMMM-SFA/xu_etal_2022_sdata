library("dplyr")

filepath = "intermediate_data/idf_add_sim_period_output_versionUpdate"

files <- list.files(filepath, pattern = "Religious*")

files

for (f in files) {
    print(f)
    lines = readLines(sprintf("%s/%s", filepath, f))
    redundant.objs.ends =
        sort(c(
            which(stringr::str_detect(lines, "linearpowerdensity;"))[[1]],
            which(stringr::str_detect(lines, "controlmode;"))[[1]],
            which(stringr::str_detect(lines, "volumetricflowrate;"))[[1]]))
    wrong.end.date.line = which(lines == "    12,                      !- End Day of Month")[[1]]
    wrong.day.of.week.line = which(lines == "    31,                      !- Day of Week for Start Day")[[1]]
    newlines = c(lines[1:(redundant.objs.ends[[1]] - 6)],
                 lines[(redundant.objs.ends[[1]] + 1):(redundant.objs.ends[[2]] - 6)],
                 lines[(redundant.objs.ends[[2]] + 1):(redundant.objs.ends[[3]] - 6)],
                 lines[(redundant.objs.ends[[3]] + 1):(wrong.end.date.line - 1)],
                 "    31,                      !- End Day of Month",
                 lines[(wrong.end.date.line + 1):(wrong.day.of.week.line - 1)],
                 "    Monday,                  !- Day of Week for Start Day",
                 lines[(wrong.day.of.week.line + 1):length(lines)]
                 )
    writeLines(newlines, sprintf("%s/_%s", filepath, f))
}
