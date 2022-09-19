library("dplyr")

filepath = "intermediate_data/idf_add_sim_period_output_versionUpdate"
filename = "NursingHome_baseline-LA.idf"

lines <- readLines(sprintf("%s/%s", filepath, filename))

idx = which(lines == "  Chiller:Electric:EIR,")
lines[[idx + 24]] = "    0.0,                     !- Design Heat Recovery Water Flow Rate {m3/s}"
writeLines(lines, sprintf("%s/_%s", filepath, filename))
