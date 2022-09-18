
files = list.files("intermediate_data/idf_add_sim_period_output", "*.idf")

head(files)

files = files[which(stringr::str_detect(files, "Family"))]

for (f in files) {
  print(f)
  lines = readLines(sprintf("intermediate_data/idf_add_sim_period_output/%s", f))
  newlines = sapply(lines, function(line) {
      if (stringr::str_detect(line, "/Users/sky/Sites/CBES/cbes_api/test/files/")) {
          newline = gsub("/Users/sky/Sites/CBES/cbes_api/test/files/",
                         "/Users/yujiex/Dropbox/workLBNL/EESA/meta_repo/xu_etal_2022_tbd/input_data/LA Prototypes/res_schedule/", line, fixed = TRUE)
      } else {
          newline = line
      }
      newline
  })
  writeLines(newlines, sprintf("intermediate_data/idf_add_sim_period_output/%s", f))
}
