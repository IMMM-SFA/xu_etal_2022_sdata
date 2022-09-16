## Modify design day info
library("dplyr")

files = list.files("input_data/LA Prototypes/Com (OS_Standards)/", pattern = "*.idf")

temp <- lapply(files, function(f) {
  from.file = sprintf("input_data/LA Prototypes/Com (OS_Standards)/%s", f)
  to.file = sprintf("intermediate_data/idf_to_modify/%s", f)
  print(from.file)
  print(to.file)
  file.copy(from=from.file, to=to.file)
})

from.file <- "input_data/LA Prototypes/Com need mod/16_Religious Worship/post-1980/3B.idf"
to.file <- "intermediate_data/idf_to_modify/Religious-post-1980-3B.idf"
file.copy(from=from.file, to=to.file)
from.file <- "input_data/LA Prototypes/Com need mod/16_Religious Worship/pre-1980/3B.idf"
to.file <- "intermediate_data/idf_to_modify/Religious-pre-1980-3B.idf"
file.copy(from=from.file, to=to.file)

from.file <- "input_data/LA Prototypes/Res (CBES)/bldg_13_vin_1_cz_6/model.idf"
to.file <- "intermediate_data/idf_to_modify/MultiFamily-pre-1980.idf"
file.copy(from=from.file, to=to.file)
from.file <- "input_data/LA Prototypes/Res (CBES)/bldg_13_vin_5_cz_6/model.idf"
to.file <- "intermediate_data/idf_to_modify/MultiFamily-2004.idf"
file.copy(from=from.file, to=to.file)
from.file <- "input_data/LA Prototypes/Res (CBES)/bldg_13_vin_8_cz_6/model.idf"
to.file <- "intermediate_data/idf_to_modify/MultiFamily-2013.idf"
file.copy(from=from.file, to=to.file)

from.file <- "input_data/LA Prototypes/NursingHome mod sched autosize/NursingHome_baseline-SanFrancisco.idf"
to.file <- "intermediate_data/idf_to_modify/NursingHome_baseline-SanFrancisco.idf"
file.copy(from=from.file, to=to.file)

la.design.day <- readLines("input_data/la-design-day/USA_CA_Los.Angeles.Intl.AP.722950_TMY3.ddy")

location.data <- la.design.day[6:11]

ddy.end.of.objects <- which(stringr::str_detect(la.design.day, ";.*!"))
summer.design.day <- la.design.day[203:227]
winter.design.day <- la.design.day[27:52]

## check version
files = list.files(path="intermediate_data/idf_to_modify", pattern="*.idf")

files = files[files != "NursingHome_baseline-SanFrancisco.idf"]

df.version.num <- lapply(files, function(f) {
  lines <- readLines(sprintf("intermediate_data/idf_to_modify/%s", f))
  version.num = gsub(pattern = "[a-zA-z ;!-]*", replacement = "", x=lines[[3]])
  end.of.objects <- which(stringr::str_detect(lines, ";.*!"))
  location.line.start <- which(lines == "Site:Location,")
  location.line.end <- end.of.objects[which(end.of.objects > location.line.start)[[1]]]
  winter.design.day.start <- which(stringr::str_detect(lines, "Ann Htg 99.6% Condns "))
  winter.design.day.end <- end.of.objects[which(end.of.objects > winter.design.day.start)[[1]]]
  ddy.winter.design.day.start <- which(stringr::str_detect(la.design.day, winter.design.day.setting))
  ddy.winter.design.day.end <- ddy.end.of.objects[which(ddy.end.of.objects > ddy.winter.design.day.start)[[1]]]
  winter.design.day.setting <- stringr::str_extract(lines[winter.design.day.start],
                                                    "Ann Htg 99.6% Condns.*\\,")
  winter.design.day <- la.design.day[ddy.winter.design.day.start:ddy.winter.design.day.end]
  summer.design.day.start <- which(stringr::str_detect(lines, "Ann Clg .4% Condns "))
  summer.design.day.end <- end.of.objects[which(end.of.objects > summer.design.day.start)[[1]]]
  summer.design.day.setting <- stringr::str_extract(lines[summer.design.day.start],
                                                   "Ann Clg .4% Condns.*\\,")
  ddy.summer.design.day.start <- which(stringr::str_detect(la.design.day, summer.design.day.setting))
  ddy.summer.design.day.end <- ddy.end.of.objects[which(ddy.end.of.objects > ddy.summer.design.day.start)[[1]]]
  summer.design.day <- la.design.day[ddy.summer.design.day.start:ddy.summer.design.day.end]
  if (winter.design.day.start < summer.design.day.start) {
    newlines <- c(lines[1:(location.line.start - 1)],
                  location.data,
                  lines[(location.line.end + 1):(winter.design.day.start - 1)],
                  winter.design.day,
                  lines[(winter.design.day.end + 1):(summer.design.day.start - 1)],
                  summer.design.day,
                  lines[(summer.design.day.end + 1):length(lines)])
  } else {
    newlines <- c(lines[1:(location.line.start - 1)],
                  location.data,
                  lines[(location.line.end + 1):(summer.design.day.start - 1)],
                  summer.design.day,
                  lines[(summer.design.day.end + 1):(winter.design.day.start - 1)],
                  winter.design.day,
                  lines[(winter.design.day.end + 1):length(lines)])
  }
  writeLines(newlines, sprintf("intermediate_data/idf_change_design_day/%s", f))
  print(sprintf("%s %s | %s |", f, winter.design.day.setting, summer.design.day.setting))
  tibble::tibble(filename=f, version.num=version.num)
})

df.version.num %>%
  dplyr::bind_rows() %>%
  readr::write_csv("intermediate_data/idf_version_num.csv")

## nursing home
f = "NursingHome_baseline-SanFrancisco.idf"
lines <- readLines(sprintf("intermediate_data/idf_to_modify/%s", f))
end.of.objects <- which(stringr::str_detect(lines, ";.*!"))
location.line.start <- which(lines == "Site:Location,")
location.line.end <- end.of.objects[which(end.of.objects > location.line.start)[[1]]]
design.day.start <- 102
design.day.end <- 616
ref.lines <- readLines("intermediate_data/idf_change_design_day/Hospital-90.1-2004-ASHRAE 169-2013-3B.idf")
ref.end.of.objects <- which(stringr::str_detect(ref.lines, ";.*!"))
ref.winter.design.day.start <- which(stringr::str_detect(ref.lines, "Ann Htg 99.6% Condns "))
ref.winter.design.day.end <- ref.end.of.objects[which(ref.end.of.objects > ref.winter.design.day.start)[[1]]]
ref.summer.design.day.start <- which(stringr::str_detect(ref.lines, "Ann Clg .4% Condns "))
ref.summer.design.day.end <- ref.end.of.objects[which(ref.end.of.objects > ref.summer.design.day.start)[[1]]]
winter.design.day = ref.lines[(ref.winter.design.day.start - 1):ref.winter.design.day.end]
summer.design.day = ref.lines[(ref.summer.design.day.start - 1):ref.summer.design.day.end]
newlines <- c(lines[1:(location.line.start - 1)],
              location.data,
              lines[(location.line.end + 1):(design.day.start - 1)],
              winter.design.day,
              summer.design.day,
              lines[(design.day.end + 1):length(lines)])
writeLines(newlines, gsub("SanFrancisco", "LA", sprintf("intermediate_data/idf_change_design_day/%s", f)))
