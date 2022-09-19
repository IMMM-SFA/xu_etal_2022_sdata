library("dplyr")

files <- list.files("intermediate_data/idf_add_sim_period_output", pattern = "Warehouse-*")

for (f in files) {
    file.copy(sprintf("intermediate_data/idf_add_sim_period_output_versionUpdate/%s", f),
              sprintf("intermediate_data/warehouse_model_modify/%s", f))
}

file.copy("input_data/M02_EnergyPlus_Forcing_Historical_LowRes/USA_CA_Los.Angeles.Intl.AP.722950_TMY3.epw",
          "intermediate_data/warehouse_model_modify/USA_CA_Los.Angeles.Intl.AP.722950_TMY3.epw")

adjust.elec.gas.equipment <- function(input.file, output.file, elec.equip.1,
                                      elec.equip.2, gas.equip) {
  lines <- readLines(sprintf("intermediate_data/warehouse_model_modify/%s", input.file))
  electric.equipment.line.starts = which(stringr::str_detect(lines, "ElectricEquipment,"))
  gas.lines = c("GasEquipment,",
                "  Warehouse Bulk Gas Equip,",
                "  Warehouse Bulk,",
                "  Warehouse Bldg Equip,",
                "  Watts/Area,",
                "  ,",
                sprintf("  %s,", gas.equip),
                "  ,",
                "  0,",
                "  0.5,",
                "  0,",
                "  ,",
                "  General;")
  newlines = c(lines[1:electric.equipment.line.starts[[1]] - 1],
              gas.lines,
              lines[electric.equipment.line.starts[[1]]:(electric.equipment.line.starts[[1]] + 5)],
              sprintf("  %s,", elec.equip.1),
              lines[(electric.equipment.line.starts[[1]] + 7):(electric.equipment.line.starts[[2]] + 5)],
              sprintf("  %s,", elec.equip.2),
              lines[(electric.equipment.line.starts[[2]] + 7):length(lines)]
              )
  writeLines(newlines, sprintf("intermediate_data/warehouse_model_modify/%s", output.file))
}

adjust.elec.gas.equipment("Warehouse-90.1-2013-ASHRAE 169-2013-3B.idf",
                          "LightManufacturing-90.1-2013-ASHRAE 169-2013-3B.idf",
                          "69.0493", "204.41", "97.114")

adjust.elec.gas.equipment("Warehouse-90.1-2004-ASHRAE 169-2013-3B.idf",
                          "LightManufacturing-90.1-2004-ASHRAE 169-2013-3B.idf",
                          "111.1202", "350.41", "97.114")

adjust.elec.gas.equipment("Warehouse-DOE Ref Pre-1980-ASHRAE 169-2013-3B.idf",
                          "LightManufacturing-DOE Ref Pre-1980-ASHRAE 169-2013-3B.idf",
                          "110.29", "367.41", "96.9924")

adjust.elec.gas.equipment("Warehouse-90.1-2013-ASHRAE 169-2013-3B.idf",
                          "HeavyManufacturing-90.1-2013-ASHRAE 169-2013-3B.idf",
                          "247.8168", "689.00", "247.6299")

adjust.elec.gas.equipment("Warehouse-90.1-2004-ASHRAE 169-2013-3B.idf",
                          "HeavyManufacturing-90.1-2004-ASHRAE 169-2013-3B.idf",
                          "401.291", "1200.00", "247.6298")

adjust.elec.gas.equipment("Warehouse-DOE Ref Pre-1980-ASHRAE 169-2013-3B.idf",
                          "HeavyManufacturing-DOE Ref Pre-1980-ASHRAE 169-2013-3B.idf",
                          "394.6291", "1100.00", "247.619")

## The electricity and gas consumption of the adjusted buildings are
##       Elec GJ	Gas GJ	vintage
## light	4447.27	4195.23	2013
## light	7393.87	4195.23	2004
## light	6629.3	4195.23	1980
## heavy	14535.29	10697.37	2013
## heavy	24165.84	10697.37	2004
## heavy	21666.97	10697.37	1980

files = c(list.files("intermediate_data/warehouse_model_modify", pattern="LightManufacturing*"),
          list.files("intermediate_data/warehouse_model_modify", pattern="HeavyManufacturing*"))

files

for (f in files) {
    print(file.copy(sprintf("intermediate_data/warehouse_model_modify/%s", f),
                    sprintf("intermediate_data/idf_add_sim_period_output_versionUpdate/%s", f)))
}
