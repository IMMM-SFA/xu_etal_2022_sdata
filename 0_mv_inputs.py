# This script is from this repo: https://github.com/LBNL-ETA/im3-wrf
import os
from subprocess import run

# set up the directory containing the WRF forcing data

# this is the july data setting
# forcing_folder = 'input_data/M02_EnergyPlus_Forcing_Historical_LowRes'
# this is the annual 2018 WRF setting
# forcing_folder = 'input_data/annual_WRF/M02_EnergyPlus_Forcing_Historical_LowRes_ann_2018'
# this is the annual 2016 WRF setting
forcing_folder = 'input_data/annual_WRF/M02_EnergyPlus_Forcing_Historical_LowRes_ann_2016'

file_list = os.listdir(forcing_folder)

for var_name in ['T2', 'RH', 'Q2', 'GLW', 'PSFC', 'WINDS', 'WINDD', 'SWDOWN']:
    run(['mkdir', os.path.join(forcing_folder, var_name)])
run(['mkdir', os.path.join(forcing_folder, 'meta')])
run(['mkdir', os.path.join(forcing_folder, 'time_series')]) # time series weather data for all grids in CSV format for each var
run(['mkdir', os.path.join(forcing_folder, 'grids_csv')]) # time series weather data for all vars in CSV format for each grid
run(['mkdir', os.path.join(forcing_folder, 'wrf_epw')]) # time series weather data for all vars in CSV format for each grid

for file_name in file_list:
    for var_name in ['T2', 'RH', 'Q2', 'GLW', 'PSFC', 'WINDS', 'WINDD', 'SWDOWN']:
        if var_name in file_name:
            run(['mv', os.path.join(forcing_folder, file_name), os.path.join(forcing_folder, var_name, file_name)])
