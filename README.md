_your zenodo badge here_

# xu_etal_2022_tbd

**LA County Building Anthropogenic Heat During Heat Waves**

Yujie Xu<sup>1</sup>, Pouya Vahmani<sup>1</sup>, Tianzhen Hong\*, Andy Jones<sup>1</sup>

<sup>1 </sup> LBNL

\* corresponding author:  thong@lbl.gov

## Abstract
TBD

## Journal reference
TBD

## Code reference
<!-- References for each minted software release for all code involved.   -->

<!-- These are generated by Zenodo automatically when conducting a release when Zenodo has been linked to your GitHub repository. The Zenodo references are built by setting the author order in order of contribution to the code using the author's GitHub user name.  This citation can, and likely should, be edited without altering the DOI. -->

<!-- If you have modified a codebase that is outside of a formal release, and the modifications are not planned on being merged back into a version, fork the parent repository and add a `.<shortname>` to the version number of the parent and construct your own name.  For example, `v1.2.5.hydro`. -->

<!-- Human, I.M. (2021, April 14). Project/repo:v0.1.0 (Version v0.1.0). Zenodo. http://doi.org/some-doi-number/zenodo.7777777 -->

to be added

## Data reference

### Input data
Held in the input_data folder, including the external data files referenced downstream in the analysis
- LA buildings
    - LA building characteristics: Assessor_Parcels_Data_-_2019.csv, from Dropbox
    - LA building geometry: LARIAC6_LA_County.geojson
- Climate
    - Annual WRF data: held in annual_WRF in a 12km x 12km grid system. The two
      .tar files are original data from Pouya. The tar balls contain weather
      data in the following format: Variable_<variable_name>_<timestamp>.txt and
      grid latitude longitude data (Fixed_XLAT.txt, Fixed_XLONG.txt) The
      annual_WRF/M02_EnergyPlus_Forcing_Historical_LowRes_ann_<year> folders
      holds the grid weather data for each variable in separate folders (GLW,
      PSFC, Q2, RH, SWDOWN, T2, WINDD, WINDS). The grids_csv holds weather data
      compiled for each grid cell, time_series holds weather data for each WRF
      variable. The wrf_epw folder contains the .epw files for each grid cell.
      These epw files are used in the heat and energy simulation.
    - July WRF data: held in M02_EnergyPlus_Forcing_Historical_LowRes. The
      folder structure is similar to the
      annual_WRF/M02_EnergyPlus_Forcing_Historical_LowRes_ann_<year> folders
    - WRF grid latitude longitude for two resolutions, 
        - coarse grid:
          M02_EnergyPlus_Forcing_Historical_LowRes/meta/wrf-grids-origin.geojson.
          This grid corresponds to the epw files used in EnergyPlus simulations.
        - fine grid: high res grid for reporting/wrf-grids-origin.geojson. This
          grid system is used in reporting
        - census tract: domain/tl_2018_06_tract/tl_2018_06_tract.shp. This is
          used in reporting.
- Prototype Building Models: held in folder "LA Prototypes"
    - commercial buildings: most commercial building models are held in Com
      (OS_Standards). The Com need mod folder contains models for Religious
      buildings in different climate zones. The study uses the one corresponding
      to 3B. "​​NursingHome source" contains the nursing home models from
      https://www.sciencedirect.com/science/article/pii/S0360132320302018?casa_token=Ct-JsOrSNeYAAAAA:LVadWomEMGB-oGf3A69HCkAzhKZpeKZJ78kjKuKlcQpuFzki2By9JRU7azPgErbZhjk-y10iOg#fig4.
      The blackout incident is removed and the systems are changed to autosize.
      The adjusted model is in "NursingHome mod sched autosize"
    - residential buildings: held in "Res (CBES)" folder. "bldg_11" are
      single-family buildings. "building_13" are multi-family buildings. "vin_1"
      is pre-1980. "vin_5" is 2004 and "vin_8" is 2013. ""es_schedule" contains
      the schedule files used in residential models.
- Lookup tables to re-map types between assessor data, prototype building
  models, and Energy Atlas building types in verification
    - building_type_recode.csv maps the building types in
      Assessor_Parcels_Data_-_2019.csv and EnergyPlus prototype building type
    - type_vintage_to_idf_mapping.csv maps building type and vintage to the idf
      file used to simulate the building
- MECS survey tables are held in "MECS" folder. It is used for extracting
  summary statistics to model heavy and light manufacturing facilities
- Energy Atlas: annual electricity and gas consumption. "usage_bld_kwh.csv" is
  electricity data. "usage_bld_therm.csv" is gas data. "usage_bld_btu.csv" is
  electricity + gas data.

### Intermediate data
Held in the intermediate_data folder, including the data files in the intermediate data analysis or simulation steps
- EnergyPlus input idf files in various processing stages
    - idf_to_modify: input idfs in their original state
    - idf_change_design_day: idf with design day and location changed to LA
    - idf_add_sim_period_output: idf with RunPeriod adjusted to Jan. 1st to Dec. 31st and with heat emission and energy consumption outputs added
    - warehouse_model_modify: warehouse models and light and heavy manufacturing facility models derived from them.
- Summary statistics: held in "summary" folder
- compiled MECS data: energy_intensity_per_type.csv, used in compiling the weighted quantile for manufacturing facilities 
- epw_idf_to_simulate.csv: epw-idf combination to be simulated with EnergyPlus, referenced in run_sim.py
- weather_2018.csv: data file that generates the figure "compiled_epw_weather.png"

To be continued...

### Output data

- simulation results for prototype-building-wrf-grid-combination: held in
  EP_output/result_ann_WRF_<year>, for 2016 and 2018: Each subfolder contains a
  simulation output of a prototype model and WRF grid as follows <prototype
  model key word>____<WRF grid ID>. The eplusout.csv in each subfolder holds the
  hourly energy and heat emission results.
- Building metadata: building_metadata.geojson file holds the type, vintage,
building size, and centroid geometry of the compiled

| Column name | Column definition |
|-------------|-------------------|
| OBJECTID | Unique building ID inherited from LARIAC6_LA_County.geojson |
| GeneralUseType | Building type, inherited from Assessor_Parcels_Data_-_2019.csv |
| SpecificUseType | Building type, inherited from Assessor_Parcels_Data_-_2019.csv |
| EffectiveYearBuilt | Built year, inherited from Assessor_Parcels_Data_-_2019.csv |
| building.type | Prototype building type |
| vintage | Prototype building vintage |
| idf | Prototype model filename used to simulate the building |
| usetype | Corresponding EnergyAtlas usetype |
| footprint.area | Total building footprint area [m2], inherited from SQFTMain column in Assessor_Parcels_Data_-_2019.csv |
| building.size | Total building total floor area [m2] |
| id.grid.coarse | The grid cell in the 12km x 12km grid system containing this building |
| id.grid.finer | The grid cell in the 450m x 450m grid system containing this building |
| id.tract | The census tract GEOID containing this building |
| geometry | Point of building centroid |

- aggregated heat emission and energy data for the three spatial resolutions. All three files have the same column structure.
    - finer grid 450 x 450m
    - coarser grid 12km x 12km
    - census tract

| Column name | Column definition |
|-------------|-------------------|
| GeoID | WRF grid ID or census tract GEOID. Use the geojson for the corresponding spatial resolution to look up the location and shape of the GeoID |
| Timestamp | Hourly, local time of LA county. |
| exf | Zone exfiltration heat loss [MJ] |
| exh | Zone exhaust air heat loss [MJ] |
| rej | HVAC system heat rejection [MJ] |
| rel | HVAC system relief air heat loss [MJ] |
| surf | Surface heat emission [MJ] |
| emission.total | Total heat emission [MJ] |
| elec | Total electricity consumption [MJ] |
| gas | Total gas consumption [MJ] |
| energy.total | Total electricity and gas consumption [MJ] |

- Aggregated geographical data referenced in heat emission and energy consumption
 
| Column name | Column definition |
|-------------|-------------------|
| GeoID | WRF grid ID or census tract GEOID |
| geometry | Polygon shapes of the WRF grid points bounding box or census tracts |
| area | Grid or census tract polygon size |
| footprint.area | Total building footprint area [m2] |
| building.size | Total building total floor area [m2] |
    
## Contributing modeling software
| Model | Version | Repository Link | DOI |
|-------|---------|-----------------|-----|
| EnergyPlus | 22.1 | https://github.com/NREL/EnergyPlus |  |

## Reproduce my experiment

The following is an overview of the workflow

![workflow](figures/workflow.png)

Following the steps to reproduce the analysis

1. Compile a LA county geojson file with building footprint, type, vintage,
   number of stories, and footprint area using the geometry and assessor data
   files from the Dropbox folder "City Data/LA".
2. Acquire WRF climate data (in a 12 km x 12 km grid system)
3. Convert the WRF climate data of the historic forcing to epw. 
    1. Untar the "M02_EnergyPlus_Forcing_Historical_LowRes*" folder. This
       creates a folder <A> to hold all the files from the tar ball
    2. Run 0_mv_inputs.py to create folder structure and move files to
       corresponding folders. "forcing_folder" is set to be where the WRF data
       is extracted, i.e. <A> from above.
    3. Copy the "USA_CA_Los.Angeles.Intl.AP.722950_TMY3.epw" into folder <A>
    4. Run 2_wrf_to_epw.py to before this line. Note that the WRF_FOLDER at the
    beginning should be set to <A> as well. df_wrf_data =
    pd.read_csv(os.path.join(WRF_FOLDER, 'time_series', 'LA-SOLAR.csv'),
    sep=',', encoding='UTF-8')
    5. Get solar radiation input with get_solar_input.R. This creates a file
       "LA-SWDOWN_input_to_excel.csv" in the <a>/time_series folder. The file
       looks like this
    6. Open the file and copy data to the excel tool Los_Angeles_TMY_2010s Solar
     irradiance.xlsx. Note that for leap year, we should not copy in the Feb 29
     data, as the excel tool won't accept that. Also due to UTC to local time
     conversion, there is a time shift, the UTC time 2018 does not cover the
     whole local time 2018. We’ll use the year-end data of the previous year to
     fill in for the missing hours of the current end of year. Then paste the W,
     X, Y column in sheet "Output – Isotropic sky" to
     "LA-SWDOWN_input_to_excel.csv". For Feb 29 data, paste it in the "input"
     sheet in place of Feb 28, then get the output three solar component of Feb
     28 from "Output – Isotropic sky", and paste them in the Feb 29 slots in the
     csv. Change the column names of the three solar component to "sw_normal",
     "sw_dif", and "sw_dir". Save the csv file as LA-SOLAR.csv
    7. Go back to 2_wrf_to_epw.py, now the previous line should run through and
       read in solar data to df_wrf_data.
    8. Check whether there are missing data in the generated .epw files using
       check_epw_err.R. Missing values are most often in Dew Point. If there are
       missing values, use fill_na_in_epw.R to fill the missing value with the
       previous non-missing record.
4. Assign the nearest grid point to each building. The epw files for the
   assigned grid point will be used in the simulation of the target building.
   This is documented in the rmd/1_match_building_to_grid.Rmd file
5. For each building type-vintage combination in each grid cell, simulate the
   historic forcing. There are xx possible prototype buildings and 3 possible
   vintages, but we will only simulate the type-vintage combination appearing in
   each grid cell (see xx for the mapping from grid cells to type-vintage
   combination). Use "3_write_baseline_idf" from
   [im3 repo from Xuan](https://github.com/LBNL-ETA/im3-wrf/blob/main/3_write_baseline_idf.ipynb) to
   create EnergyPlus models.
7. Adjust prototype models
    - Change the design condition, using 3_idf_preprocess.R. The script first
      copies the idf files from input_data/annual_WRF into
      intermediate_data/idf_to_modify, then change the design day and location
      of the idf files then output them to
      intermediate_data/idf_change_design_day
    - Add run period and output variables using 3_write_baseline_idf.py: takes
      idf from intermediate_data/idf_change_design_day and add runperiod and
      output variables, outputs to intermediate_data/idf_add_sim_period_output
    - Remove un-used dependencies from files: 3_replace_schedule_csv_path.R
    - Create heavy and light manufacturing facility models by adjusting the
      electric and gas equipment to match the EUI of the MECS 75th and 25th
      percentile
        - retrieve the 25th and 75th percentile using get_manufacturing_energy_stats.R
        - adjust the model electricity and gas equipment using 3_get_manufacturing_idf.R
    - Update model version using idfVersionUpdater.exe
    - fix errors in the Religious model by running 3_fix_religious.R. The
      original model have some errors of missing objects and wrong value for the
      start day in the RunPeriod object.
    - fix a field in nursing home model using 3_correct_nursingHome.R
    - change the year and "Day of Week for Start Day" in the RunPeriod object to
      match the actual simulation year
8. Use run_sim.py to run simulations
9. Validation with measured and other data source: fixme: add rmd
10. Produce grid-level heat emission data.

<!-- Run the following scripts in the `workflow` directory to re-create this experiment: -->

<!-- | Script Name | Description | How to Run | -->
<!-- | --- | --- | --- | -->
<!-- | `step_one.py` | Script to run the first part of my experiment | `python3 step_one.py -f /path/to/inputdata/file_one.csv` | -->
<!-- | `step_two.py` | Script to run the last part of my experiment | `python3 step_two.py -o /path/to/my/outputdir` | -->

<!-- 4. Download and unzip the output data from my experiment [Output data](#output-data) -->
<!-- 5. Run the following scripts in the `workflow` directory to compare my outputs to those from the publication -->

<!-- | Script Name | Description | How to Run | -->
<!-- | --- | --- | --- | -->
<!-- | `compare.py` | Script to compare my outputs to the original | `python3 compare.py --orig /path/to/original/data.csv --new /path/to/new/data.csv` | -->

## Reproduce my figures
Fixme: add rmd files
<!-- Use the scripts found in the `figures` directory to reproduce the figures used in this publication. -->

<!-- | Script Name | Description | How to Run | -->
<!-- | --- | --- | --- | -->
<!-- | `generate_figures.py` | Script to generate my figures | `python3 generate_figures.py -i /path/to/inputs -o /path/to/outuptdir` | -->
