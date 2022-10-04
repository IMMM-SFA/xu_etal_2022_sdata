# This is from the repo https://github.com/LBNL-ETA/im3-wrf with some changes
#!/usr/bin/env python
# coding: utf-8

# In[3]:


import os, sys, json, csv
import numpy as np
import pandas as pd
import datetime as dt
import matplotlib.pyplot as plt
import seaborn as sns
import datetime as dt
import psychropy
get_ipython().run_line_magic('matplotlib', 'inline')
plt.style.use('ggplot')
import warnings
warnings.filterwarnings('ignore')


# In[9]:

wrf_headers = ['PSFC', 'Q2', 'T2', 'RH', 'WINDS', 'GLW', 'SWDOWN', 'WINDD']
# for july data
# leap_year = False
# heat_year = 2018
# WRF_FOLDER = 'M02_EnergyPlus_Forcing_Historical_LowRes'
# for annual 2018 data
heat_year = 2018
WRF_FOLDER = 'input_data/annual_WRF/M02_EnergyPlus_Forcing_Historical_LowRes_ann_2018'
leap_year = False
# for annual 2016 data
# WRF_FOLDER = 'input_data/annual_WRF/M02_EnergyPlus_Forcing_Historical_LowRes_ann_2016'
# heat_year = 2016
# leap_year = True

# # before extend period
# heat_month = [7]
# start_day = 2
# end_day = 18
# heat_day_start = [start_day]
# heat_day_end = [end_day]

# this is to generate july month epw, after extending to cover whole july
# heat_month = [6, 7, 8]
# heat_day_start = [29, 1, 1]
# heat_day_end = [30, 31, 3]

# this is to generate annual
heat_month = list(range(1, 13))
heat_day_start = [1 for i in range(12)]
heat_day_end = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
if (leap_year):
    heat_day_end[1] = 29

# for coarse grid
row = 14
col = 14

# In[5]:


valid_wrf_grids = set()
with open(os.getcwd() + '/input_data/M02_EnergyPlus_Forcing_Historical_LowRes/meta/wrf-grids-2229.geojson', 'r') as f:
    data = json.load(f)
    
    for wrf_grid in data['features']:
        if wrf_grid['properties']['has_bldgs'] == 1:
            valid_wrf_grids.add(int(wrf_grid['properties']['id']))
        
len(valid_wrf_grids)


# ## Write grid data (from txt) to timeseries CSV files

# In[14]:


for wrf_header in wrf_headers:
    wrf_data = dict()
    year = str(heat_year)

    # M02_EnergyPlus_Forcing_Historical/SWDOWN/Variable_SWDOWN_2008070200.txt

    wrf_data = dict()
    for grid in valid_wrf_grids:
        wrf_data[grid] = []
    for mon in range(len(heat_month)):
        month = str(heat_month[mon]).zfill(2)
        for day in range(heat_day_start[mon], heat_day_end[mon] + 1):
            # print("{}/{}".format(month, day))
            d = day - heat_day_start[mon]
            for h in range(24):
                date = str(day).zfill(2)
                hour = str(h).zfill(2)
                day = str(day).zfill(2)
                file_name = os.path.join(os.getcwd(), WRF_FOLDER, wrf_header, 'Variable_' + wrf_header + '_' + year + month + date + hour + '.txt')
                with open(file_name, 'r') as f:
                    for i in range(row):
                        cur_wrf_data = f.readline().strip().split('	') 
                        for j in range(col):
                            if i * col + j in valid_wrf_grids:
                                wrf_data[i * col + j].append(float(cur_wrf_data[j]))
    data = pd.DataFrame.from_dict(wrf_data)
    data.to_csv(os.path.join(WRF_FOLDER, 'time_series', wrf_header + '.csv'))


# In[15]:


data.head() # WINDD


# In[16]:


valid_wrf_grids = list(valid_wrf_grids)


# In[46]:


df_result_t2 = pd.read_csv(os.path.join(WRF_FOLDER, 'time_series', 'T2.csv'), sep=',', encoding='UTF-8', index_col=0)
df_result_p = pd.read_csv(os.path.join(WRF_FOLDER, 'time_series', 'PSFC.csv'), sep=',', encoding='UTF-8', index_col=0)
df_result_rh = pd.read_csv(os.path.join(WRF_FOLDER, 'time_series', 'RH.csv'), sep=',', encoding='UTF-8', index_col=0)
df_result_glw = pd.read_csv(os.path.join(WRF_FOLDER, 'time_series', 'GLW.csv'), sep=',', encoding='UTF-8', index_col=0)
df_result_wd = pd.read_csv(os.path.join(WRF_FOLDER, 'time_series', 'WINDD.csv'), sep=',', encoding='UTF-8', index_col=0)
df_result_ws = pd.read_csv(os.path.join(WRF_FOLDER, 'time_series', 'WINDS.csv'), sep=',', encoding='UTF-8', index_col=0)
df_result_q = pd.read_csv(os.path.join(WRF_FOLDER, 'time_series', 'Q2.csv'), sep=',', encoding='UTF-8', index_col=0)
df_result_rh.head(12)


# No need to convert UTC to local time here, as solar data has local time in it

df_result_q.describe()

# ### This solar data is manually parsed (dir-dif separation) using the external Excel tool. Using one data point only.

# In[49]:


df_wrf_data = pd.read_csv(os.path.join(WRF_FOLDER, 'time_series', 'LA-SOLAR.csv'), sep=',', encoding='UTF-8')
df_wrf_data.head()


# In[50]:


for i in range(len(valid_wrf_grids)):
    grid_id = str(valid_wrf_grids[i])
    df_wrf_data_new = df_wrf_data
    df_wrf_data_new['dry-bulb'] = df_result_t2[grid_id]
    df_wrf_data_new['rel-humidity'] = df_result_rh[grid_id] * 100
    df_wrf_data_new['hum-rat'] = df_result_q[grid_id]
    df_wrf_data_new['pressure'] = df_result_p[grid_id]
    df_wrf_data_new['dw-lwr'] = df_result_glw[grid_id]
    df_wrf_data_new['wind-dir'] = df_result_wd[grid_id]
    df_wrf_data_new['wind-speed'] = df_result_ws[grid_id]
    df_wrf_data_new.to_csv(os.path.join(WRF_FOLDER, 'grids_csv', grid_id + '.csv'))


# In[51]:


# need to copy in the base epw file first
LOC_WEATHER_FILE = os.path.join(WRF_FOLDER, 'USA_CA_Los.Angeles.Intl.AP.722950_TMY3.epw')
csv_headers_epw_map = {6: 'dry-bulb', 7:'dew-point', 8:'rel-humidity', 9:'pressure', 12: 'dw-lwr',
                        13: 'sw_normal', 14: 'sw_dir', 15: 'sw_dif', 20:'wind-dir', 21:'wind-speed'}
days_in_mon = {1:31, 2:28, 3:31, 4:30, 5:31, 6:30, 7:31, 8:31, 9:30, 10:31, 11:30, 12:31}
if (leap_year):
    days_in_mon[2] = 29


# ## Write CSV to EPW

# In[52]:


## write EPW example
grid_id = str(valid_wrf_grids[10])
df_csv_result = pd.read_csv(os.path.join(WRF_FOLDER, 'grids_csv', grid_id + '.csv'), sep=',', parse_dates=True, infer_datetime_format=True, encoding='UTF-8')
data_to_write = dict()
for i, r in df_csv_result.iterrows():
    r['dew-point'] = psychropy.Dew_point(float(r['pressure'])/1000.0, float(r['hum-rat']))
    mon = int(r['mon'])
    day = int(r['day'])
    hour = int(r['hour'])
    data_to_write[str(mon) + "/" + str(day) + "/" + str(hour)] = r


# In[53]:


data_to_write['7/2/1']


# In[59]:


for i in range(len(valid_wrf_grids)):
    grid_id = str(valid_wrf_grids[i])
    print(grid_id)
    df_csv_result = pd.read_csv(os.path.join(WRF_FOLDER, 'grids_csv', grid_id + '.csv'), sep=',', parse_dates=True, infer_datetime_format=True, encoding='UTF-8')
    data_to_write = dict()

    for i, r in df_csv_result.iterrows():
        try:
            r['dew-point'] = psychropy.Dew_point(float(r['pressure']) / 1000.0, float(r['hum-rat']))
        except:
            print('Something wrong with dew point calculation.')
            r['dew-point'] = None
        mon = int(r['mon'])
        day = int(r['day'])
        hour = int(r['hour'])
        data_to_write[str(mon) + "/" + str(day) + "/" + str(hour)] = r

    EPW_FILE = os.path.join(WRF_FOLDER, 'wrf_epw', str(int(grid_id)) + '.epw')
    f = open(EPW_FILE, 'w')
    with open(LOC_WEATHER_FILE, newline='') as csvfile:
        csvreader = csv.reader(csvfile, delimiter=',', quotechar='|')
        for row in csvreader:
            if (len(row) == 35):
                mon = row[1]
                day = row[2]
                hour = row[3]
                minute = row[4]
                if int(hour) == 24:
                    hour = 0
                    if int(day) == days_in_mon[int(mon)]:
                        # here fixed an issue in the online repo that will error on 12-31
                        if (int(mon) < 12):
                            mon = int(mon) + 1
                        else:
                            mon = 1
                        day = 1
                    else :
                        day = int(day) + 1
                cur_time_stamp = str(mon) + "/" + str(day) + "/" + str(hour)
                if cur_time_stamp in data_to_write:
                    cur_data_to_write = data_to_write[cur_time_stamp]
                    for epw_col in csv_headers_epw_map:
                        if csv_headers_epw_map[epw_col] in cur_data_to_write:
                            try:
                                row[epw_col] = str(round(cur_data_to_write[csv_headers_epw_map[epw_col]], 3))
                            except:
                                row[epw_col] = ""
                                print(EPW_FILE)
                                print(cur_time_stamp)
            f.write(','.join(row) + '\n')
    f.close()
