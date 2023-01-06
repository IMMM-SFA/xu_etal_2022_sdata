#!/usr/bin/env python
# coding: utf-8

# In[1]:


import os, sys, json
import numpy as np
import pandas as pd
import datetime as dt
import matplotlib.pyplot as plt
import seaborn as sns
get_ipython().run_line_magic('matplotlib', 'inline')
plt.style.use('ggplot')
import warnings
warnings.filterwarnings('ignore')


# In[2]:


from json import dumps
from shapely.geometry import Point
from shapely.geometry.polygon import Polygon

def list_geojson_properties(geojson):
    print('GeoJson properties:\n' + "; ".join(geojson['properties']))
    print('GeoJson geometry coordinates: ' + str(geojson['geometry']['coordinates'][0][0]))

def get_polygon_from_geojson_geometry(geometry):
    polygon_list = list()
    if geometry['type'] == 'MultiPolygon':
        for polygon in geometry['coordinates']:
            polygon_list.append(Polygon(polygon[0]))
    elif geometry['type'] == 'Polygon':
        polygon_list.append(Polygon(geometry['coordinates'][0]))
    return polygon_list


# In[4]:


BOUNDARIES_NAME = 'domain/la-county-boundary.geojson'
districts_data = dict()
with open(BOUNDARIES_NAME, 'r') as f:
    data = json.load(f)
    district_list = data['features']

for district in district_list:
    district_name = str(district['properties']['OBJECTID'])
    district_name = district_name.replace(' ', '_')
    districts_data[district_name] = dict()
    districts_data[district_name]['coordinates'] = get_polygon_from_geojson_geometry(district['geometry'])


# ## Generate WRF grid GeoJSON boundaries
# 
# - Input from WRF are all in matrix format with `row` * `col`.
# - Each lat, lon in WRF input is the coordinate of a center of a WRF grid (e.g. A)
# - The code block calculate the boundary vertice [a, b, c, d] and write them as an element in the GeoJSON.
# 
# <img src="img/img1.png" alt="Drawing" style="width: 300px;"/>

# In[5]:

# grid_level = "coarse"
grid_level = "finer"

if (grid_level == "coarse"):
    # FORCING_FOLDER = 'M02_EnergyPlus_Forcing_Historical'
    # coarse grid, used for simulation
    FORCING_FOLDER = 'M02_EnergyPlus_Forcing_Historical_LowRes'
    row = 14
    col = 14
elif (grid_level == "finer"):
    # finer grid, used for visualization
    FORCING_FOLDER = 'high res grid for reporting'
    row = 384
    col = 339

c_lat = np.zeros((row + 2, col + 2))
c_lon = np.zeros((row + 2, col + 2))
file_name = os.path.join(FORCING_FOLDER, 'Fixed_XLAT.txt')
with open(file_name, 'r') as f:
    for i in range(row):
        cur_data = f.readline().split()
        for j in range(col):
            c_lat[i + 1, j + 1] = float(cur_data[j])
            
file_name = os.path.join(FORCING_FOLDER, 'Fixed_XLONG.txt')
with open(file_name, 'r') as f:
    for i in range(row):
        cur_data = f.readline().split()
        for j in range(col):
            c_lon[i + 1, j + 1] = float(cur_data[j])

for j in range(1, col + 1):
    c_lon[0, j] = c_lon[1, j] - (c_lon[2, j] - c_lon[1, j])
    c_lon[row + 1, j] = c_lon[row, j] + (c_lon[row, j] - c_lon[row - 1, j])
    c_lat[0, j] = c_lat[1, j] - (c_lat[2, j] - c_lat[1, j])
    c_lat[row + 1, j] = c_lat[row, j] + (c_lat[row, j] - c_lat[row - 1, j])

for i in range(row + 2):
    c_lon[i, 0] = c_lon[i, 1] - (c_lon[i, 2] - c_lon[i, 1])
    c_lon[i, col + 1] = c_lon[i, col] + (c_lon[i, col] - c_lon[i, col - 1])
    c_lat[i, 0] = c_lat[i, 1] - (c_lat[i, 2] - c_lat[i, 1])
    c_lat[i, col + 1] = c_lat[i, col] + (c_lat[i, col] - c_lat[i, col - 1])


# In[6]:


c_lon[:5, :5]


# In[7]:


c_lon[384 - 3:, 339 - 3:]


# In[8]:


ul_lat = np.zeros((row + 1, col + 1))
ul_lon = np.zeros((row + 1, col + 1))

for i in range(row + 1):
    for j in range(col + 1):
        ul_lat[i, j] = (c_lat[i, j] + c_lat[i + 1, j] + c_lat[i + 1, j + 1] + c_lat[i, j + 1])/4
        ul_lon[i, j] = (c_lon[i, j] + c_lon[i + 1, j] + c_lon[i + 1, j + 1] + c_lon[i, j + 1])/4


# In[9]:


ul_lon[:3, :3]


# In[10]:


ul_lon[384 - 3:, 339 - 3:]


# ### note: adding a "domain_flag" indicating if a WRF grid is within the domain (e.g. LA County)

# In[13]:


wrf_grids = []
ind = -1
for i in range(row):
    for j in range(col):
        ind += 1
        grid = [[ul_lon[i,j], ul_lat[i,j]], [ul_lon[i+1,j], ul_lat[i+1,j]],
                [ul_lon[i+1,j+1], ul_lat[i+1,j+1]], [ul_lon[i,j+1], ul_lat[i,j+1]], [ul_lon[i,j], ul_lat[i,j]]]
        grid_id = ind
        domain_flag = 0
        geometry = Polygon(grid)
        centroid = geometry.centroid
        for district in districts_data:
            district_poly = districts_data[district]['coordinates'][0]
            if district_poly.contains(centroid):
                domain_flag = 1
                break
        grid_properties = {'id':grid_id, 'domain_flag':domain_flag, 'centroid':str((centroid.x, centroid.y))}
        grid_geo = { "type": "Polygon", "coordinates":[grid]}
        grid_info = {"type": "Feature", "properties": grid_properties, "geometry": grid_geo}
        wrf_grids.append(grid_info)

if (grid_level == "coarse"):
    geojson = open(os.path.join(FORCING_FOLDER, 'meta', 'wrf-grids-origin.geojson'), 'w')
    # geojson = open(os.path.join(FORCING_FOLDER, 'meta', 'wrf-grids-origin.geojson'), 'w')
elif (grid_level == "finer"):
    geojson = open(os.path.join(FORCING_FOLDER, 'wrf-grids-origin.geojson'), 'w')

geojson.write(dumps({"type": "FeatureCollection", "features": wrf_grids}, indent=2))
geojson.close()
print('Total number of grids: ' + str(ind + 1))


# In[ ]:




