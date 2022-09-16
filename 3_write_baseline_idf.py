#!/usr/bin/env python
# coding: utf-8

# In[3]:


import os, sys, csv, json, re
import parseidf
import glob
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
get_ipython().run_line_magic('matplotlib', 'inline')


# ## Write Baseline IDF

# In[4]:


WORK_FOLDER_NAME = 'intermediate_data/idf_change_design_day' ## todo: modify the following code for new models
OUT_FOLDER_NAME = 'intermediate_data/idf_add_sim_period_output'
BLDG_NAME_LIST = glob.glob("{}/*.idf".format(WORK_FOLDER_NAME))

# BLDG_NAME_LIST = ['idf_change_design_day/Warehouse-90.1-2004-ASHRAE 169-2013-3B.idf',
#                   'idf_change_design_day/Warehouse-90.1-2013-ASHRAE 169-2013-3B.idf',
#                   'idf_change_design_day/Warehouse-DOE Ref Pre-1980-ASHRAE 169-2013-3B.idf']

# BLDG_NAME_LIST = ['idf_change_design_day/NursingHome_baseline-LA.idf']

for bldg_name in BLDG_NAME_LIST:
    print(bldg_name)
    input_file_path = os.path.join(bldg_name)
    output_file_path= os.path.join(bldg_name.replace(WORK_FOLDER_NAME, OUT_FOLDER_NAME))
    
    if not os.path.exists(input_file_path):
        continue

    with open(input_file_path, 'r') as f:
        idf = parseidf.parse(f.read())
        ext_surfaces = []
        
        # Change run period: for heatwave month
        # idf['RunPeriod'.upper()][0][2] = 7
        # idf['RunPeriod'.upper()][0][3] = 1
        # idf['RunPeriod'.upper()][0][5] = 7
        # idf['RunPeriod'.upper()][0][6] = 31
        # annual simulation
        idf['RunPeriod'.upper()][0][2] = 1
        idf['RunPeriod'.upper()][0][3] = 1
        idf['RunPeriod'.upper()][0][5] = 12
        idf['RunPeriod'.upper()][0][6] = 31
        
        # Add meter and reportable variables
        idf['Output:Meter'.upper()] = list()
        idf['Output:Meter'.upper()].append(['Output:Meter', 'Cooling:Electricity', 'hourly'])
        idf['Output:Meter'.upper()].append(['Output:Meter', 'Heating:Gas', 'hourly'])
        idf['Output:Meter'.upper()].append(['Output:Meter', 'Electricity:Facility', 'hourly'])
        idf['Output:Meter'.upper()].append(['Output:Meter', 'NaturalGas:Facility', 'hourly'])
        idf['Output:Variable'.upper()] = list()

        # five heat emission component
        idf['Output:Variable'.upper()].append(
            ['Output:Variable', '*', 'Site Total Zone Exfiltration Heat Loss', 'hourly'])
        idf['Output:Variable'.upper()].append(
            ['Output:Variable', '*', 'Site Total Zone Exhaust Air Heat Loss', 'hourly'])
        idf['Output:Variable'.upper()].append(
            ['Output:Variable', '*', 'HVAC System Total Heat Rejection Energy', 'hourly'])
        idf['Output:Variable'.upper()].append(
            ['Output:Variable', '*', 'Air System Relief Air Total Heat Loss Energy', 'hourly'])
        idf['Output:Variable'.upper()].append(
            ['Output:Variable', '*', 'Site Total Surface Heat Emission to Air', 'hourly'])
        idf['Output:Variable'.upper()].append(
            ['Output:Variable', '*', 'Site Outdoor Air Drybulb Temperature', 'hourly'])
        idf['Output:Variable'.upper()].append(
            ['Output:Variable', '*', 'Site Outdoor Air Dewpoint Temperature', 'hourly'])
        idf['Output:Variable'.upper()].append(
            ['Output:Variable', '*', 'Site Outdoor Air Wetbulb Temperature', 'hourly'])
        idf['Output:Variable'.upper()].append(
            ['Output:Variable', '*', 'Site Outdoor Air Relative Humidity', 'hourly'])
        idf['Output:Variable'.upper()].append(
            ['Output:Variable', '*', 'Site Outdoor Air Barometric Pressure', 'hourly'])
        idf['Output:Variable'.upper()].append(
            ['Output:Variable', '*', 'Site Wind Speed', 'hourly'])
        idf['Output:Variable'.upper()].append(
            ['Output:Variable', '*', 'Site Wind Direction', 'hourly'])
        
        # to CSV output directly
        idf['OutputControl:Files'.upper()] = list()
        idf['OutputControl:Files'.upper()].append(['OutputControl:Files', 'Yes', 'No', 'No', 'No'])


    with open(output_file_path, 'w') as out:
        for idf_object_key in idf:
            idf_objects = idf[idf_object_key]
            for idf_object in idf_objects:
                for attr_num in range(len(idf_object)):
                    if attr_num == 0:
                        out.write('  ' + str(idf_object[attr_num]) + ',\n')
                    elif attr_num == len(idf_object) - 1:
                        out.write('    ' + str(idf_object[attr_num]) + ';\n')
                    else:
                        out.write('    ' + str(idf_object[attr_num]) + ',\n')
                out.write('\n')
