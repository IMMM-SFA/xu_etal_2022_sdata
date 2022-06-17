import os
import shutil
import sys
import pandas as pd
from threading import Thread
from time import sleep

from joblib import Parallel, delayed

ProductsDir = '/Applications/EnergyPlus-22-1-0'
RepoRoot = '/Applications/EnergyPlus-22-1-0'

sys.path.insert(0, str(ProductsDir))
from pyenergyplus.api import EnergyPlusAPI

working_dir = os.path.join(os.getcwd(), "idf_add_sim_period_output")

df = pd.read_csv("epw_idf_to_simulate.csv")

# df = pd.read_csv("to_simulate_round2.csv")
df.rename(columns={"epw.id": "id"}, inplace=True)

idfs = df['idf.name'].unique()
idfs = [f for f in idfs if not "to be modified" in f]

# idfs = ['MediumOffice-90.1-2013-ASHRAE 169-2013-3B.idf']
idfs = [f for f in idfs if ("Religious-" in f) or ("Manufacturing-" in f)]

idfs = [f for f in idfs if "Family-" in f]

idfs = [f for f in idfs if "RetailStandalone-" in f]

def run_sim_with_idf_epw_df(df_idf_epw):
    api = EnergyPlusAPI()
    df_idf_epw = df_idf_epw.reset_index()
    for index,row in df_idf_epw.iterrows():
        epw_id = row['id']
        idf_name = row['idf.name'].replace(".idf", "")
        print("{} epw: {}, idf: {}".format(index, epw_id, idf_name))
        idf_kw = idf_name.replace(".", "_")
        # output dir for annual simulation
        output_dir = os.path.join(os.getcwd(), "result_ann_0520/{}____{:d}".format(idf_kw, epw_id))
        # output_dir = os.path.join(os.getcwd(), working_dir, "{}____{:d}".format(idf_kw, epw_id))
        if (not os.path.isdir(output_dir)):
            os.mkdir(output_dir)
            # the epw file can run, but the idf has issue
            state = api.state_manager.new_state()
            return_value = api.runtime.run_energyplus(
                state, [
                    '-d',
                    output_dir,
                    # annual simulation comparing with EnergyAtlas
                    '-a',
                    '-w',
                    os.path.join(os.getcwd(), 'M02_EnergyPlus_Forcing_Historical_LowRes/wrf_epw', '{:d}.epw'.format(epw_id)),
                    '-r',
                    os.path.join(os.getcwd(), working_dir, "{}.idf".format(idf_name))
                ]
            )
            api.state_manager.delete_state(state)

sim_to_run = df[df['idf.name'].isin(idfs)]
n_thread = 5
k = len(sim_to_run) // n_thread
dfs = [sim_to_run.iloc[k*i:k*(i+1), :] for i in range(n_thread - 1)]
dfs.append(sim_to_run.iloc[k*(n_thread - 1):max(k*n_thread, len(sim_to_run)), :])

Parallel(n_jobs=n_thread)(delayed(run_sim_with_idf_epw_df)(dfs[i]) for i in range(n_thread))

def run_sim_multi_thread(idf_name, firstRow=False):
    api = EnergyPlusAPI()
    df_type = df[df['idf.name'] == idf_name]
    if (firstRow):
        df_type = df_type.head(n=1)
    df_type = df_type.reset_index()
    for index,row in df_type.iterrows():
        epw_id = row['id']
        idf_name = row['idf.name'].replace(".idf", "")
        print("{} epw: {}, idf: {}".format(index, epw_id, idf_name))
        idf_kw = idf_name.replace(".", "_")
        # output dir for annual simulation
        output_dir = os.path.join(os.getcwd(), "result_ann_0520/{}____{:d}".format(idf_kw, epw_id))
        # output_dir = os.path.join(os.getcwd(), working_dir, "{}____{:d}".format(idf_kw, epw_id))
        if (not os.path.isdir(output_dir)):
            os.mkdir(output_dir)
            # the epw file can run, but the idf has issue
            state = api.state_manager.new_state()
            return_value = api.runtime.run_energyplus(
                state, [
                    '-d',
                    output_dir,
                    # annual simulation comparing with EnergyAtlas
                    '-a',
                    '-w',
                    os.path.join(os.getcwd(), 'M02_EnergyPlus_Forcing_Historical_LowRes/wrf_epw', '{:d}.epw'.format(epw_id)),
                    '-r',
                    os.path.join(os.getcwd(), working_dir, "{}.idf".format(idf_name))
                ]
            )

# single thread
for idf_name in idfs:
    run_sim_multi_thread(idf_name, firstRow=True)

# idf_name does not have ".idf"
def run_one(idf_name, epw_id):
    api = EnergyPlusAPI()
    print("epw: {}, idf: {}".format(epw_id, idf_name))
    idf_kw = idf_name.replace(".", "_")
    # output dir for annual simulation
    # output_dir = os.path.join(os.getcwd(), "result_ann_0520/{}____{:d}".format(idf_kw, epw_id))
    output_dir = os.path.join(os.getcwd(), working_dir, "{}____{:d}".format(idf_kw, epw_id))
    if (not os.path.isdir(output_dir)):
        os.mkdir(output_dir)
        # the epw file can run, but the idf has issue
    state = api.state_manager.new_state()
    return_value = api.runtime.run_energyplus(
        state, [
            '-d',
            output_dir,
            # annual simulation comparing with EnergyAtlas
            # '-a',
            '-w',
            os.path.join(os.getcwd(), 'M02_EnergyPlus_Forcing_Historical_LowRes/wrf_epw', '{:d}.epw'.format(epw_id)),
            '-r',
            os.path.join(os.getcwd(), working_dir, "{}.idf".format(idf_name))
        ]
    )

# [1] "SmallOffice-DOE Ref Pre-1980-ASHRAE 169-2013-3B____48" 
# [2] "SmallOffice-DOE Ref Pre-1980-ASHRAE 169-2013-3B____49" 
# [3] "SmallOffice-90_1-2013-ASHRAE 169-2013-3B____92"        
# [4] "SmallOffice-90_1-2013-ASHRAE 169-2013-3B____93"        
# [5] "LightManufacturing-90_1-2013-ASHRAE 169-2013-3B____97" 
# [6] "LightManufacturing-90_1-2013-ASHRAE 169-2013-3B____106"


# [1] "SmallHotel-90_1-2013-ASHRAE 169-2013-3B____24"
# [2] "Religious-post-1980-3B____54"                 
# [3] "Religious-pre-1980-3B____54"                  
# [4] "Religious-post-1980-3B____65"                 
# [5] "Religious-pre-1980-3B____65"                  

combos = [ "Religious-pre-1980-3B____30" , "NursingHome_baseline-LA____36" ,
           "MultiFamily-2013____37" , "Religious-pre-1980-3B____48" ,
           "MidriseApartment-90_1-2004-ASHRAE 169-2013-3B____49" ,
           "LargeOffice-90_1-2013-ASHRAE 169-2013-3B____50" , "LargeOffice-DOE
           Ref Pre-1980-ASHRAE 169-2013-3B____50" , "MediumOffice-DOE Ref
           Pre-1980-ASHRAE 169-2013-3B____63" ,
           "FullServiceRestaurant-90_1-2004-ASHRAE 169-2013-3B____64" ,
           "FullServiceRestaurant-90_1-2013-ASHRAE 169-2013-3B____64" ,
           "MediumOffice-90_1-2004-ASHRAE 169-2013-3B____79" ,
           "Hospital-90_1-2013-ASHRAE 169-2013-3B____80" , "Hospital-DOE Ref
           Pre-1980-ASHRAE 169-2013-3B____80" , "SingleFamily-pre-1980____80" ,
           "Hospital-DOE Ref Pre-1980-ASHRAE 169-2013-3B____107" ,
           "MultiFamily-2004____110" , "MultiFamily-pre-1980____110"]

combos = [ "MidriseApartment-90.1-2004-ASHRAE 169-2013-3B____49" 
           ,"LargeOffice-90.1-2013-ASHRAE 169-2013-3B____50"
           ,"FullServiceRestaurant-90.1-2004-ASHRAE 169-2013-3B____64"
           ,"FullServiceRestaurant-90.1-2013-ASHRAE 169-2013-3B____64"
           ,"MediumOffice-90.1-2004-ASHRAE 169-2013-3B____79"
           ,"Hospital-90.1-2013-ASHRAE 169-2013-3B____80" ]

for combo in combos:
    tokens = combo.split("____")
    run_one(tokens[0], int(tokens[1]))


for i in range(1):
    threads = list()
    for index in range(4):
        idf_name = idfs[i * 4 + index]
        print("running for idf: {}".format(idf_name))
        x = Thread(target=run_sim_multi_thread, args=(idf_name,))
        threads.append(x)
        x.start()
        sleep(0)
