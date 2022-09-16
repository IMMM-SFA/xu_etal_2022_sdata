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

working_dir = os.path.join(os.getcwd(), "intermediate_data/idf_add_sim_period_output")

df = pd.read_csv("intermediate_data/epw_idf_to_simulate.csv")

# df = pd.read_csv("to_simulate_round2.csv")
df.rename(columns={"epw.id": "id"}, inplace=True)

idfs = df['idf.name'].unique()
idfs = [f for f in idfs if not "to be modified" in f]

# annual simulation using only 2018 July WRF
# dirname = "intermediate_data/result_ann_0520"
# epw_path = "input_data/M02_EnergyPlus_Forcing_Historical_LowRes"

# annual simulation using whole 2018 WRF
dirname = "intermediate_data/EP_output/result_ann_WRF_2018"
epw_path = "input_data/annual_WRF/M02_EnergyPlus_Forcing_Historical_LowRes_ann_2018"
# dirname = "intermediate_data/EP_output/result_ann_WRF_2016"
# epw_path = "input_data/annual_WRF/M02_EnergyPlus_Forcing_Historical_LowRes_ann_2016"

def run_sim_with_idf_epw_df(df_idf_epw):
    api = EnergyPlusAPI()
    df_idf_epw = df_idf_epw.reset_index()
    for index,row in df_idf_epw.iterrows():
        epw_id = row['id']
        idf_name = row['idf.name'].replace(".idf", "")
        print("{} epw: {}, idf: {}".format(index, epw_id, idf_name))
        idf_kw = idf_name.replace(".", "_")
        # output dir for annual simulation
        output_dir = os.path.join(os.getcwd(), "{}/{}____{:d}".format(dirname, idf_kw, epw_id))
        # output_dir = os.path.join(os.getcwd(), working_dir, "{}____{:d}".format(idf_kw, epw_id))
        # annual simulation for 2018
        if (not os.path.isdir(output_dir)):
            os.mkdir(output_dir)
            # indent to not run when folder exists
            state = api.state_manager.new_state()
            return_value = api.runtime.run_energyplus(
                state, [
                    '-d',
                    output_dir,
                    # annual simulation comparing with EnergyAtlas
                    '-a',
                    '-w',
                    os.path.join(os.getcwd(), '{}/wrf_epw'.format(epw_path), '{:d}.epw'.format(epw_id)),
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

# idf_name does not have ".idf"
def run_one(idf_name, epw_id):
    api = EnergyPlusAPI()
    print("epw: {}, idf: {}".format(epw_id, idf_name))
    idf_kw = idf_name.replace(".", "_")
    # output dir for annual simulation
    output_dir = os.path.join(os.getcwd(), "{}/{}____{:d}".format(dirname, idf_kw, epw_id))
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
            os.path.join(os.getcwd(), '{}/wrf_epw'.format(epw_path), '{:d}.epw'.format(epw_id)),
            '-r',
            os.path.join(os.getcwd(), working_dir, "{}.idf".format(idf_name))
        ]
    )
    api.state_manager.delete_state(state)

combos = [
"SingleFamily-pre-1980____68"
]

# rerun certain list of simulation combos.
for combo in combos:
    tokens = combo.split("____")
    run_one(tokens[0], int(tokens[1]))
