import os
import shutil
import sys
import pandas as pd
import glob
from threading import Thread
from time import sleep

from joblib import Parallel, delayed

ProductsDir = '/Applications/EnergyPlus-22-1-0'
RepoRoot = '/Applications/EnergyPlus-22-1-0'

sys.path.insert(0, str(ProductsDir))
from pyenergyplus.api import EnergyPlusAPI

# year = 2018
year = 2016

working_dir = os.path.join(os.getcwd(), "intermediate_data/idf_to_sim_{}".format(year))

df = pd.read_csv("intermediate_data/epw_idf_to_simulate.csv")

# df = pd.read_csv("to_simulate_round2.csv")
df.rename(columns={"epw.id": "id"}, inplace=True)

idfs = df['idf.name'].unique()
idfs = [f for f in idfs if not "to be modified" in f]

# annual simulation using whole 2016 or 2018 WRF
dirname = "output_data/EP_output/result_ann_WRF_{}".format(year)
epw_path = "input_data/annual_WRF/M02_EnergyPlus_Forcing_Historical_LowRes_ann_{}".format(year)

def test_run_all_model(epw_path, output_folder, filter_substring=""):
    idfs = glob.glob("{}/*.idf".format(working_dir))
    idfs = [f for f in idfs if filter_substring in f]
    test_epw = os.path.join(os.getcwd(), epw_path)
    api = EnergyPlusAPI()
    for idf in idfs:
        idf_name = idf.replace(".idf", "")
        idf_name = idf_name.replace(working_dir + "/", "")
        idf_kw = idf_name.replace(".", "_")
        print(idf_kw)
        output_dir = os.path.join(os.getcwd(), "intermediate_data/EP_output/{}/{}".format(output_folder, idf_kw))
        if (not os.path.isdir(output_dir)):
            os.mkdir(output_dir)
            state = api.state_manager.new_state()
            return_value = api.runtime.run_energyplus(
                state, [
                    '-d',
                    output_dir,
                    '-w',
                    test_epw,
                    '-r',
                    idf
                ]
            )
            api.state_manager.delete_state(state)

# test_run_all_model("input_data/M02_EnergyPlus_Forcing_Historical_LowRes/USA_CA_Los.Angeles.Intl.AP.722950_TMY3.epw", "testrun")

# run all models using the same weather file as PNNL reference model
# test_run_all_model("input_data/scorecards/USA_TX_El.Paso_.Intl_.AP_.722700_TMY3.epw", "using_el_paso")

# run all models using the same weather file as NREL reference model
# test_run_all_model("input_data/scorecards/3B_USA_NV_LAS_VEGAS_TMY2.epw", "using_LA_tmy2", "Pre-1980")
# test_run_all_model("input_data/scorecards/3B_USA_NV_LAS_VEGAS_TMY2.epw", "using_LA_tmy2", "pre-1980")

# run all models using the same weather file as ResStock and ComStock
test_run_all_model("intermediate_data/resstock/G0600370_tmy3.epw", "using_G0600370", "")

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

def run_multi_thread(n_thread):
    sim_to_run = df[df['idf.name'].isin(idfs)]
    k = len(sim_to_run) // n_thread
    dfs = [sim_to_run.iloc[k*i:k*(i+1), :] for i in range(n_thread - 1)]
    dfs.append(sim_to_run.iloc[k*(n_thread - 1):max(k*n_thread, len(sim_to_run)), :])

    Parallel(n_jobs=n_thread)(delayed(run_sim_with_idf_epw_df)(dfs[i]) for i in range(n_thread))

run_multi_thread(4)

# if needs rerun certain idf-epw combination, do the following
combos = ["MultiFamily-pre-1980____53", "Religious-pre-1980-3B____53",
          "Religious-post-1980-3B____93"]

for combo in combos:
    shutil.rmtree(os.path.join(os.getcwd(), "{}/{}".format(dirname, combo)))

run_multi_thread(5)
