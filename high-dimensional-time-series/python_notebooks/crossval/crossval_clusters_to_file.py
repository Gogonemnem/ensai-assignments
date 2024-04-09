"""
This scripts runs the crossval against all clusters in the JSON file and writes the MSEs 
of the validation folds and the test fold to a .csv.
"""
import pandas as pd
import seaborn as sns
import numpy as np
from sklearn.linear_model import RidgeCV
from sklearn.preprocessing import SplineTransformer
from tqdm import tqdm
from models import Model, ARRidgeModel, SparseVAR, PerClusterModel
from utils import cluster_convert
import seaborn as sns
from sklearn.model_selection import KFold, TimeSeriesSplit
from joblib import Parallel, delayed
from itertools import product
import json
import os
os.environ['R_HOME'] = '/usr/lib/R'

from rpy2.robjects import pandas2ri, r
from rpy2.robjects.packages import importr
pandas2ri.activate()

sparsevar = importr('sparsevar')
rrpack = importr('rrpack')

df = pd.read_csv("../../assets/TaiwanAirBox032017.csv").set_index("time")

locations = pd.read_csv("../../assets/locations032017.csv", index_col=0)
locations.index = "V" + locations.index.astype(str)
locations.index.name = "device"

# # 2.1 Remove seasonality
def periodic_spline_transformer(period, n_splines=None, degree=3):
    if n_splines is None:
        n_splines = period
    n_knots = n_splines + 1  # periodic and include_bias is True
    return SplineTransformer(
        degree=degree,
        n_knots=n_knots,
        knots=np.linspace(0, period, n_knots).reshape(n_knots, 1),
        extrapolation="periodic",
        include_bias=True,
    )

N_SPLINES = 5

df_hour = df.assign(hour=(np.arange(len(df)) % 24))
splines = periodic_spline_transformer(24, n_splines=N_SPLINES).fit_transform(df_hour[["hour"]])
splines_df = pd.DataFrame(
    splines,
    columns=[f"spline_{i}" for i in range(splines.shape[1])],
)
df_hour_spline = df_hour.merge(splines_df, how = "left", left_on = "hour", right_index = True).drop(columns = "hour")

r2s = []
residuals = []
for target_col in df_hour_spline.columns[df_hour_spline.columns.str.startswith("V")]:
    # get ridgecv r^2
    ridgecv = RidgeCV()
    ridgecv.fit(df_hour_spline[["spline_" + str(i) for i in range(N_SPLINES)]], df_hour_spline[target_col])
    r2s.append(ridgecv.score(df_hour_spline[["spline_" + str(i) for i in range(N_SPLINES)]], df_hour_spline[target_col]))
    # get residuals
    residuals.append(df_hour_spline[target_col] - ridgecv.predict(df_hour_spline[["spline_" + str(i) for i in range(N_SPLINES)]]))

r2s = pd.Series(r2s, index = df_hour_spline.columns[df_hour_spline.columns.str.startswith("V")])
residuals = pd.concat(residuals, axis = 1)
df = residuals

# # 3. Crossval
TEST_FRAC = 0.2
n_train = int(len(df) * (1 - TEST_FRAC))
n_test = len(df) - n_train
df_train, df_test = df.iloc[:n_train], df.iloc[n_train:]

# # 3.2. Cross-val grid search



with open("../../out/clustering_results.json", "r") as f:
    clustering_results = json.load(f)

clustering_results = pd.DataFrame(clustering_results)

# %%
CLUSTER_TYPE = "gcc" # acf | cc | gcc
CLUSTER_INDEX = 5 # [0, len(clustering_results[CLUSTER_TYPE]))
CLUSTER_METHODOLOGY = "kmedoids" # kmedoids | hierarchical

cluster_info = pd.Series(clustering_results.loc[CLUSTER_INDEX, CLUSTER_TYPE])
print(cluster_info)
if CLUSTER_METHODOLOGY == "kmedoids":
    cluster = cluster_info["labels_kmedoids"]
elif CLUSTER_METHODOLOGY == "hierarchical":
    cluster = cluster_info["labels_hierarchical"]
else:
    raise ValueError("CLUSTER_METHODOLOGY must be kmedoids or hierarchical")

assert len(cluster) == len(df_train.columns), "Cluster length does not match number of columns in df_train"

# %% [markdown]
# # 4.4. Sparse VAR model

# %%
model = SparseVAR(lookback = 1, horizon = 1)

N_SPLITS = 3
cv = TimeSeriesSplit(N_SPLITS, test_size = (len(df) - 5) // 10)

grid = {
    "cluster_type": ["gcc", "cc", "acf"],
    "cluster_index": range(len(clustering_results["gcc"])), # same amount of clusters per type
    "cluster_methodology": ["kmedoids", "hierarchical"]
}

# We iterate over all permutations
for hyperparams in 

