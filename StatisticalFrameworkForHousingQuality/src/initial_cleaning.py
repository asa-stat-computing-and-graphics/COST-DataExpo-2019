# -*- coding: utf-8 -*-
# File created by Damien Chambon and Jacob Gerszten
# Data Challenge Expo 2019

import pandas as pd
import numpy as np
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import seaborn as sns; sns.set()

# We read the dataset as a Pandas DataFrame (easier for analysis)
df = pd.read_csv('dataset.csv')
df = df.drop(columns=['index'])

# we work with the walls variables
walls = df[["_d3","_d4"]]
# we create a column for severity_wall
col = pd.DataFrame({'severity_walls': []})
walls = walls.join(col)
walls = walls.drop(walls[walls["_d3"] == 8 ].index) # "8" represents the absence of response
walls = walls.replace(9,0)
walls['severity_walls'] = walls['_d3'] + walls['_d4']

# we work with the windows variables
windows = df[["_e1","_e2","_e3"]]
# we create a column for severity_windows
col = pd.DataFrame({'severity_windows': []})
windows = windows.join(col)
windows = windows.drop(windows[windows["_e1"] == 8 ].index) # "8" represents the absence of response
windows = windows.replace(9,0)
windows['severity_windows'] = windows['_e1'] + windows['_e2'] + windows['_e3']

# we work with the stairways variables
stairways = df[["_f1","_f2"]]
# we create a column for severity_stairways
col = pd.DataFrame({'severity_stairways': []})
stairways = stairways.join(col)
stairways = stairways.drop(stairways[stairways["_f1"] == 8 ].index) # "8" represents the absence of response
stairways = stairways.replace(9,0)
stairways['severity_stairways'] = stairways['_f1'] + stairways['_f2']

# we work with the floors variables
floors = df[["_g3","_g4",]]
# we create a column for severity_floors
col = pd.DataFrame({'severity_floors': []})
floors = floors.join(col)
floors = floors.drop(floors[floors["_g3"] == 8 ].index) # "8" represents the absence of response
floors = floors.replace(9,0)
floors['severity_floors'] = floors['_g3'] + floors['_g4']

# concatenate the severity for the features in a new Dataframe
severity = pd.concat([walls["severity_walls"],windows["severity_windows"],stairways['severity_stairways'],floors['severity_floors']], axis=1)
severity = severity.dropna()
severity = severity.apply(np.int64)


# we create a Pandas DataFrame that stores all the housing conditions
df_cond = pd.DataFrame()
df_cond["Severity walls"] = severity["severity_walls"]
df_cond["Severity windows"] = severity["severity_windows"]
df_cond["Severity stairways"] = severity["severity_stairways"]
df_cond["Severity floors"] = severity["severity_floors"]
df_cond["General building condition"] = df["_h"]
df_cond["Toilets breakdowns"] = df["_25c"]
df_cond["Kitchen functioning"] = df["_26c"]
df_cond["Heating breakdowns"] = df["_32a"]
df_cond["Mice and rats"] = df["_35a"]
df_cond["Holes in floors"] = df["_36b"]
df_cond["Broken plaster"] = df["_37a"]
df_cond["Water leakage"] = df["_38a"]

# we proceed with the principal component analysis on the Dataframe
pca = PCA(n_components=1)
pca.fit(df_cond)
df_cond_pca = pca.transform(df_cond)
df_cond["pca"] = df_cond_pca

# we scale the obtained pca scores between 0 and 10
df_cond["pca_scaled"] = np.interp(df_cond["pca"], (df_cond["pca"].min(), df_cond["pca"].max()), (0, 10))


# we create a Pandas Dataframe that contains the predictors used in the analysis
df_pred= pd.DataFrame()
df_pred["Householder sex"] = df["_1b"]
df_pred["Householder age"] = df["_1c"]
df_pred["Householder hispanic origin"] = df["_1e"]
df_pred["Householder race"] = df["_1f"]
df_pred["Duration of stay as of 2017"] = df["_4a"]
df_pred["Household value"] = df["_13"]
df_pred["Number of units"] = df["_20"]
df_pred["Owner in building"] = df["_21"]
df_pred["Number of stories"] = df["_22a"]
df_pred["Number of rooms"] = df["_24a"]
df_pred["Number of bedrooms"] = df["_24b"]
df_pred["Plumbing facilities"] = df["_25a"]
df_pred["Kitchen facilities"] = df["_26a"]
df_pred["Length of lease"] = df["_29"]
df_pred["Monthly rent"] = df["_30a"]
df_pred["Resident rating"] = df["_39"]
df_pred["Householder income"] = df["hhinc"]
df_pred["Year"] = df["year"]
df_pred["Borough"] = df["borough"]
df_pred["Number of people"] = df["npr"]

# creating the status column
list_statuses = []
for elem in df_pred["Length of lease"]:
    if elem == "Owner-occupied":
        list_statuses.append('Owner')
    else:
        list_statuses.append('Renter')
df_pred["Status"] = list_statuses
df_pred

# we create a Pandas Dataframe that contains the predictors used in the analysis, 
# the housing conditions and the PCA scores
final_df = pd.DataFrame()
final_df = df_pred.join(df_cond)

# exporting of the dataset
final_df.to_csv(r'dataset_final.csv')
