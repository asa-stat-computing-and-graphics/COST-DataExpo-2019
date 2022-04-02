# House Quality Index Construction and Rent Prediction in New York City with Interactive Visualization and Product Design

Authors: Xiang Shen, Shunyan Luo, Mingze Zhang

Abstract: Housing is of primary importance for immigrants in New York City. This study analyzed the housing conditions of and price changes for residents in New
York City from the NYCHVS survey over the past 30 years. First, a house condition index is defined through dimension reduction approach with a supervised framework. In addition, spatio-temporal information is leveraged to build a two-stage model to predict rent. Data visualization is utilized to show immigrant preferences interactively and provide information for both researchers and new residents to the city.

## About this repo
## Data generation
We conduct extensive data preprocessing before analyzing and visualizaing the data. The raw data and data cleaning script are in 'data generation' folder.

## Replication of the paper
To replicate the figures and tables in the paper, please load 'R1.RData' and run the plot functions.
1. 'composite indicator.R' is the R file to construct house quality index.
-  Run line 18 and 28 to obtain result of Table 1
-  Run line 31 to obtain Figure 1
-  Run line 36 to obtain Figure 2

2. 'spatial.R' fits a spatio temporal model to predict the rent. The data are also included in 'R1.RData'.
- Run line 1 to install 'SpatioTemporal' package.
- Run line 68, 69 to obtain Figure 3 and 4
- Run line 103 to obtain Table 2
- Run line 109 to 124 to obtain Figure 5

Meanwhile, read and save intermediate data are commented and those data are saved in the 'intermedia data' folder.

## Shiny app
Our shinyapp is available online https://paradise1008.shinyapps.io/ShinyExpo2019/.
The code and required data 'R2.Rdata' are stored in 'shinyapp' folder. You can also load the data and run 'shinyapp.R' script to launch the application locally.
