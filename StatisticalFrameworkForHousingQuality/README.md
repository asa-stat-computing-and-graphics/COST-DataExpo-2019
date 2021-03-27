# A statistical framework for analyzing housing quality: A case study of New York City

Authors: Jacob Gerszten and Damien Chambon

This folder contains code, files, etc. for a paper titled "_A statistical framework for analyzing housing quality: A case study of New York City_" by Jacob Gerszten and Damien Chambon, which was presented at JSM 2019 during the Data Challenge Expo.

## Research

**Abstract** The physical condition of a person’s home plays a significant role in determining the dweller’s overall quality of life. This paper provides a statistical framework for measuring housing quality in an urban area through a standardized index. Using demographic, geographic, and economic factors from the New York City Housing and Vacancy Survey, this index is constructed using principal com- ponent analysis. Differences in housing quality based upon ownership status were tested and demonstrated that renters face more housing quality issues than owners. Several of the variables driving these differences were found to have varying effects on housing quality over time, in part due to the 2008 financial crisis. Using this novel statistical framework, housing quality indices can be constructed for other cities to investigate housing disparities and inform policies aimed at improving overall quality of life for urban residents.

The final version of the paper can be found in `submitted_version.pdf`.

## Data

The raw data from the New York City Housing and Vacancy Survey is located in `data/dataset_raw.csv`. The processed data, which can be obtained after running `src/initial_cleaning.py`, is directly available in `data/dataset_processed.csv`.

The other datasets, which include `data/renter_vs_owner.csv` and `data/allcoeffs.csv`, correspond to the output of `src/analysis.R`.

## Content of the `src` files

The following files are located in the `src` folder.

The files are organized as follows:

1. `initial_cleaning.py`: this file cleans the raw data, creates variables that are used in the analysis, and constructs the Housing Quality Index (HQI) via principal component analysis. It outputs `data/dataset_processed.csv` which is used in the subsequent R scripts. 

2. `processing.R`: this file creates a function, `clean_data()`, that does additional cleaning prior to analysis. It drops variables and observations that we do not use and does transformations on several variables. This function is executed in both `analysis.R` and `graphs.R` scripts for those files to work.

3. `analysis.R`: this file performs the statistical analysis for this paper. First, it outputs basic summary statistics for the HQI as well as other variables from the housing data. Second, it divides the data into two separate groups for renters and owners. Third, it runs the regressions for the entire sample, then only renters, and only owners. It also runs the regressions with the HQI as a target variable for each of the year in the dataset, separately. Finally, the script extracts the coefficients from the regressions for comparison. They can be found in `data/renter_vs_owner.csv` and `data/allcoeffs.csv`.

4. `graphs.R`: this file creates the majority of the graphs from the paper, including the HQI histogram and density plots, the various distribution plots of important variables, and the correlation matrix. Graphs are then saved in the `figures` subfolder.

