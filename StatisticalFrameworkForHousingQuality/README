This folder contains code, files, etc. for a paper titled
"A statistical framework for analyzing housing quality: A case study of New York City"
by Jacob Gerszten and Damien Chambon.

The paper describes their entry to the Data Challenge Expo, which was presented at JSM 2019.

The final version of the paper is saved as "submitted_version.pdf".

The raw data from the New York City Housing and Vacancy Survey is located in data/dataset_raw.csv. 
The following files are located in the src folder.

The files are organized as follows:
1. initial_cleaning.py: this file cleans the raw data, creates variables that are used in the analysis, 
and constructs the Housing Quality Index via principal component analysis. It outputs data/dataset_final.csv 
which is used in the subsequent R scripts. 

2. processing.R: this file creates a function called "clean_data()" that does additional cleaning prior to analysis. 
It drops variables and observations that we do not use and does transformations on several variables. 
This function must be run in both the analysis.R and graphs.R scripts for those files to work.

3. analysis.R: this file performs the statistical analysis for this paper. First, it outputs basic summary statistics
for the HQI as well as other variables from the housing data. Second, it divides the data into two separate groups 
for renters and owners. Third, it runs the regressions for the entire sample, renters, and owners. 
Finally, it extracts the coefficients from the regressions for comparison.

4. graphs.R: this file creates the majority of the graphs from the paper, including the HQI histogram and density plots,
the various distribution plots of important variables, and the correlation matrix.   

