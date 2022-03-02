R Code in Support of:
  Housing Variables and Immigration: An Exploratory Analysis in New York City
Authors:
  Jhonatan Jorge Medri, Braden Probst, Juergen Symanzik
Reference:
  Computational Statistics, under Review

Instructions:

Our work contains 5 R Scripts.
We detailed below a brief description of the code and inputs.

0. Review code inputs
- Raw housing data (10 csv files located in the Data_CSV folder). 
This was the data provided for the Data Expo Challenge. 
The Codebook folder caontains additional information.
- Official shapefile docs for New York City (4 files located in nypuma_17 folder).

1. Run R code "1.ObjectStep.R".
Code needs the directory line to be updated before running anything else.
This code creates 4 key relevant files for later use.
- Optimized shapefile of New York City map (named NYPUMA)
- Aggregated raw housing 30 years of data (named NYCHVSall.csv)
- Aggregated housing data by sub-borough 30 years of data (named NYCHVS55.csv)
- Aggregated housing data by borough 30 years of data (named NYCHVS05.csv)

2. Run R code "2.StatDepStep.R".
Code needs the directory line to be updated before running anything else.
This code performs a spatial autocorrelation analysis in the data.

3. Run R code "3.DataVisStep.R"
Code needs the directory line to be updated before running anything ele.
This code produces functions to perform the following visualizations:
- Choropleth maps
- Linked micromap plots for three variables
- Smoothed scatterplots for two variables
- Box-dot plots (Boxes based on the raw data and dots on sub-borough medians)
It also provides the summary statistics and correlations information.

4. R code "4.ShinyRSource.R" (No action required)
Code needs the directory line to be updated before running anything else.
Code acts as a source for the Shiny R app that builds functions.

5. Run R code "5.ShinyRInterface.R"
Code needs the directory line to be updated before running anything else.
This code produces the following features in the Shiny R app:
- Choropleth maps
- Linked micromap plots for up to three variables
- Smoothed scatterplots for two variables
- Spatial dependence tests


There are two subtle differences between the results from the R code and the manuscript:
One of them involves the calculation of the Rent and Income values (Table 3) and the other one
involves the calculation of the Immigration Percentages (Table 4). Those values will be
adjusted in the next revised version of the manuscript to match the results from the R code. 
Neither of these numerical changes has an impact on the interpretation of the results in the manuscript.