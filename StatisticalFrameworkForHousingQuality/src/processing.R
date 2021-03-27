# A Statistical framework for analyzing housing quality: a case study of New York City
# Damien Chambon and Jacob Gerszten
# Data processing file

clean_data <- function(filename) {
  original_dataset <- read.csv(filename, header=TRUE, sep=",")
  
  inputdata <- original_dataset #inputdata will be used for the entire analysis
  
  #Remove variables used to create HQI since they will not be used individually in the analysis
  inputdata <- subset(inputdata, select = -c(Severity.walls, Severity.windows, 
                                             Severity.stairways, Severity.floors,General.building.condition, 
                                             Toilets.breakdowns, Kitchen.functioning,Heating.breakdowns, 
                                             Mice.and.rats, Holes.in.floors, Broken.plaster,Water.leakage,pca))

  #Remove variables that are not used in the analysis for different reasons (too many levels that are complex to merge, too many NaNs)
  inputdata <- subset(inputdata, select = -c(Householder.race,Number.of.people))
  inputdata <- subset(inputdata, Householder.hispanic.origin!="8" & 
                        Plumbing.facilities!="8" & Kitchen.facilities!="8" & 
                        Householder.sex!="8") #removes 200 observations
  
  # additional cleaning for regressions
  inputdata$Number.of.stories[inputdata$Number.of.stories == "1 story"] <- "1-2 stories"
  inputdata$Number.of.stories[inputdata$Number.of.stories == "2 stories"] <- "1-2 stories"
  inputdata$Number.of.units[inputdata$Number.of.units == "100-199 units"] <- "100+ units"
  inputdata$Number.of.units[inputdata$Number.of.units == "200+ units"] <- "100+ units"
  inputdata$Year = paste(inputdata$Year)
  
  # log transformation of data that needs to be transformed
  # and extraction of the lists of household value and monthly rent without 9999999
  inputdata$Duration.of.stay.as.of.2017 <- log(inputdata$Duration.of.stay.as.of.2017+1)
  list_household_value <- inputdata$Household.value
  list_household_value <- list_household_value[! list_household_value %in% c(9999999)]
  list_household_value <- log(list_household_value+1)
  inputdata$Household.value <- log(inputdata$Household.value+1)
  list_monthly_rent <- inputdata$Monthly.rent
  list_monthly_rent <- list_monthly_rent[! list_monthly_rent %in% c(99999)]
  list_monthly_rent <- log(list_monthly_rent+1)
  inputdata$Monthly.rent <- log(inputdata$Monthly.rent+1)
  inputdata$Householder.income <- log(inputdata$Householder.income+1)
  
  # remove NaN produced (157 obs) and unused levels
  inputdata <- na.omit(inputdata)
  inputdata <- droplevels(inputdata)
  
  # Creation of the other datasets (used for regression on only renters, or only owners)
  inputdata_noHval_noRent = subset(inputdata, select = -c(Household.value,Monthly.rent))
  inputdata_withHval_noRent = subset(inputdata, select = -c(Monthly.rent))
  inputdata_noHval_withRent = subset(inputdata, select = -c(Household.value))
  
  # Save the clean datasets
  my_list <- list("original_dataset"=original_dataset,"inputdata"=inputdata,"noHval_noRent"=inputdata_noHval_noRent,"withHval_noRent"=inputdata_withHval_noRent,"noHval_withRent"=inputdata_noHval_withRent,"list_household_value"=data.frame(list_household_value),"list_monthly_rent"=data.frame(list_monthly_rent))
  return(my_list)
}
