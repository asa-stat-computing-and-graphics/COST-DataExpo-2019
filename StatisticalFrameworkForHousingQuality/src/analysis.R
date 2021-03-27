# A Statistical framework for analyzing housing quality: a case study of New York City
# Damien Chambon and Jacob Gerszten
# Statistical analysis file

library(data.table)

source("src/processing.R")

##################
## DATA LOADING ##
##################

# reading in the data
# IMPORTANT: set working directory to the root folder 
# where all the files are located with the setwd command
# or from the RStudio menu directly

datasets <- clean_data('data/dataset_processed.csv')

##############
## ANALYSIS ##
##############

## FIRST SECTION ##

get_statistics <- function(x) {
  cat('Mean:',mean(x),'\n')
  cat('Std. Dev.:',sqrt(var(x)),'\n')
  cat('Min:',min(x),'\n')
  cat('Q1:',summary(x)[2],'\n')
  cat('Median:',summary(x)[3],'\n')
  cat('Q1:',summary(x)[5],'\n')
  cat('Max:',max(x),'\n')
}

get_statistics(datasets$original_dataset$pca_scaled) # all observations
get_statistics(subset(datasets$original_dataset, c(Length.of.lease=="Owner-occupied"))$pca_scaled) #owners
get_statistics(subset(datasets$original_dataset, c(Length.of.lease!="Owner-occupied"))$pca_scaled) #renters

# nb of rooms and nb of bedrooms are strongly correlated so removal of bedrooms
datasets$original_dataset <- subset(datasets$original_dataset, select = -c(Number.of.bedrooms))
datasets$inputdata <- subset(datasets$inputdata, select = -c(Number.of.bedrooms))
datasets$noHval_noRent <- subset(datasets$noHval_noRent, select = -c(Number.of.bedrooms))
datasets$withHval_noRent <- subset(datasets$withHval_noRent, select = -c(Number.of.bedrooms))
datasets$noHval_withRent <- subset(datasets$noHval_withRent, select = -c(Number.of.bedrooms))

## SECOND SECTION ##

pca_owners <- subset(datasets$inputdata, c(Length.of.lease=="Owner-occupied"))$pca_scaled
pca_renters <- subset(datasets$inputdata, c(Length.of.lease!="Owner-occupied"))$pca_scaled

length(pca_owners)
length(pca_renters)

wilcox.test(pca_owners, pca_renters)


## THIRD SECTION ##

# Linear regression for all years
summary(lm(pca_scaled ~ .-Year, data=subset(datasets$noHval_noRent,select=-Length.of.lease)))


## FOURTH SECTION ##

# status as an interaction term to check if it has a significant impact or not
reg_status_interac <- lm(pca_scaled ~ .*Status, data=datasets$noHval_noRent)
summary(reg_status_interac)
anova(reg_status_interac)

# regression with RENTERS only
renter <- subset(datasets$noHval_noRent, c(Length.of.lease!="Owner-occupied"))
renter = droplevels(renter)
renter = subset(renter, select=-c(Status,Length.of.lease))
reg_renter_interac <- lm(pca_scaled ~ .-Year, data=renter)
summary(reg_renter_interac)
renters_coeffs <- summary(reg_renter_interac)$coefficients[,1]

# regression with OWNERS only
owner <- subset(datasets$noHval_noRent, c(Length.of.lease=="Owner-occupied"))
owner = droplevels(owner)
owner = subset(owner, select=-c(Status,Length.of.lease))
reg_owner_interac <- lm(pca_scaled ~ .-Year, data=owner)
summary(reg_owner_interac)
owners_coeffs <- summary(reg_owner_interac)$coefficients[,1]

# combining their coefficients to compare them
renter_vs_owner <- cbind(renters_coeffs,owners_coeffs)
write.csv(renter_vs_owner,'data/renter_vs_owner.csv')


## FIFTH SECTION ##

# year as an interaction term to check if it has a significant impact or not
reg_year_interac <- lm(pca_scaled ~ .*Year, data=datasets$noHval_noRent)
anova(reg_year_interac)

#Finding coefficients for every variable for every year BOTH RENTERS AND OWNERS
all1991c <- summary(lm(pca_scaled ~ . , data=subset(datasets$noHval_noRent, Year=="1991",select=-Year)))$coefficients[,1]
all1993c <- summary(lm(pca_scaled ~ . , data=subset(datasets$noHval_noRent, Year=="1993",select=-Year)))$coefficients[,1]
all1996c <- summary(lm(pca_scaled ~ . , data=subset(datasets$noHval_noRent, Year=="1996",select=-Year)))$coefficients[,1]
all1999c <- summary(lm(pca_scaled ~ . , data=subset(datasets$noHval_noRent, Year=="1999",select=-Year)))$coefficients[,1]
all2002c <- summary(lm(pca_scaled ~ . , data=subset(datasets$noHval_noRent, Year=="2002",select=-Year)))$coefficients[,1]
all2005c <- summary(lm(pca_scaled ~ . , data=subset(datasets$noHval_noRent, Year=="2005",select=-Year)))$coefficients[,1]
all2008c <- summary(lm(pca_scaled ~ . , data=subset(datasets$noHval_noRent, Year=="2008",select=-Year)))$coefficients[,1]
all2011c <- summary(lm(pca_scaled ~ . , data=subset(datasets$noHval_noRent, Year=="2011",select=-Year)))$coefficients[,1]
all2014c <- summary(lm(pca_scaled ~ . , data=subset(datasets$noHval_noRent, Year=="2014",select=-Year)))$coefficients[,1]
all2017c <- summary(lm(pca_scaled ~ . , data=subset(datasets$noHval_noRent, Year=="2017",select=-Year)))$coefficients[,1]

allcoeffs <- cbind(all1991c,all1993c, all1996c,all1999c, all2002c, all2005c,  all2008c, all2011c, all2014c, all2017c)
allcoeffs

colnames(allcoeffs) <- c("1991", "1993", "1996", "1999", "2002", "2005", "2008", "2011", "2014", "2017")

write.csv(allcoeffs,'data/allcoeffs.csv')

