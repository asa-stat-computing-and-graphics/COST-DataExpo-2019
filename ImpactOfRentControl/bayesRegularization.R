
library(tidyverse)
library(ggfortify)
library(fitdistrplus)
library(dplyr)

load("aggmodel.Rdata")

aggmodel <- aggmodel %>%
  mutate(RentControl=factor(RentControl),
         Quartile = factor(Quartile),
         borough = factor(borough))


## "Fix" the three proportions
##
## We use an Empirical Bayes Regularization
##
## External
tmp <- aggmodel %>%
  filter(PctDamage.External > 0 & PctDamage.External < 1) %>%
  dplyr::select(PctDamage.External)

beta.parms <- fitdist(tmp$PctDamage.External, "beta", method="mme")$estimate
aggmodel <- aggmodel %>%
  mutate(PctDamage.External.Bayes = (numHouses)/(numHouses+sum(beta.parms))*PctDamage.External + sum(beta.parms)/(numHouses+sum(beta.parms))*beta.parms[1]/sum(beta.parms) )

## Now Utility
tmp <- aggmodel %>%
  filter(PctDamage.UtilityDamage > 0 & PctDamage.UtilityDamage < 1) %>%
  dplyr::select(PctDamage.UtilityDamage)

beta.parms <- fitdist(tmp$PctDamage.UtilityDamage, "beta", method="mme")$estimate
aggmodel <- aggmodel %>%
  mutate(PctDamage.UtilityDamage.Bayes = (numHouses)/(numHouses+sum(beta.parms))*PctDamage.UtilityDamage + sum(beta.parms)/(numHouses+sum(beta.parms))*beta.parms[1]/sum(beta.parms) )

## Fit Pest
tmp <- aggmodel %>%
  filter(PctDamage.PestPresent > 0 & PctDamage.PestPresent < 1) %>%
  dplyr::select(PctDamage.PestPresent)

beta.parms <- fitdist(tmp$PctDamage.PestPresent, "beta", method="mme")$estimate
aggmodel <- aggmodel %>%
  mutate(PctDamage.PestPresent.Bayes = (numHouses)/(numHouses+sum(beta.parms))*PctDamage.PestPresent + sum(beta.parms)/(numHouses+sum(beta.parms))*beta.parms[1]/sum(beta.parms) )

### Fix factor levels for later
aggmodel <- aggmodel %>%
  mutate(borough = factor(borough, 1:5, labels=c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")),
         Quartile=factor(Quartile, 1:4, labels=c("First Quartile", "Second Quartile", "Third Quartile", "Fourth Quartile")),
         RentControl = factor(RentControl, 0:1, labels=c("No", "Yes")),
         YearBuild = factor(YearBuild, c("Pre-47", "47-79", "80+")))

## Done
## Sanity check
glimpse(aggmodel)

save(aggmodel, file="dataAfterBayesFix.RData")
