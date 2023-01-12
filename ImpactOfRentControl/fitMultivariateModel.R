
source("mlm_aic.R")

load("dataAfterBayesFix.RData")

aggmodel <- aggmodel %>%
  mutate(logodds.external = log(PctDamage.External.Bayes/(1-PctDamage.External.Bayes)),
         logodds.utility = log(PctDamage.UtilityDamage.Bayes/(1-PctDamage.UtilityDamage.Bayes)),
         logodds.pest = log(PctDamage.PestPresent.Bayes/(1-PctDamage.PestPresent.Bayes)) )

glimpse(aggmodel)
### The tranformation should make the response fairly "normal"
qqnorm(aggmodel$logodds.external)
qqline(aggmodel$logodds.external)
qqnorm(aggmodel$logodds.utility)
qqline(aggmodel$logodds.utility)
qqnorm(aggmodel$logodds.pest)
qqline(aggmodel$logodds.pest)

### Sure looks like it worked well!

###########################
## By EDA it looked like year may or may not have an effect
## but that it does not interact with Rent Control.
##
## It is also pretty clear that Borough and Rent Control interact
## Likewise Quartile and Rent Control appear to interact
## From context we would expect Borough & Quartile to interact 
##   (a second quartile home in Manhatten compared to second 
##    quartile in the Bronx)
## But not sure if all three interact, so we consider it
##
## But we start with a big interaction model

big.mod1 <- lm(cbind(logodds.external, logodds.utility, logodds.pest ) ~
                 RentControl*Quartile*borough*year, data=aggmodel)

## Separate the time element
big.mod2 <- lm(cbind(logodds.external, logodds.utility, logodds.pest ) ~
                 RentControl*Quartile*borough + year, data=aggmodel)


## Time and rent control interact
big.mod3 <- lm(cbind(logodds.external, logodds.utility, logodds.pest ) ~
                 RentControl*Quartile*borough + year + year:RentControl, data=aggmodel)
## remove the time effect all together
big.mod4 <- lm(cbind(logodds.external, logodds.utility, logodds.pest ) ~
                 RentControl*Quartile*borough, data=aggmodel)

aic_mlm(big.mod1)
aic_mlm(big.mod2)
aic_mlm(big.mod3)
aic_mlm(big.mod4)
### So from this, it looks like we should include time
## as a non-interactive influence. 


### Only Rent Control & Quartile Interact
big.mod5 <- lm(cbind(logodds.external, logodds.utility, logodds.pest ) ~
                 RentControl*Quartile + year + borough, data=aggmodel)

### Only Rent Control & Borough interact
big.mod6 <- lm(cbind(logodds.external, logodds.utility, logodds.pest ) ~
                 RentControl*borough + Quartile + year, data=aggmodel)

## Only borough & quartile interact, rent control on its own
big.mod7 <- lm(cbind(logodds.external, logodds.utility, logodds.pest ) ~
                 RentControl + borough*Quartile + year, data=aggmodel)

aic_mlm(big.mod2)
aic_mlm(big.mod5)
aic_mlm(big.mod6)
aic_mlm(big.mod7)

## The model with the three way interaction looks to be 
## the best fitting this far. Let's try a few others
## 
## Here Rent Control interacts with Borough and Quartile
## but those two do not.
big.mod8 <- lm(cbind(logodds.external, logodds.utility, logodds.pest ) ~
                 RentControl*(borough+Quartile) + year, data=aggmodel)
aic_mlm(big.mod2)
aic_mlm(big.mod8)

## Try three two-way interactions but no three-way interaction term
big.mod9 <- lm(cbind(logodds.external, logodds.utility, logodds.pest ) ~
                 RentControl + borough + Quartile + year +
                 RentControl:borough + RentControl:Quartile + borough:Quartile,
               data=aggmodel)
aic_mlm(big.mod2)
aic_mlm(big.mod9)

## The three way interaction model is still best.
##
## It seems clear this interaction should be included but
## what if we have a full main effects model
big.mod10 <- lm(cbind(logodds.external, logodds.utility, logodds.pest ) ~
                 RentControl + borough+Quartile + year, data=aggmodel)
aic_mlm(big.mod2)
aic_mlm(big.mod10)

## So yes, surely the interaction model...

summary(big.mod2)
cor(residuals(big.mod2))

library(car)
Anova(big.mod2, type=2)
save(big.mod2, aggmodel, file = "dataAfterModel.Rdata")


