library(tidyverse)
library(RColorBrewer)

load("dataAfterBayesFix.RData")


#rent.control.col <- brewer.pal(n=3, name="Dark2")[1:2]
rent.control.col <- c("darkblue", "darkorange")


#####################################
##
## Damage in time
##
#####################################
borough <- aggmodel %>%
  dplyr::filter(borough == "Manhattan") %>%
  group_by(year, RentControl) %>%
  summarize(Q1 = quantile(PctDamage.UtilityDamage, prob=0.25)*100,
            Median = median(PctDamage.UtilityDamage)*100,
            Q3 = quantile(PctDamage.UtilityDamage, prob=0.75)*100) %>%
  mutate(year = year+1990)


utlDamageTime <- ggplot(borough) + 
  geom_ribbon(aes(x=year, ymin=Q1, ymax=Q3, fill=RentControl),
              alpha=0.35) +
  geom_line(aes(x=year, y=Median, color=RentControl, linetype=RentControl), size=1.15) +
  geom_point(aes(x=year, y=Median, color=RentControl)) +
  scale_x_continuous(name="", breaks=c(1991,1993,1996,1999,2002,2005,2008,2011,2014,2017)) +
  scale_fill_manual(name="Under Rent Control?", values=rent.control.col) +
  scale_color_manual(name="Under Rent Control?", values=rent.control.col) +
  scale_linetype_manual(name="Under Rent Control?", values=c(1,2)) + 
  scale_y_continuous(limits=c(0,100) ) + 
  labs(y="% of Homes",
       x="",
       title="Utility Damage Rates in Time",
       subtitle="Manhattan Sub-borough Damage Rates from 1991-2017") +
  theme_bw() + 
  theme(legend.position="bottom")

utlDamageTime


#####################################
##
## All external damages by Borough
##  and Quartile
##
#####################################
extDamageFacet <- ggplot(aggmodel) + 
  geom_violin(aes(x=borough, y=PctDamage.External*100, fill=RentControl), 
              alpha=0.4, position=position_dodge(width = 0.75),
              color="gray45") +
  geom_boxplot(aes(x=borough, y=PctDamage.External*100, color=RentControl), 
               fill="gray95", width=0.15, 
               position=position_dodge(width = 0.75), outlier.colour=NA) +
  facet_grid(Quartile~.) +
  scale_color_manual(name="Under Rent Control?", values=rent.control.col) +
  scale_fill_manual(name="Under Rent Control?", values=rent.control.col) +
  scale_y_continuous(limits=c(0, 100)) + 
  labs(x="", y="% of homes",
       title="Comparison of External Damage Rates in New York City",
       subtitle="Grouped by Rent Control status and Faceted by Rent Quartile") + 
  theme_bw() +
  theme(legend.position="bottom")

extDamageFacet




#####################################
##
## Density plots showing Bayes regularization
##
#####################################

densplot_data <- aggmodel %>%
  dplyr::select(c(PctDamage.External, PctDamage.External.Bayes,
                  PctDamage.UtilityDamage, PctDamage.UtilityDamage.Bayes,
                  PctDamage.PestPresent, PctDamage.PestPresent.Bayes)) %>%
  pivot_longer(starts_with("Pct")) %>%
  mutate(Damage = case_when(name=="PctDamage.External" ~ "External",
                            name=="PctDamage.External.Bayes" ~ "External",
                            name=="PctDamage.UtilityDamage" ~ "Utility",
                            name=="PctDamage.UtilityDamage.Bayes" ~ "Utility",
                            name=="PctDamage.PestPresent" ~ "Pest",
                            name=="PctDamage.PestPresent.Bayes" ~ "Pest"),
         Data = case_when(name=="PctDamage.External" ~ "Raw data",
                          name=="PctDamage.External.Bayes" ~ "Regularized",
                          name=="PctDamage.UtilityDamage" ~ "Raw data",
                          name=="PctDamage.UtilityDamage.Bayes" ~ "Regularized",
                          name=="PctDamage.PestPresent" ~ "Raw data",
                          name=="PctDamage.PestPresent.Bayes" ~ "Regularized"),
         value = value*100)

denRegPlot <- ggplot(densplot_data) + 
  geom_density(aes(x=value, fill=Data), alpha=0.4, color="gray60" ) +
  facet_grid(.~Damage) +
  scale_fill_manual(values=c("aquamarine", "springgreen4")) +
  labs(x="", y="", title="Sub-borough Damage Rates",
       subtitle="Visualization of Bayesian Regularization",
       fill="") + 
  theme_bw() + 
  theme(legend.position="bottom")

denRegPlot



ggsave(utlDamageTime, filename="utilityDamageInTime.png", 
       width=8, height=4.5)
ggsave(extDamageFacet, filenam="externalDamageByGroup.png",
       width=8, height=8)
ggsave(denRegPlot, filename="densityRegularization.png", 
       width=8, height=4.0)

###################################
##
## Now fitted model plots
##
## First a pairs plot of the transformed response
##   justifies the chosen model
###################################
load("dataAfterModel.Rdata")

library(GGally)
responsePairsPlot <- ggpairs(aggmodel, columns=14:16, 
        ggplot2::aes(colour=RentControl, alpha=0.25),
        columnLabels=c("Log Odds External", "Log Odds Utility", "Log Odds Pest")) +
  scale_color_manual(name="Under Rent Control?", values=rent.control.col) +
  scale_fill_manual(name="Under Rent Control?", values=rent.control.col) +
  theme_bw()

responsePairsPlot
ggsave(responsePairsPlot, filename="logOddsPairs.png",
       width=6.5, height=6)

####################################
## 
## Confidence intervals for the 
##   predicted percentage of damage
##
####################################
source("predictionIntervals.R")

fake_data <- as.data.frame(expand.grid(unique(aggmodel$RentControl), unique(aggmodel$Quartile), unique(aggmodel$borough)))
names(fake_data) <- c("RentControl", "Quartile", "borough")
fake_data$year <- 27  ## for 2017

pred <- predict(big.mod2, newdata=fake_data)
moe <- predictionIntervals(big.mod2, newdata=fake_data,
                           type="confidence")

fake_data_pred <- cbind(fake_data, pred)
fake_data_ints <- cbind(fake_data, pred-moe, pred+moe)

names(fake_data_ints)[5:7] <- paste0(names(fake_data_pred)[5:7],".lo")
names(fake_data_ints)[8:10] <-paste0(names(fake_data_pred)[5:7],".hi")

fake_data_pred <- fake_data_pred %>%
  mutate(across(5:7, function(x) { 100*(exp(x)/(1+exp(x)))}))
fake_data_ints <- fake_data_ints %>%
  mutate(across(5:10, function(x) { 100*(exp(x)/(1+exp(x)))}))


fake_data_pred_tall <- fake_data_pred %>%
  pivot_longer(-c(RentControl, Quartile, borough, year)) %>%
  mutate(name = str_remove(name, "logodds."),
         name = factor(name, c("external", "utility", "pest"),
                       labels=c("External", "Utility", "Pest")))
fake_data_ints_tall <- fake_data_ints %>%
  pivot_longer(-c(RentControl, Quartile, borough, year)) %>%
  mutate(int = str_sub(name, -2),
         name = str_remove(name, "logodds."),
         name = str_remove(name, ".lo"),
         name = str_remove(name, ".hi")) %>%
  pivot_wider(names_from=int, values_from=value) %>%
  mutate(name = factor(name, c("external", "utility", "pest"),
                       labels=c("External", "Utility", "Pest")))


modelPredict <- ggplot(fake_data_ints_tall) + 
  geom_errorbar(aes(y=borough, xmin=lo, xmax=hi, color=RentControl),
                position=position_dodge(0.7)) +
  geom_point(data=fake_data_pred_tall, aes(y=borough, x=value, color=RentControl),
             position=position_dodge(0.7)) +
  facet_grid(Quartile~name) +
  scale_color_manual(name="Under Rent Control?", values=rent.control.col) +
  labs(x="Predicted % of Homes with Damage", y="") +
  theme_bw() +
  theme(legend.position="bottom")

ggsave(modelPredict, filename="expectedPercentage.png",
       width=6, height=6)
