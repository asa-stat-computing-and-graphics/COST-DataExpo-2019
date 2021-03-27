# A Statistical framework for analyzing housing quality: a case study of New York City
# Damien Chambon and Jacob Gerszten
# Graph creation file

source("src/processing.R")

# IMPORTANT: set working directory to the root folder 
# where all the files are located with the setwd command
# or from the RStudio menu directly

library(ggcorrplot)
library(gridExtra)
library(GGally)

datasets <- clean_data('data/dataset_processed.csv')


###Figures###
#Overall distribution (not included in paper)
ggplot(datasets$inputdata, aes(pca_scaled)) + geom_histogram(color = "black", alpha = .7, breaks = seq(0, 10, 1), binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 10, 1),
                     limits= c(-.5, 10),
                     expand = c(0,0)) +
  scale_y_continuous(name= "Count",expand = c(0,0), limits = c(0,60000)) +
  theme_grey() + theme(text = element_text(size = 20))
ggsave("figures/overall_distribution.png")

#Figure 1: HQI counts by ownership status
ggplot(datasets$inputdata, aes(x=pca_scaled, color = Status, fill=Status)) + geom_histogram(color = "black", alpha = .7, breaks = seq(0, 10, 1), binwidth = 1) +
  scale_x_continuous(name="",breaks = seq(0, 10, 1),
                     limits= c(-.5, 10),
                     expand = c(0,0)) +
  scale_y_continuous(name="Count", expand = c(0,0), limits = c(0,60000)) +
  theme_bw()+ theme(text = element_text(size = 20))
ggsave("figures/HQI_counts_by_ownership.png")

#Figure 2: HQI density plot by ownership status
ggplot(data=datasets$inputdata, aes(x = pca_scaled, fill=Status)) + 
  geom_density(alpha =.5, color = "black", adjust =4) +
  scale_x_continuous(name = "",
                     breaks = seq(0, 10, 1),
                     limits= c(0, 10),
                     expand = c(0,0)) +
  scale_y_continuous(name="Density", expand = c(0,0), limits=c(0,1))+
  theme_bw() + theme(text = element_text(size = 20))
ggsave("figures/HQI_density_by_ownership.png")

#Figure 3: Distribution plots
p1 <- ggplot(datasets$inputdata, aes(x=Householder.age)) + 
  geom_density(fill="Black", alpha=0.5) + theme_bw() + scale_y_continuous(name = "Density") +
  scale_x_continuous(name="Householder age")

p2 <- ggplot(datasets$inputdata, aes(x=Duration.of.stay.as.of.2017)) +
  geom_density(fill="Black", alpha=0.5) + theme_bw() + scale_y_continuous(name = "Density") + 
  scale_x_continuous(name="Log Duration of stay as of 2017")
  
p3 <- ggplot(datasets$list_household_value, aes(x=list_household_value),layout.exp = 2) + 
  geom_density(fill="Black", alpha=0.5) + theme_bw()+ scale_y_continuous(name = "Density") +
  scale_x_continuous(name="Log Household value") 

p4 <- ggplot(datasets$inputdata, aes(x=Number.of.rooms)) + 
  geom_histogram(color="Black", fill="Black", alpha=0.5, binwidth = 1) + theme_bw()+ scale_y_continuous(name = "Counts")+
  scale_x_continuous(name="Number of rooms",breaks = seq(0, 10, 1))

p5 <- ggplot(datasets$list_monthly_rent, aes(x=list_monthly_rent)) + 
  geom_density(fill="Black", alpha=0.5) + theme_bw()+ scale_y_continuous(name = "Density") +
  scale_x_continuous(name="Log Monthly rent",labels = scales::comma) 

p6 <- ggplot(datasets$inputdata, aes(x=Householder.income)) + 
  geom_density(fill="Black", alpha=0.5) + theme_bw()+ scale_y_continuous(name = "Density")+
  scale_x_continuous(name="Log Householder income")

distribution_plots <- grid.arrange(p1,p2,p3,p4,p5,p6,ncol=3)
ggsave("figures/distribution_plots.png",distribution_plots)

#Figure 4: Correlation Matrix
ggcorr(datasets$inputdata,label = TRUE,name = "Color \nScale", label_round = 2, hjust = 0.85, layout.exp = 2, size = 4.5,label_size = 4.5) + theme_bw() +
  theme(text = element_text(size = 13))
ggsave("figures/correlation_matrix.png")

