###################################
## this code inputs the processed
##   data (based on the intersections
##   of different regions/sub-boroughts)
##   and makes choropleths and a 
##   scatterplot that highlights the main
##   contributions of the story.

library(tidyverse)

# load("PresentationData.Rdata")
load("CleanedData_Sep25.Rdata")

b_labels <- b_labels %>%
  mutate(lat = ifelse(plotCode==4, 40.70142, lat))

theme_special <-  theme_minimal()+
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y=element_blank(),
        panel.border = element_rect(colour='black', fill=NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.size = unit(1.2, "cm"),
        legend.background = element_rect(colour = NA,fill=NA),
        legend.text = element_text(size=22),
        legend.title = element_text(size=26),
        legend.position = c(0.18,0.7),
        plot.caption = element_text(size=12))

fig1 <- ggplot(b2) + 
  geom_polygon(aes(x=long,y=lat,fill=immigrantPct,group = group),
               color="gray40",alpha=0.7,size=0.5)+ 
  coord_quickmap()+    
  geom_text(data= b_labels, aes(x=long, y=lat, label = name,fontface=2),
            color="black",size=12)+
  theme_special+
  theme(legend.position = c(0.22,0.67))+
  scale_fill_gradient(name="Immigrant\nResidency",limits=c(15,80),
                      breaks=seq(20,80,by=20),
                      labels=paste0(seq(20,80,by=20),"%"),
                      high="#6e016b", low="white")
# +
#   labs(caption="The percentage of immigrant households by sub-borough region.")

fig2 <- ggplot(b2)+  
  geom_polygon(aes(x=long,y=lat,fill=avgTotalHouseholdIncome2,
                   group = group),color="gray40",alpha=0.7,size=0.5)+ 
  coord_quickmap()+ 
  geom_text(data= b_labels, aes(x=long, y=lat, label = name,fontface=2),
            color="black",size=12)+
  theme_special+ 
  #theme(legend.position = c(0.25,0.68))+
  theme(legend.position = c(0.25,0.67))+
  scale_fill_gradient(name="Avg Total\nHousehold Income",
                      limits=c(30000,210000),
                      breaks=seq(30000,210000,by=45000),
                      labels=paste0("$",seq(30,210,by=45),"K"),
                      high="#005a32", low="white")
# +
#   labs(caption = "The average total household income by sub-borough region.")

fig3 <- ggplot(b2)+
  geom_polygon(aes(x=long,y=lat,fill=avgTotalMonthlyRent2,
                   group = group),color="gray40",alpha=0.7,size=0.5)+
  coord_quickmap()+   
  theme_minimal()+ 
  geom_text(data= b_labels, aes(x=long, y=lat, label = name,fontface=2),
            color="black",size=12)+
  theme_special+
  #theme(legend.position = c(0.22,0.69))+
  theme(legend.position = c(0.22,0.67))+
  scale_fill_gradient(name="Avg Total\nMonthly Rent",
                      limits=c(800,3300),
                      breaks=seq(880,3280,by=800),
                      labels=c("<$900","$1.7K","$2.5K","$3.3K"),
                      high="#252525", low="#ffffff")
# +
  # labs(caption="The average total monthly contract rent by sub-borough region.")

fig4 <- ggplot(crimePlot)+ 
  geom_polygon(aes(x=long,y=lat,fill=CrimeCount,group = group),
               color="gray40",alpha=0.7,size=0.5)+
  coord_quickmap()+ 
  theme_minimal()+  
  geom_text(data= b_labels, aes(x=long, y=lat, label = name,fontface=2),
            color="black",size=12)+
  theme_special+  
  #theme(legend.position = c(0.25,0.68))+
  theme(legend.position = c(0.25,0.67))+
  scale_fill_gradient(name="Total Major\nFelony Offenses",
                      limits=c(80,3800),
                      breaks=seq(500,4000,by=1000),
                      labels=c("< 500","1,500","2,500","> 3,500"),
                      high="#cb181d", low="white")
# +
#   labs(caption = "The total number of major felony offenses by precinct region.")

fig5 <- ggplot(eduPlot)+
  geom_polygon(aes(x=long,y=lat,fill=AchievedRate,group = group),
               color="gray40",alpha=0.7,size=0.5)+
  coord_quickmap()+   
  theme_minimal()+
  geom_text(data= b_labels, aes(x=long, y=lat, label = name,fontface=2),
            color="black",size=12)+ 
  theme_special+
  theme(legend.position = c(0.20,0.67))+
  scale_fill_gradient(name="Achievement\nRate",
                      limits=c(0,0.6),
                      breaks=seq(0,0.6,by=0.2),
                      labels=paste0(seq(0,60,by=20),"%"),
                      high="#2171b5", low="white")
# +
#   labs(caption = "The rate of achievement of high school\ngraduates by school district region.")

fig6 <- ggplot(cdPlot)+
  geom_polygon(aes(x=long,y=lat,fill=MedianDeathAge,group = group),
               color="gray40",alpha=0.7,size=0.5)+
  coord_quickmap()+ 
  theme_minimal()+
  geom_text(data= b_labels, aes(x=long, y=lat, label = name,fontface=2),
            color="black",size=12)+
  theme_special+     
  theme(legend.position = c(0.22,0.67))+
  scale_fill_manual(name="Median Death\nAge Range",
                    breaks=c("Age_65_69","Age_70_74",
                             "Age_75_79","Age_80_84",NA),
                    labels=c("65-69","70-74","75-79","80-84","NA"),
                    values = c("#fed98e","#fe9929","#d95f0e",
                               "#993404","white"))
# +
#   labs(caption = "The median death age range by community district region.")

# Plot all of the plots together.

ggsave("fig1.png", fig1, width=8.2, height=8)
ggsave("fig2.png", fig2, width=8.2, height=8)
ggsave("fig3.png", fig3, width=8.2, height=8)
ggsave("fig4.png", fig4, width=8.2, height=8)
ggsave("fig5.png", fig5, width=8.2, height=8)
ggsave("fig6.png", fig6, width=8.2, height=8)

figs <- arrangeGrob(grobs=list(fig1,fig2,fig3,fig4,fig5,fig6),
                     nrow=3, ncol=2)
# figs <- ggarrange(fig1,fig2,fig3,fig4,fig5,fig6,
#                   widths=c(8.2,8.2),heights=c(8,8,8), 
#                   nrow=3,ncol=2)
ggsave("figs.png", figs, width=16.4, height=24)









agg1 <- ggplot() +
  geom_polygon(data=b_bMap,aes(x=long,y=lat,group = group),
               color="#e41a1c", fill="white", size=0.7)+ # dark red
  coord_equal()+
  theme_minimal()+
  theme_special+
  # theme(legend.position = "none",
  #       plot.caption = element_text(size=6))+
  labs(title =  bquote("Region"~R[i]~ "of sub-borough regions"),
       subtitle="in red.")

# Add the police precinct regions in dark blue
agg2 <- agg1 + 
  geom_polygon(data=b_pMap,aes(x=long,y=lat,group = group),
               color="#377eb8",fill=NA,size=0.7)+ # dark blue 
  labs(title = expression(paste("Region"~R[ij]~ "with precinct regions")),
       subtitle="in red and blue, respectively.")

# Add the school district regions in orange
agg3 <- agg2 + 
  geom_polygon(data=b_sMap,aes(x=long,y=lat,group = group),
               color="#ff7f00",fill=NA,size=0.7)+ # orange
  labs(title = expression(paste("Region"~R[ijk]~ "with school district")),
       subtitle="in red, blue and orange, respectively.")

# Add the community district regions in purple
agg4 <- agg3 + 
  geom_polygon(data=filter(b_cMap, !group %in% c(57.1,57.3)),aes(x=long,y=lat,group = group),
               color="#984ea3",fill=NA,size=0.7)+ # purple
  labs(title = expression(paste("Region "~R[ijkl]~ "with community district")),
       subtitle="in red, blue, orange, and purple, respectively.")

# Plot all of the plots together.
aggs <- arrangeGrob(grobs=list(agg1, agg2, agg3, agg4),
                    nrow=2, ncol=2)
ggsave("aggs.png", aggs, width=7.2, height=8)




tempB <- b_bMap %>% filter(GEOID10 == 3604011)
tempP <- b_pMap %>% filter(Precinct %in% c(67, 70, 71, 77, 78))
tempS <- b_sMap %>% filter(SchoolDist %in% c(17, 18))
tempC <- b_cMap %>% filter(BoroCD %in% c(309, 314, 317, 355, 308))

# Plot the small section
agg_close <- ggplot() +
  geom_polygon(data=tempB,aes(x=long,y=lat,group = group),
               color="#e41a1c", fill="gray70",alpha=0.5, size=0.8)+ # dark red
  geom_polygon(data=tempP,aes(x=long,y=lat,group = group),
               color="#377eb8",fill=NA,size=0.8)+ # dark blue 
  geom_polygon(data=tempS,aes(x=long,y=lat,group = group),
               color="#ff7f00",fill=NA,size=0.8)+ # orange
  geom_polygon(data=tempC,aes(x=long,y=lat,group = group),
               color="#984ea3",fill=NA,size=0.8)+ # purple
  coord_fixed(xlim=c(994493.9,1004652), ylim=c(176232.6,183971.2))+
  theme_minimal()+
  theme_special+
  theme(legend.position = "none",
        plot.caption = element_text(size=14))+
  labs(caption =  expression(atop(paste("Region "~R[1234]~ " of sub-borough region 1 in red, precinct region 2 in blue,"), "school district region 3 in orange, and community district region 4 in purple.")))

ggsave("aggZoom.png", agg_close,
       width=8, height=6)

happyMap <- ggplot(b2) +
  geom_polygon(aes(x=long,y=lat,fill=happiness,group = group),
               color="gray40",alpha=0.7)+
  coord_quickmap()+
  theme_minimal()+
  geom_text(data= b_labels, aes(x=long, y=lat, label = name,fontface=2),
            color="black",size=12)+ 
#  labs(title = "Happiness Score Index" ) + #,
       #caption = "Figure 9: A map of New York City where darker yellow represents happier sub-borough regions.")+
  theme_special+
  scale_fill_gradient2(name="Happiness\nScore\nIndex",limits=c(0,5),
                       high="darkgoldenrod4", mid="gold1", low="gray99",
                       midpoint=2.5)

ggsave("happyMap.png", happyMap,
       width=16, height=8)

happyScatter <- ggplot(data=b2) +
  geom_point(aes(x=immigrantPct,y=happiness,color=name,size=totalNum),alpha=0.7)+
  labs(x="Immigrant Residency", y="Happiness Score Index",
       title = "Immigrant Residency and Happiness Score Index") + #, 
       #caption= "Figure 10: A scatter plot where the color represents each sub-borough and the size represents number of households.") +
  theme_minimal()+
  theme(legend.background = element_rect(colour = "gray20"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "vertical",
        plot.title = element_text(size=18, hjust = 0.5),
        legend.text = element_text(size=12),
        axis.title = element_text(size=12),
        axis.text = element_text(size=11))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  scale_x_continuous(limits=c(15,75),breaks = seq(10,90,by=10), 
                     labels=paste0(seq(10,90,by=10),"%"))+
  scale_y_continuous(limits=c(0.75,5),breaks = seq(1,5))+
  scale_color_brewer("",aesthetics = "colour",
                     palette = "Set2",type="qual")+
  scale_size_continuous(name="Total Households:",limits = c(35000,120000),
                        breaks = seq(35000,120000,by=20000),
                        labels = paste0(seq(35,120,by=20),"K"),
                        range = c(1, 10))

ggsave("happyScatter.png", happyScatter,
       width=8, height=6)


