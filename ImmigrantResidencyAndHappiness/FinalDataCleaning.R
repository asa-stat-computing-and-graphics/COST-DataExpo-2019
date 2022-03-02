##############################################
## The main data processing file
##   for the Alison Tuiyott led project
##   looking at immigration status and
##   'happiness' by sub-borough of New York.


# Read in necessary libraries & set directory
setwd("C:/Users/POA5/Documents/MiamiAlison3/ASA_Comp")
library(pacman)
p_load(ggplot2,Stack,dplyr,ggrepel,gridExtra,
       leaflet,plotly,readxl,rgdal,tigris,rgdal,rgeos,maptools,sp)

# Read in data
##  This code figured out how to process everything, then
##   all years are processed

# setwd("C:/Users/POA5/Documents/MiamiAlison3/ASA_Comp/ASA_Data/CSV")
# filename1 <- paste("NYCHVS", 1991, "Occupied File for ASA Challenge_CSV.csv")
# temp1 <- read.csv(filename1,  skip = 1, header = T)
# counter <- 1993
# allData <- {temp1}
# 
# for(i in 1:9){
#   filename <- paste("NYCHVS", counter, "Occupied File for ASA Challenge_CSV.csv")
#   temp <- read.csv(filename,  skip = 1, header = T)
#   allData <- Stack(allData,temp)#colnames(temp))
#   #colnames(temp) <- gsub(colnames(temp)
#   #allData <- rbind(allData, temp)
#   counter <- counter+3
# }
#save(allData, file="AllYears.Rdata")
load("AllYears.Rdata")

# Filter for first and second generation immigrants  
allData_id <- allData %>%
  mutate(
    immigrant = ifelse(
      ( !(Place.of.Householder.s.Birth %in% c(9,10,98)) & Year.Identifier < 2005 ) |
        ( !(Place.of.Householder.s.Birth %in% c(7,9,98)) & Year.Identifier >= 2005 ), 1,0),
    missingH = ifelse(Place.of.Householder.s.Birth == 98,1,0),
    sgen = ifelse(
      ( (Place.of.Householder.s.Birth %in% c(9,10)  & Year.Identifier < 2005) |
          (Place.of.Householder.s.Birth %in% c(7,9)  & Year.Identifier >= 2005)            ) &
        ( (Place.of.Householder.s.Mother.s.Birth %in% c(9,10)  & Year.Identifier < 2005) |
            (Place.of.Householder.s.Mother.s.Birth %in% c(7,9)  & Year.Identifier >= 2005)   ) &
        ( (Place.of.Householder.s.Father.s.Birth %in% c(9,10)  & Year.Identifier < 2005) |
            (Place.of.Householder.s.Father.s.Birth %in% c(7,9)  & Year.Identifier >= 2005)   ),1,0),
    missingMF = ifelse(
      Place.of.Householder.s.Mother.s.Birth == 98 | 
        Place.of.Householder.s.Father.s.Birth == 98,1,0),
    fgen = ifelse(
      ( (Place.of.Householder.s.Birth %in% c(9,10)  & Year.Identifier < 2005) |
          (Place.of.Householder.s.Birth %in% c(7,9)  & Year.Identifier >= 2005)   ) &
        ( ( (!(Place.of.Householder.s.Mother.s.Birth %in% c(9,10,98))  & Year.Identifier < 2005) |
              (!(Place.of.Householder.s.Mother.s.Birth %in% c(7,9,98))  & Year.Identifier >= 2005)   ) |
            ( (!(Place.of.Householder.s.Father.s.Birth %in% c(9,10,98))  & Year.Identifier < 2005) |
                (!(Place.of.Householder.s.Father.s.Birth %in% c(7,9,98))  & Year.Identifier >= 2005) )),1,0)
  )

# Building out shape files
load("sub_borough_shapefile.RData")
nycBoroughs <- data.frame(plotCode=c(0:4),
                          name=c("Manhattan","Bronx","Staten Island",
                                 "Brooklyn", "Queens"),
                          dataCode=c(3,1,5,2,4))


# Complete Data W/ Sampling Weights and Immigration numbers
completeData <- allData_id %>%
  select(Year.Identifier,Borough, Sub.Borough.Area, GEO.id2,immigrant,fgen,sgen,
         Monthly.contract.rent,#Condition.of.building,Tenure.1,Number.of.bedrooms,Presence.of.mice.or.rats,
         Total.Household.Income.Recode,
         Household.Sampling.Weight..5.implied.decimal.places.) %>%
  mutate(Year = ifelse(Year.Identifier < 100,1900+Year.Identifier,Year.Identifier)) %>%
  rename(SubBorough = Sub.Borough.Area) %>%
  group_by(Year,Borough, SubBorough, GEO.id2) %>%
  mutate(samplingWeight = Household.Sampling.Weight..5.implied.decimal.places. / 100000,
         immigrantNum = round(sum(immigrant*samplingWeight)),
         fgenNum = round(sum(fgen*samplingWeight)),
         sgenNum = round(sum(sgen*samplingWeight)),
         totalNum = round(sum(samplingWeight)),
         #buildingCond = as.factor(as.character(ifelse(Condition.of.building != 8,Condition.of.building,NA))),
         #rentOwn = as.factor(ifelse(Tenure.1 == 1,"Owned","Rent/NA")),
         #bedNum = ifelse(Number.of.bedrooms == 1, 0,Number.of.bedrooms-1),
         monthlyContractRent = ifelse(Monthly.contract.rent %in% c(99998,99999),NA,Monthly.contract.rent),
         #ratNum = as.factor(ifelse(Presence.of.mice.or.rats == 8, NA,ifelse(Presence.of.mice.or.rats == 2,0,1))),
         totalHouseholdIncome = ifelse(Total.Household.Income.Recode %in% c(999998, 9999999), NA,Total.Household.Income.Recode),
         immigrantPct = round((immigrantNum/totalNum * 100), digits = 2),
         fgenPct = round((fgenNum/totalNum * 100), digits = 2),
         sgenPct = round((sgenNum/totalNum * 100), digits = 2),
         #avgBedNum = round(mean(bedNum,na.rm = T), digits = 2),
         avgMonthlyContractRent = round(mean(monthlyContractRent,na.rm = T),digits = 2),
         avgTotalHouseholdIncome = round(mean(totalHouseholdIncome,na.rm = T),digits = 2) 
  ) %>%
  select(Year,Borough, SubBorough, GEO.id2,immigrantNum,fgenNum,
         sgenNum,totalNum,
         immigrantPct,fgenPct,sgenPct,avgMonthlyContractRent,avgTotalHouseholdIncome) %>%
  distinct() 

currentCPI <- 361
completeData <- completeData %>%
  #select(-c(name.y,plotCode.y)) %>%
  #rename(name = name.x,plotCode = plotCode.x) %>%
  mutate(avgTotalHouseholdIncome2 = case_when(
    Year == 1991 ~ avgTotalHouseholdIncome * (currentCPI / 205.1),
    Year == 1993 ~ avgTotalHouseholdIncome * (currentCPI / 215.5),
    Year == 1996 ~ avgTotalHouseholdIncome * (currentCPI / 231.3),
    Year == 1999 ~ avgTotalHouseholdIncome * (currentCPI / 244.6),
    Year == 2002 ~ avgTotalHouseholdIncome * (currentCPI / 264.2),
    Year == 2005 ~ avgTotalHouseholdIncome * (currentCPI / 286.9),
    Year == 2008 ~ avgTotalHouseholdIncome * (currentCPI / 316.3),
    Year == 2011 ~ avgTotalHouseholdIncome * (currentCPI / 330.5),
    Year == 2014 ~ avgTotalHouseholdIncome * (currentCPI / 348.3),
    Year == 2017 ~ avgTotalHouseholdIncome * (currentCPI / 361) ),
    avgTotalMonthlyRent2 = case_when(
      Year == 1991 ~ avgMonthlyContractRent * (currentCPI / 205.1),
      Year == 1993 ~ avgMonthlyContractRent * (currentCPI / 215.5),
      Year == 1996 ~ avgMonthlyContractRent * (currentCPI / 231.3),
      Year == 1999 ~ avgMonthlyContractRent * (currentCPI / 244.6),
      Year == 2002 ~ avgMonthlyContractRent * (currentCPI / 264.2),
      Year == 2005 ~ avgMonthlyContractRent * (currentCPI / 286.9),
      Year == 2008 ~ avgMonthlyContractRent * (currentCPI / 316.3),
      Year == 2011 ~ avgMonthlyContractRent * (currentCPI / 330.5),
      Year == 2014 ~ avgMonthlyContractRent * (currentCPI / 348.3),
      Year == 2017 ~ avgMonthlyContractRent * (currentCPI / 361) ) ) 

completeData <- left_join(completeData,nycBoroughs, by=c("Borough" = "dataCode"))

# Combine this complete data with the spatial file to get all longs and lats
completeMap <- fortify(sub_nyc, region = "GEOID10") %>%
  mutate(id = as.numeric(id)) %>%
  left_join(completeData, by=c("id" = "GEO.id2"))

boroughs <- as.character(unique(completeData$name))
completeData <- data.frame(completeData)
completeMap <- data.frame(completeMap)

blank_layer <- list(
  title = "",
  showgrid = F,
  showticklabels = F,
  zeroline = F)

## add labels for borough
b_labels <- completeMap %>%
  group_by(Borough) %>%
  summarize(lat = (max(lat) + min(lat)) / 2,
            long = (max(long) + min(long)) / 2)
b_labels<- left_join(b_labels,nycBoroughs,by=c("Borough"="dataCode"))
                     
                     
# Save just the complete data and try shiny app
#save(completeData,completeMap,b_labels,blank_layer,boroughs, file="CompleteData.Rdata")
load("CompleteData.Rdata")
#save.image("Apr9.Rdata")
load("Apr9.Rdata")

#########################################
## Some dataviz to discover what the 
##   data says and to make sure processed
##   correctly
## These preliminary results were used
##   at the 2019 Undergraduate Research Forum
##   at Miami University

# Create Line Plots to discover
testing <- completeData %>%
  filter(name == "Brooklyn") %>%#,SubBorough %in% c(2:6)) %>%
  mutate(SubBorough = as.factor(SubBorough))

s <- ggplot()+
  geom_line(data=testing,aes(x=Year,y=immigrantPct,group=SubBorough),color="gray",size=2,stat="identity")+
  geom_line(data=testing[testing$SubBorough == 1,],aes(x=Year,y=immigrantPct,group=SubBorough),color="red",size=2,stat="identity")+
  #geom_line(data=testing,aes(x=Year,y=avgTotalHouseholdIncome2,group=SubBorough),color="gray",size=2,stat="identity")+
  #geom_line(data=testing[testing$SubBorough == 2,],aes(x=Year,y=avgTotalHouseholdIncome2,group=SubBorough),color="red",size=2,stat="identity")+
  scale_x_continuous(breaks = seq(1993,2017,by = 3))
t <- ggplot()+
  #geom_line(data=testing,aes(x=Year,y=immigrantPct,group=SubBorough),color="gray",size=2,stat="identity")+
  #geom_line(data=testing[testing$SubBorough == 1,],aes(x=Year,y=immigrantPct,group=SubBorough),color="red",size=2,stat="identity")+
  geom_line(data=testing,aes(x=Year,y=log(avgTotalHouseholdIncome2),group=SubBorough),color="gray",size=2,stat="identity")+
  geom_line(data=testing[testing$SubBorough == 1,],aes(x=Year,y=log(avgTotalHouseholdIncome2),group=SubBorough),color="red",size=2,stat="identity")+
  scale_x_continuous(breaks = seq(1993,2017,by = 3))
grid.arrange(s,t)


healthData <- read.csv("Health_and_Hospitals_Corporation__HHC__Facilities.csv")
healthData <- healthData %>%
  filter(!is.na(Longitude))

ggplot() + 
  geom_polygon(data=completeMap[completeMap$Year == 2017,],aes(x=long, y=lat,fill=SubBorough,group=group),color = "black")  + 
  geom_polygon(data=completeMap[completeMap$Year == 2017,],aes(x=long, y=lat,fill=avgMonthlyContractRent, group=group)) +
  geom_point(data=starbucks, aes(x=longitude,y=latitude),color="black",alpha=.5)+
  theme_minimal()+
  coord_map()+
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        plot.title = element_text(size=18, hjust=.5))+
  scale_fill_gradient2(name="Average Monthly Contract Rent",low="red",mid="white",high="black", 
                       midpoint = 1750,limits=c(0,3500))

# Crime Data
crime <- read_xls("crimeData.xls",skip = 2,n_max=616)
crime$precinct <- rep(unique(crime$PCT)[-2],each=8)

crimeData <- crime %>%
  dplyr::select(-PCT) %>%
  filter(CRIME !="TOTAL SEVEN MAJOR FELONY OFFENSES") %>%
  tidyr::gather(Year,CrimeCount,"2000":"2018") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year %in% c(seq(2002,2017,by=3)))
shape <- readOGR('~/MiamiAlison3/ASA_Comp/precinct.shp')
shape <- spTransform(shape,CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
shape2 <- fortify(shape)

precinctID <- data.frame(FortifyID = unique(shape2$id),
                         DataId = precinctOrder)
shape2 <- left_join(shape2, precinctID,by=c("id"="FortifyID"))

crimePlotData <- crimeData %>%
  left_join(shape2,by=c("precinct"="DataId"))

p_labels <- shape2 %>%
  group_by(DataId) %>%
  summarize(lat = (max(lat) + min(lat)) / 2,
            long = (max(long) + min(long)) / 2)
ggplot() + 
  geom_polygon(aes(x=long, y=lat,
                   fill=CrimeCount, group=group),
               data=crimePlotData) +
  theme_minimal()+
  geom_text(data= p_labels, aes(x=long, y=lat, label = DataId),color="red")+
  coord_map()+
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        plot.title = element_text(size=18, hjust=.5))+
  scale_fill_gradient2(name="Crime Count",low="red",mid="white",high="black", 
                       midpoint = 8,limits=c(0,16))

########################
## After the Undergrad Research Forum
##   Some additional data processing. 
#######################

# Load Data
load("CompleteData.Rdata")
load("Crime.Rdata")

# Read in the mortality data
mortality <- read.csv("mortality.csv")
mortalityData <- mortality %>%
  select(Community_District,Male,Female,Total,Year)

# Read in birth data
birth <- read.csv("birth.csv")
birthData <- birth %>%
  select(cd,birthtot,Year)

#rm(mortality,birth)
birthData <- birthData %>%
  mutate(borough = substr(cd,1,1),
         community = substr(cd,2,3),
         boroughName = case_when(
           borough == "1" ~ "Manhattan",
           borough == "2" ~ "Bronx",
           borough == "3" ~ "Brooklyn",
           borough == "4" ~ "Queens",
           borough == "5" ~ "Staten Island",
           TRUE ~ "Else")  )

# Create key file for community district name and code
cdKey <- as.data.frame(cbind(cdName = as.character(mortalityData$Community_District),
               cdCode = as.numeric(as.character(birthData$cd)))) %>%
  distinct()

# Join health related tables
health <- cdKey %>%
  mutate(cdCode = as.numeric(as.character(cdCode))) %>%
  left_join(birthData,by=c("cdCode"="cd")) %>%
  left_join(mortalityData, by=c("cdName" = "Community_District",
                                "Year" = "Year"))
#save(health, file="Health.Rdata")

#### Calculate the health score ---TO DO

#### Crime
# Read in the crime data again for only totals
crime2 <- read_xls("crimeData.xls",skip = 2,n_max=616)
crime2$precinct <- rep(unique(crime2$PCT)[-2],each=8)
crimeData2 <- crime2 %>%
  filter(CRIME =="TOTAL SEVEN MAJOR FELONY OFFENSES") %>%
  dplyr::select(-PCT) %>%
  tidyr::gather(Year,CrimeCount,"2000":"2018") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year %in% c(seq(2002,2017,by=3))) %>%
  rename(crime = CRIME)
#save(crimeData2,file="Crime2.Rdata")

#### Calculate the crime score ---TO DO

#### School District
edu <- read_xlsx(path = "gradRates.xlsx",sheet = 6) %>%
  mutate(ClassOf = `Cohort Year`+4) %>%
  filter(ClassOf %in% c(2011,2014)) %>%
  rename(TotalGraduates =`# number of graduates`,
         EnteringYear = `Cohort Year`,
         CohortPct = `% of cohort`,
         GradPct = `% of graduates`) %>%
  dplyr::select(DBN,School,ClassOf,EnteringYear,CohortPct,TotalGraduates,GradPct) %>%
  mutate(sd = substr(DBN,1,2),
         randomLetter = substr(DBN,3,3),
         sID = substr(DBN,4,7),
         borough = case_when(
           randomLetter == "M" ~ "Manhattan",
           randomLetter == "X" ~ "Bronx",
           randomLetter == "K" ~ "Brooklyn",
           randomLetter == "Q" ~ "Queens",
           randomLetter == "R" ~ "Staten Island",
           TRUE ~ "Else") ) %>%
  dplyr::select(borough,sd,sID,ClassOf,EnteringYear,CohortPct,TotalGraduates,GradPct)

edu <- edu %>%
  group_by(ClassOf, sd,borough,EnteringYear) %>%
  mutate(CohortPct = as.numeric(ifelse(CohortPct == "s",NA,CohortPct)),
         TotalGraduates = as.numeric(ifelse(TotalGraduates == "s",NA,TotalGraduates)),
         GradPct = as.numeric(ifelse(GradPct == "s",NA,GradPct))) %>%
  summarise(nSchools = n(),
          AvgCohortPct = mean(CohortPct,na.rm = T),
         AvgTotalGrad = mean(TotalGraduates,na.rm = T),
         AvgGradPct = mean(GradPct,na.rm = T))

#save(edu,file="Education.Rdata")

#### Calculate the education score ---TO DO

#### May 12
load("CompleteData.Rdata")
load("Health.Rdata")
load("Crime2.Rdata")
load("Education.Rdata")
#load("nycShapefiles.Rdata") # cd,sd,pp shapefiles
#load("cps_regions.RData")
load("cpsb_regions.RData")


edu <- edu %>%
  mutate(schooldist = as.numeric(as.character(sd)))



allYears <- cps_regions@data
allYears <- allYears %>%
  left_join(crimeData2,by=c("Precinct"="precinct")) %>%
  left_join(edu,by=c("SchoolDist"="schooldist",
                     "Year" = "ClassOf")) %>%
  left_join(health,by=c("BoroCD"="cdCode",
                        "Year"="Year"))  %>%
  mutate(GEOID10 = as.numeric(GEOID10)) %>%
  left_join(completeData,by=c("GEOID10"="GEO.id2",
                              "Year"))

plotData <- allYears %>%
  filter(Year %in% c(2011,2014)) %>%
  dplyr::select(-c(borough.x,borough.y))

# try plotting the data again
plotData2 <- plotData %>%
  filter(Year == 2011)

#set row names so we can make an spdf
row.names(plotData2) = paste(plotData2$c,plotData2$p,plotData2$s,
                             plotData2$b,plotData2$id)

#left join all the  data files onto cps_regions@data 
cps2 <- SpatialPolygonsDataFrame(cps_regions,plotData2)

mean(rownames(cps2@data) == sapply(cps2@polygons, function(x) slot(x,"ID")))
mean(rownames(cps2@data) == sapply(cps_regions@polygons, function(x) slot(x,"ID")))

# Plot data for sanity check
leaflet(cps2) %>%
  addTiles() %>% 
  addPolygons(label = paste0("BoroCD: ", cps2@data$BoroCD,
                             ", Precinct: ", cps2@data$Precinct,
                             ", School District: ", cps2@data$SchoolDist,
                             ", Number of Crimes: ", cps2@data$CrimeCount,
                             ", Avg. % of Graduates: ", round(cps2@data$AvgGradPct,digits = 2),
                             ", Number of Births: ", cps2@data$birthtot,
                             ", Number of Deaths: ", cps2@data$Total,
                             ", % of Immigrants: ", cps2@data$immigrantPct))

#save.image(file="May13.Rdata")
