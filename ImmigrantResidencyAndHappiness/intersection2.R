###################################
## the code that determines the
##    intersection of the different regions
## From the intersection of these regions
##    we use a weighted aggregation to
##    compute a 'happiness' score for each
##    sub-borough in New York


library(rgdal)
library(rgeos)
library(tidyverse)
library(leaflet)
setwd("C:/Users/POA5/Documents/MiamiAlison3/ASA_Comp")

#read in individual files
community = spTransform(readOGR("ASA_Data/nycd_19a/nycd.shp"),
                        CRSobj = CRS("+init=epsg:2263"))
police =    spTransform(readOGR("ASA_Data/nypp_19a/nypp.shp"),
                        CRSobj = CRS("+init=epsg:2263"))
school =    spTransform(readOGR("ASA_Data/nysd_19a/nysd.shp"),
                        CRSobj = CRS("+init=epsg:2263"))
load("sub_borough_shapefile.RData")
borough = spTransform(sub_nyc,
                      CRSobj = CRS("+init=epsg:2263"))



#### Time to intersect ####
#0 width buffer to fix up self intersections
community = gBuffer(community, byid = TRUE, width = 0)
police = gBuffer(police, byid = TRUE, width = 0)
school = gBuffer(school, byid = TRUE, width = 0)
borough = gBuffer(borough, byid = TRUE, width = 0)


#intersect all the files, drop all points/lines and keep only polygons
com_pol = gIntersection(community, police, byid = TRUE, drop_lower_td = TRUE)
com_pol_school = gIntersection(com_pol, school, byid = T, drop_lower_td = TRUE)
com_pol_school= gBuffer(com_pol_school, byid = T,width = 0)
com_pol_school_bor = gIntersection(com_pol_school,borough, byid = T, drop_lower_td = TRUE)
com_pol_school_bor= gBuffer(com_pol_school_bor, byid = T,width = 0)


#project back to long/lat
cps = spTransform(com_pol_school_bor,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

#check the work
ggplot(cps) +
  geom_path(aes(long,lat,group = group)) +
  coord_quickmap()

#need to do this for later merging
community@data$c = sapply(community@polygons, function(x) slot(x,"ID"))
police@data$p = sapply(police@polygons, function(x) slot(x,"ID"))
school@data$s = sapply(school@polygons, function(x) slot(x,"ID"))
borough@data$b = sapply(borough@polygons, function(x) slot(x,"ID"))


#parse the automatically created ids into useful columns for each shapefile
regions = sapply(cps@polygons, function(x) slot(x,"ID")) %>%
      str_split(" ",simplify = T) %>% as.data.frame() %>% mutate_if(is.factor, as.character)
names(regions) = c("c","p","s","b","id")
#regions <- regions[,-5]

#left join on the real IDs
regions = left_join(regions,community@data %>% dplyr::select(BoroCD,c))
regions = left_join(regions,police@data %>% dplyr::select(Precinct,p))
regions = left_join(regions,school@data %>% dplyr::select(SchoolDist,s))
regions = left_join(regions,borough@data %>% dplyr::select(GEOID10,b) %>% mutate(b=str_split(b," ",simplify = T)[,1],
                                                                                 id=as.character(rep(1,55))))
#regions <- regions[,-5]

#set row names so we can make an spdf
row.names(regions) = paste(regions$c,regions$p,regions$s,regions$b,regions$id)

#this is the final object you need.
#left join all the  data files onto cps_regions@data 
cps_regions = SpatialPolygonsDataFrame(cps,regions)

#check that these both are 1 (data matches order of polygons)
mean(rownames(cps_regions@data) == sapply(cps_regions@polygons, function(x) slot(x,"ID")))
mean(rownames(cps_regions@data) == sapply(cps@polygons, function(x) slot(x,"ID")))

#remove the old stuff if you want
#rm(com_pol, com_pol_school, community, police, school, regions, cps)

#check work 
leaflet(cps_regions) %>%
  addTiles() %>% 
  addPolygons(label = paste0("BoroCD: ", cps_regions@data$BoroCD,
                             ", Precinct: ", cps_regions@data$Precinct,
                             ", School District ", cps_regions@data$SchoolDist))

#save(cps_regions, file = "cps_regions.RData")
#save(cps_regions, file = "cpsb_regions.RData")
