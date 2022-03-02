# Main Author: Jhonatan Medri
# Article name: Housing Variables and Immigration:
#   An Exploratory Analysis in New York City
# This code was reviewed for the last time on 02/15/2022
# This code refines and aggregates the datasets used in the next steps.
# It considers the data of the last 10 NYCHVS from 1991 - 2017.

#--------- RUN THESE STEPS FIRST ---------#

# Load R Packages
library(dplyr)
library(geojsonio)
library(Hmisc)
library(plyr)
library(readr)
library(rmapshaper)
library(rgdal)
library(sp)
library(styler)

# Set Working Directory, Seed, and Shapefile Location
# setwd("C:/Users/jhona/OneDrive/Desktop/Thesis/R Scripts")
set.seed(6901)
nypuma.17adsn <- "./nypuma_17"

#--------- GET AND REFINE THE SHAPEFILE  ---------#

# Step 1: Read shapefiles (select whichever is appropriate from the following)
# dsn is the folder containing the shapefies (previously defined)
# layer is the filename for the files contained
# Using RGDAL package
shape <-
  readOGR(
    dsn = nypuma.17adsn,
    layer = "nypuma",
    verbose = TRUE
  )

# Step 2: Use rmapshaper to convert shapefiles to geo_json file type,
# and then to a spatial class
# Using Geojsonio package
NYPuma.json <-
  geojson_json(shape, geometry = "polygon", group = "group")
NYPuma.sp <- geojson_sp(NYPuma.json)

# Step 3:  Use rmapshaper to simplify boundaries,
# adjust keep option for more or less abstraction
# The keep option is a proportion of the total points that the file will keep.
# We are keeping 0.7% of the original points in the file, thus simplifying it.
# Using Rmapshaper package
NYPuma.simp <- ms_simplify(NYPuma.sp, keep = .007)

# Step 4: Manually include Boroughs and SubBoroughs Labels

NYPuma.simp$Bor <-
  ifelse(substr(NYPuma.simp$puma, 1, 2) == "37",
    "BX",
    ifelse(
      substr(NYPuma.simp$puma, 1, 2) == "40",
      "BK",
      ifelse(
        substr(NYPuma.simp$puma, 1, 2) == "38",
        "MN",
        ifelse(substr(NYPuma.simp$puma, 1, 2) == "41", "QN", "SI")
      )
    )
  )

NYPuma.simp$sba <- paste(NYPuma.simp$Bor,
  substr(NYPuma.simp$puma,
    start = 3,
    stop = 4
  ),
  sep = ""
)

# Step 5: Export NYPuma.simp
writeOGR(
  obj = NYPuma.simp,
  dsn = "NYPuma",
  layer = "nypuma",
  driver = "ESRI Shapefile",
  overwrite_layer = TRUE
)

#-------- Compile Data and Aggregate it (by Borough and Subborough)  --------#

# Aggregate NYCHVS csvs previously downloaded
NYCHVS <-
  ldply(
    list.files(
      path = "./Data_CSV",
      pattern = "*.csv",
      full.names = TRUE
    ),
    read_csv
  ) %>%
  rename_all(function(x) {
    gsub("_", "X", x)
  }) %>%
  filter(recordtype != "Record Type")

#--------- CREATE READABLE SBS REGIONS  ---------#

NYCHVS$puma <- as.character(substr(NYCHVS$geoXid2, start = 4, stop = 7))

NYCHVS$sba <-
  ifelse(
    substr(NYCHVS$geoXid2, 4, 5) == "37",
    paste("BX", substr(
      NYCHVS$geoXid2,
      start = 6, stop = 7
    ), sep = ""),
    ifelse(
      substr(NYCHVS$geoXid2, 4, 5) == "40",
      paste("BK", substr(
        NYCHVS$geoXid2,
        start = 6, stop = 7
      ), sep = ""),
      ifelse(
        substr(NYCHVS$geoXid2, 4, 5) == "38",
        paste("MN", substr(
          NYCHVS$geoXid2,
          start = 6, stop = 7
        ), sep = ""),
        ifelse(
          substr(NYCHVS$geoXid2, 4, 5) == "41",
          paste("QN", substr(
            NYCHVS$geoXid2,
            start = 6, stop = 7
          ), sep = ""),
          paste("SI", substr(
            NYCHVS$geoXid2,
            start = 6, stop = 7
          ), sep = "")
        )
      )
    )
  )

#--------- BIG STEP: DATASET   ---------#

NYCHVS.simp <- NYCHVS %>%
  # Borough, sub-borough, and year

  # (1=Bronx, 2=Brooklyn, 3=Manhattan, 4=Queens, 5=Staten Island)
  transmute(
    borough = mapvalues(
      borough, c("1", "2", "3", "4", "5"),
      c("BX", "BK", "MN", "QN", "SI")
    ),

    # Coded PUMA regions (i.e. BX01)
    sba = as.character(sba),
    puma = as.character(puma),

    # 1991, 1993, 1996, 1999, 2002, ...., 2017
    year = as.numeric(mapvalues(
      year,
      c(91, 93, 96, 99),
      c(1991, 1993, 1996, 1999)
    )),

    # Individual sociodemographic information

    # Individual's sex (1=Male, 0=Female)
    sex = as.numeric(mapvalues(X1b, c(2, 8), c(0, NA))),

    # Individual's age (Years tend to include years or more in last value)
    age = as.numeric(mapvalues(X1c, 98, NA)),

    # Whether individual has Hispanic origin (1=No, 2-7=Country of Origin)
    hispanic = as.numeric(mapvalues(X1e, 8, NA)),

    # Individual's country of origin (98= Not Reported)
    birthplace = as.numeric(mapvalues(X7a, 98, NA)),

    # We are defining immigrant as someone who was born out of the US and is not
    # a US citizen, rather than just someone who reports himself as a citizen
    # We left Puerto Ricans as part of the US citizen group

    imm = as.numeric(mapvalues(
      birthplace,
      c(
        7, 9, 10, 11, 12, 13, 14, 15, 16, 17,
        18, 19, 20, 21, 22, 23, 24, 25, 26
      ),
      c(
        0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1
      )
    )),

    # Individual father's country of origin (98= Not Reported)
    birthplacef = as.numeric(mapvalues(X7b, 98, NA)),
    imm.f = as.numeric(mapvalues(
      birthplacef,
      c(
        7, 9, 10, 11, 12, 13, 14, 15, 16, 17,
        18, 19, 20, 21, 22, 23, 24, 25, 26
      ),
      c(
        0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1
      )
    )),

    # Individual mother's country of origin (98= Not Reported)
    birthplacem = as.numeric(mapvalues(X7c, 98, NA)),
    imm.m = as.numeric(mapvalues(
      birthplacem,
      c(
        7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
        21, 22, 23, 24, 25, 26
      ),
      c(
        0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1
      )
    )),
    imm.p = ifelse(imm.f == 0 | imm.m == 0, 0,
      ifelse(imm.f == 1 & imm.m == 1, 1, NA)
    ),

    # Wheter individual moved as immigrant
    # (1=Yes, 2=No, 8=Not Reported, 9=Born in US)
    us.imm = as.numeric(mapvalues(X57a, c(2, 8, 9), c(0, NA, 0))),

    # Year individual moved to US (9998=Not Reported, 9999=Born in US)
    year.immus = as.numeric(mapvalues(X57b, c(9998, 9999), c(NA, NA))),

    # Year individual moved to NYC (9998=Not Reported, 9999=Born in NYC)
    year.mnyc = as.numeric(mapvalues(X58, c(9998, 9999), c(NA, NA))),

    # Individual economic and household information

    # Monthly Contract Rent
    mgrent = as.numeric(mapvalues(
      mgrent, c(9998, 9999, 99999),
      c(NA, NA, NA)
    )),
    mlogrent = log10(mgrent),

    # Whether individual owns the household (1=Owned, 9=Renter Occupied)
    own.status = as.numeric(mapvalues(X9a, 9, 0)),

    # Whether individual rents or live for free
    # (2=Pay Rent, 3=Rent Free, 9=Owned)
    rent.status = as.numeric(mapvalues(X9c, c(2, 3, 9), c(1, 0, 0))),

    # Whether individual has a mortgage (1=Yes, 2=No, 3,8,9=Not Reported)
    mort.status = as.numeric(mapvalues(X14, c(2, 8, 9), c(0, NA, NA))),

    # House Value ($1-$3,950,000. Over is not reported. Only applies to owned.)
    house.value = as.numeric(mapvalues(
      X13, c(9999999, 9999998),
      c(NA, NA)
    )),

    # Mortgage interest rate (1-699. Over is not reported)
    mort.int = as.numeric(mapvalues(X15c, c(9998, 9999), c(NA, NA))),

    # Individual's income
    # (- Income loss, $1-$9,999,9997. $9,999,999 not reported)
    yhhinc = as.numeric(mapvalues(
      hhinc, c(9999999, 999998),
      c(NA, NA)
    )),
    mhhinc = ifelse(yhhinc > 10, yhhinc / 12, NA),
    mloghhinc = ifelse(mhhinc > 10, log10(mhhinc), NA),

    # Shelter Burden (Housing Cost)
    sheltburd = ifelse(mgrent > 0 &
      mhhinc > 0, mgrent / mhhinc, NA)
  )


# Aggregate the data by the 55 sub-boroughs

NYCHVS.simp55 <- NYCHVS.simp %>%
  dplyr::group_by(year, puma, sba, borough) %>%
  dplyr::summarise(
    n = n(),
    # Male and Female Prop
    MaleProp = {
      sum(sex == 1, na.rm = TRUE) / (sum(sex == 1, na.rm = TRUE) +
        sum(sex == 0, na.rm = TRUE))
    },
    FemaleProp = {
      sum(sex == 0, na.rm = TRUE) / (sum(sex == 1, na.rm = TRUE) +
        sum(sex == 0, na.rm = TRUE))
    },

    # Age
    MedianAge = median(age, na.rm = TRUE),

    # Immigration Proportion
    ImmProp = {
      sum(imm == 1, na.rm = TRUE) / (sum(imm == 1, na.rm = TRUE) +
        sum(imm == 0, na.rm = TRUE))
    },
    NImmProp = 1 - ImmProp,
    ImmProp2 = {
      sum(us.imm == 1, na.rm = TRUE) / (sum(us.imm == 1, na.rm = TRUE) +
        sum(us.imm == 0, na.rm = TRUE))
    },
    MedianIncome = median(yhhinc, na.rm = TRUE),
    MedianRent = median(mgrent, na.rm = TRUE),
    MedianHValue = median(house.value, na.rm = TRUE),
    RentingProp = sum(rent.status == 1, na.rm = TRUE) / n,
    Renters = sum(rent.status == 1, na.rm = TRUE),
    OwningProp = sum(own.status == 1, na.rm = TRUE) / n,
    Owners = sum(own.status == 1, na.rm = TRUE),
    Morters = sum(mort.status == 1, na.rm = TRUE),
    MortProp = Morters / Owners,
    MedianRate = median(mort.int, na.rm = TRUE) / 1000,
    SheltBurd = median(sheltburd, na.rm = TRUE)
  )

# Aggregate the data by the 5 boroughs

NYCHVS.simp5 <- NYCHVS.simp %>%
  dplyr::group_by(year, borough) %>%
  dplyr::summarise(
    n = n(),
    # Male and Female Prop
    MaleProp = {
      sum(sex == 1, na.rm = TRUE) / (sum(sex == 1, na.rm = TRUE) +
        sum(sex == 0, na.rm = TRUE))
    },

    # Immigrants
    Immigrants = sum(imm == 1, na.rm = TRUE),
    NonImmigrants = sum(imm == 0, na.rm = TRUE),
    ImmProp = Immigrants / (Immigrants + NonImmigrants),
    MedianIncome = median(yhhinc, na.rm = TRUE),
    MedianRent = median(mgrent, na.rm = TRUE),
    MedianHValue = median(house.value, na.rm = TRUE),
    RentingProp = sum(rent.status == 1, na.rm = TRUE) /
      n,
    Renters = sum(rent.status == 1, na.rm = TRUE),
    OwningProp = sum(own.status == 1, na.rm = TRUE) / n,
    Owners = sum(own.status == 1, na.rm = TRUE),
    Morters = sum(mort.status == 1, na.rm = TRUE),
    MortProp = Morters / Owners,
    MedianRate = median(mort.int, na.rm = TRUE) / 1000,
    SheltBurd = median(sheltburd, na.rm = TRUE)
  )

# Export the 3 Datasets
# (Raw Data, Aggregated by Sub-borough, and Aggregated by Borough)

write.csv(NYCHVS.simp,
  "NYCHVSall.csv",
  row.names = FALSE
)

write.csv(NYCHVS.simp55,
  "NYCHVS55.csv",
  row.names = FALSE
)

write.csv(NYCHVS.simp5,
  "NYCHVS05.csv",
  row.names = FALSE
)
