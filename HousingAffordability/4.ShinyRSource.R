# Main Author: Jhonatan Medri
# Article name: Housing Variables and Immigration:
#   An Exploratory Analysis in New York City
# This code was reviewed for the last time on 02/15/2022
# This code builds the functions used in the Shiny R interface

# Load R Packages
library(classInt)
library(dplyr)
library(formattable)
library(ggmap)
library(ggplot2)
library(grid)
library(gridExtra)
library(labeling)
library(micromap)
library(RColorBrewer)
library(rgdal)
library(scales)
library(sp)
library(spdep)
library(styler)
library(tidyr)
library(plyr)

# Set Work Directory
# setwd("C:/Users/jhona/OneDrive/Documents/Classes/USU/Thesis/R Scripts")

# Call Shapefile and Data
NY_shape <- readOGR(
  dsn = "./NYPUMA",
  layer = "nypuma",
  verbose = TRUE
)
NY_data <- read.csv("NYCHVS55.csv")
NY_data_t <- read.csv("NYCHVSall.csv")
NY_data$puma <- as.character(NY_data$puma)

#--------- Function 1: Choropleth maps  ---------------#

# The parameters of the function are:
#
# data: Selected Dataset. This was produced in the 1.ObjectStep.R code.
#
# shapefile: Selected Shapefile. This was produced the 1.ObjectStep.R code.
#
# year: 1991, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2014, or 2017.
#
# variable: Selected Variable. This includes "Own" (home ownership percentage),
# "Rent" (household median rent), "Cost" (housing cost percentage),
# "Value" (housing median value in gross US$),
# "Imm" (immigrant households), "sex" (female householder proportion),
# and "Age" (householder median age).
#
# color: Any R palette. We suggest sequential palettes.
# such as "Greens", "Greys", "Oranges", "Red", etc.
#
# breaks: number of class intervals in the choropleth maps.
#
# dif_break: "Y" for different breaks between 1991 - 2017 and
# "N" for the same breaks for the entire time.
#

choropleths <- function(data = NY_data,
                        shapefile = NY_shape,
                        year = 2017,
                        variable = "Own",
                        color = "Greens",
                        breaks = 5,
                        dif_break = "N",
                        boroughs = "All") {
  data$OwningProp <- data$OwningProp * 100
  data$SheltBurd <- data$SheltBurd * 100
  data$ImmProp <- data$ImmProp * 100
  data$FemaleProp <- data$FemaleProp * 100

  data <- if (dif_break == "Y") {
    data[data$year == year, ]
  } else {
    data
  }

  data$int <-
    if (variable == "Own") {
      classIntervals(data$OwningProp,
        n = breaks,
        style = "quantile",
        cutlabels = TRUE
      )$brks %>%
        cut(data$OwningProp, ., include.lowest = TRUE)
    } else if (variable == "Rent") {
      classIntervals(data$MedianRent, n = breaks, style = "quantile")$brks %>%
        round(.) %>%
        cut(data$MedianRent,
          .,
          include.lowest = TRUE,
          dig.lab = 4
        )
    } else if (variable == "Cost") {
      classIntervals(data$SheltBurd, n = breaks, style = "quantile")$brks %>%
        cut(data$SheltBurd, ., include.lowest = TRUE)
    } else if (variable == "Cost2") {
      cut(data$SheltBurd, c(0, 30, 50), include.lowest = TRUE)
    } else if (variable == "Value") {
      classIntervals(data$MedianHValue, n = breaks, style = "quantile")$brks %>%
        cut(data$MedianHValue,
          .,
          include.lowest = TRUE,
          dig.lab = 8
        )
    } else if (variable == "Income") {
      classIntervals(data$MedianIncome, n = breaks, style = "quantile")$brks %>%
        cut(data$MedianIncome,
          .,
          include.lowest = TRUE,
          dig.lab = 6
        )
    } else if (variable == "Imm") {
      classIntervals(data$ImmProp, n = breaks, style = "quantile")$brks %>%
        cut(data$ImmProp, ., include.lowest = TRUE)
    } else if (variable == "Sex") {
      classIntervals(data$FemaleProp, n = breaks, style = "quantile")$brks %>%
        cut(data$FemaleProp, ., include.lowest = TRUE)
    } else if (variable == "Age") {
      classIntervals(data$MedianAge, n = breaks, style = "quantile")$brks %>%
        cut(data$MedianAge, ., include.lowest = TRUE)
    }

  data$col <- brewer.pal(breaks, color)[data$int]

  data <- if (dif_break == "N") {
    data[data$year == year, ]
  } else {
    data
  }


  shapefile@data$id <- rownames(shapefile@data)
  Shapedf <- join(fortify(shapefile), shapefile@data, by = "id") %>%
    merge(., data, by = "puma")


  title <- if (variable == "Own") {
    "Home \nOwnership \nPercentage (%)"
  } else if (variable == "Rent") {
    "Median \nMonthly \nGross Rent ($)"
  } else if (variable == "Cost" |
    variable == "Cost2") {
    "Housing \nCost \nPercentage (%)"
  } else if (variable == "Value") {
    "Median Housing \nValue (US$)"
  } else if (variable == "Value") {
    "Median Housing \nValue (US$)"
  } else if (variable == "Income") {
    "Median Monthly \nGross Income (US$)"
  } else if (variable == "Imm") {
    "Immigrant Household \nPercentage (%)"
  } else if (variable == "Age") {
    "Householder Median \nAge (Years)"
  } else if (variable == "Sex") {
    "Householder Female \nProportion (%)"
  }


  choropleth_map <- qmplot(
    long,
    lat,
    group = group,
    data = Shapedf,
    source = "osm",
    maptype = "roadmap",
    mapcolor = "bw",
    zoom = 11,
    geom = "tile",
    darken = 0.1
  ) +

    geom_polygon(
      data = Shapedf,
      aes(
        x = long,
        y = lat,
        group = group,
        fill = int
      ),
      colour = "black",
      alpha = 0.7,
      linetype = 1,
      size = 0.75
    ) +

    scale_fill_brewer(
      palette = color,
      name = title,
      drop = FALSE
    ) +

    theme(
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20),
      plot.title = element_text(size = 20),
      plot.margin = margin(b = 5)
    )

  return(choropleth_map)
}

#--------- Function 2: Spatial Dependence Tests  ---------------#

# The parameters of the function are:
#
# DB: Selected Dataset. This was produced in the previous R code.
#
# Shp: Selected Shapefile. This was produced in the previous R code.
#
# Var: Selected Variable. This includes "Own" (home ownership percentage),
# "Rent" (household median rent), "Cost" (housing cost percentage),
# "Value" (housing median value in gross US$),
# "Imm" (immigrant households), "sex" (female householder proportion),
# and "Age" (householder median age)
#
# Alt. Alternative hypothesis. a character string specifying the alternative
# hypothesis, must be one of "greater" (default), "less" or "two.sided".
#
# Year: 1991, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2014, or 2017.
#
# Test: "Moran" (Moran's I) or "Geary" (Geary's C)
#
# NbW: Proximity measure. "Queens" ("queen" method),
# "KN" (k-nearest-neighbor)", and "Dist" (maximum proximity distance)
#
# kn: If k-nearest-neighbor method is selected, user can input # of neighbors.
# This number must be a positive integer greater than 0.
#
# dist: If max proximity distance method is selected, user can input number of
# km, but must be greater than 7.88km (smallest distance between sub-boroughs).
#
# mode: Several outputs of selected variable.
# "name" (name of variable), "ave" (average), "sd" (standard deviation),
# "tstat" (Moran's I or Geary's C statistic), "pvalue" (test pvalue),
# "conclusion" (reject or fail to reject), "interpretation" (brief summary)
# "Test" (leave the list as it is)

statdeptests <-
  function(DB = NY_data,
           Shp = NY_shape,
           Var = "Own",
           Alt = "greater",
           Year = 2017,
           Test = "Moran",
           NbW = "Queens",
           kn = 0,
           dist = 0,
           mode = "Test") {
    Test_data <- merge(Shp, DB[DB$year == Year, ],
      by = "puma"
    )

    coords <- coordinates(Test_data)
    IDs <- row.names(as(Test_data, "data.frame"))
    dsts <-
      unlist(nbdists(
        knn2nb(knearneigh(coords, k = 1), row.names = IDs),
        coords
      ))

    Weights <-
      if (NbW == "Queens") {
        nb2listw(poly2nb(Test_data, queen = TRUE), zero.policy = TRUE)
      } else if (NbW == "KN") {
        nb2listw(knn2nb(knearneigh(
          coords,
          k = as.numeric(kn), longlat = TRUE
        ), row.names = IDs),
        zero.policy = TRUE
        )
      } else if (NbW == "Dist") {
        nb2listw(
          dnearneigh(
            coords,
            d1 = 0,
            d2 = as.numeric(dist),
            row.names = IDs,
            longlat = TRUE
          ),
          zero.policy = TRUE
        )
      } else {
        "Check Weights Spelling"
      }

    Variable <-
      if (Var == "Own") {
        Test_data$OwningProp
      } else if (Var == "Rent") {
        Test_data$MedianRent
      } else if (Var == "Cost") {
        Test_data$SheltBurd
      } else if (Var == "Value") {
        Test_data$MedianHValue
      } else if (Var == "Income") {
        Test_data$MedianIncome
      } else if (Var == "Imm") {
        Test_data$ImmProp
      } else if (Var == "Sex") {
        Test_data$FemaleProp
      } else if (Var == "Age") {
        Test_data$MedianAge
      } else {
        "Check Variable Spelling"
      }

    Name <-
      if (Var == "Own") {
        "Owned Households (proportion)"
      } else if (Var == "Rent") {
        "Median Monthly Rent (US$)"
      } else if (Var == "Cost") {
        "Housing Cost (proportion)"
      } else if (Var == "Value") {
        "Median Housing Value (US$)"
      } else if (Var == "Income") {
        "Median Monthly Income (US$)"
      } else if (Var == "Imm") {
        "Immigrant Households (proportion)"
      } else if (Var == "Sex") {
        "Female Householders (proportion)"
      } else if (Var == "Age") "Median Householder Age (Years Old)"


    Test_results <-
      suppressWarnings(
        if (Variable == "Check Variable Spelling") {
          c("Check Variable Spelling")
        } else if (Test == "Moran") {
          moran.test(
            Variable,
            randomisation = FALSE,
            Weights,
            alternative = Alt
          )
        } else if (Test == "Geary") {
          geary.test(
            Variable,
            randomisation = FALSE,
            Weights,
            alternative = Alt
          )
        } else {
          "Check Test Spelling"
        }
      )

    Interpretation <- ("We reject the null hypothesis")

    foutput <-
      if (mode == "name") {
        (Name)
      } else if (mode == "ave") {
        (round(mean(Variable), 3))
      } else if (mode == "sd") {
        (round(sd(Variable), 4))
      } else if (mode == "tstat") {
        round(Test_results$estimate[[1]], 3)
      } else if (mode == "pvalue") {
        ifelse(Test_results$p.value >= 0.0001, round(Test_results$p.value, 4),
          "Less than 0.0001"
        )
      } else if (mode == "null") {
        if (Alt == "greater") {
          ("Data is not spatially autocorrelated")
        } else if (Alt == "less") {
          ("Data is not spatially autocorrelated")
        } else if (Alt == "two.sided") ("Data is not spatially autocorrelated")
      } else if (mode == "alt") {
        if (Alt == "greater") {
          ("Data is spatially clustered")
        } else if (Alt == "less") {
          ("Data is spatially dispersed")
        } else if (Alt == "two.sided") ("Data is spatially autocorrelated")
      } else if (mode == "conclusion") {
        (if (Test_results$p.value >= 0.05) {
          "Do not reject"
        } else {
          "Reject"
        })
      } else if (mode == "interpretation") {
        paste(
          "At the 5% significance level, we ",
          {
            if (Test_results$p.value >= 0.05) {
              "do not reject "
            } else {
              "reject "
            }
          },
          "the null hypothesis that the data is ",
          {
            if (Alt == "greater") {
              ("not spatially autocorrelated")
            } else if (Alt == "less") {
              ("not spatially autocorrelated")
            } else if (Alt == "two.sided") ("not spatially autocorrelated")
          },
          "( p-value = ",
          ifelse(Test_results$p.value >= 0.0001,
            round(Test_results$p.value, 4), "less than 0.0001"
          ),
          "),"
        )
      } else if (mode == "Test") {
        Test_results
      }

    return(foutput)
  }

# FUNCTION 3: Micromaps

# The parameters of this functions are:
#
# DB: Selected Dataset. This was produced in the previous R code.
#
# Shp: Selected Shapefile. This was produced in the previous R code.
#
# n: Number of panels. Options are 2 and 3.
#
# Year: 1991, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2014, or 2017.
#
# Var1: Selected Variable for first panel.
# This includes "Own" (home ownership percentage),
# "Rent" (household median rent), "Cost" (housing cost percentage),
# "Value" (housing median value in gross US$),
# "Imm" (immigrant households), "sex" (female householder proportion),
# and "Age" (householder median age)
#
# Var2: Selected Variable for second panel.
# This includes "Own" (home ownership percentage),
# "Rent" (household median rent), "Cost" (housing cost percentage),
# "Value" (housing median value in gross US$),
# "Imm" (immigrant households), "sex" (female householder proportion),
# and "Age" (householder median age)
#
# Var3: Selected Variable for third panel if n = 3 (in Shiny, n=2 is included).
# This includes "Own" (home ownership percentage),
# "Rent" (household median rent), "Cost" (housing cost percentage),
# "Value" (housing median value in gross US$),
# "Imm" (immigrant households), "sex" (female householder proportion),
# and "Age" (householder median age)

micromaps <- function(DB = NY_data, Shp = NY_shape, n = 3, Year = 2017,
                      Var1 = "Own", Var2 = "Rent", Var3 = "Cost") {
  Polys <- create_map_table(Shp, "puma")

  DB$OwningProp <- DB$OwningProp * 100
  DB$SheltBurd <- DB$SheltBurd * 100
  DB$ImmProp <- DB$ImmProp * 100
  DB$FemaleProp <- DB$FemaleProp * 100

  Test_data <-
    DB[DB$year == Year, ]

  Test_data$MedianHValue <- Test_data$MedianHValue / 1000
  Test_data$MedianIncome <- Test_data$MedianIncome / 12

  Test_data$v1 <-
    if (Var1 == "Own") {
      Test_data$OwningProp
    } else if (Var1 == "Rent") {
      Test_data$MedianRent
    } else if (Var1 == "Cost") {
      Test_data$SheltBurd
    } else if (Var1 == "Value") {
      Test_data$MedianHValue
    } else if (Var1 == "Income") {
      Test_data$MedianIncome
    } else if (Var1 == "Imm") {
      Test_data$ImmProp
    } else if (Var1 == "Sex") {
      Test_data$FemaleProp
    } else if (Var1 == "Age") Test_data$MedianAge

  Test_data$v2 <-
    if (Var2 == "Own") {
      Test_data$OwningProp
    } else if (Var2 == "Rent") {
      Test_data$MedianRent
    } else if (Var2 == "Cost") {
      Test_data$SheltBurd
    } else if (Var2 == "Value") {
      Test_data$MedianHValue
    } else if (Var2 == "Income") {
      Test_data$MedianIncome
    } else if (Var2 == "Imm") {
      Test_data$ImmProp
    } else if (Var2 == "Sex") {
      Test_data$FemaleProp
    } else if (Var2 == "Age") Test_data$MedianAge

  Test_data$v3 <-
    if (Var3 == "Own" & n == 3) {
      Test_data$OwningProp
    } else if (Var3 == "Rent" & n == 3) {
      Test_data$MedianRent
    } else if (Var3 == "Cost" & n == 3) {
      Test_data$SheltBurd
    } else if (Var3 == "Value" & n == 3) {
      Test_data$MedianHValue
    } else if (Var3 == "Income" & n == 3) {
      Test_data$MedianIncome
    } else if (Var3 == "Imm" & n == 3) {
      Test_data$ImmProp
    } else if (Var3 == "Sex" & n == 3) {
      Test_data$FemaleProp
    } else if (Var3 == "Age" & n == 3) Test_data$MedianAge

  Test_data <- Test_data[order(-Test_data$v1), ]

  axiscalc <- function(var) {
    axis <- extended(min(var), max(var), 3)

    return(axis)
  }

  header1 <-
    if (Var1 == "Own") {
      "Owned\n Households"
    } else if (Var1 == "Rent") {
      "Median \n Monthly Rent"
    } else if (Var1 == "Cost") {
      "Housing \n Cost"
    } else if (Var1 == "Value") {
      "Median \n Housing Value"
    } else if (Var1 == "Income") {
      "Median \n Monthly Income"
    } else if (Var1 == "Imm") {
      "Immigrant \n Households"
    } else if (Var1 == "Sex") {
      "Female \n Householders"
    } else if (Var1 == "Age") "Median \n Householder Age"

  axis1 <-
    if (Var1 == "Own") {
      axiscalc(Test_data$OwningProp)
    } else if (Var1 == "Rent") {
      axiscalc(Test_data$MedianRent)
    } else if (Var1 == "Cost") {
      axiscalc(Test_data$SheltBurd)
    } else if (Var1 == "Value") {
      axiscalc(Test_data$MedianHValue)
    } else if (Var1 == "Income") {
      axiscalc(Test_data$MedianIncome)
    } else if (Var1 == "Imm") {
      axiscalc(Test_data$ImmProp)
    } else if (Var1 == "Sex") {
      axiscalc(Test_data$FemaleProp)
    } else if (Var1 == "Age") axiscalc(Test_data$MedianAge)

  axist1 <-
    if (Var1 == "Own") {
      "Percent (%)"
    } else if (Var1 == "Rent") {
      "Current US$"
    } else if (Var1 == "Cost") {
      "Percent (%)"
    } else if (Var1 == "Value") {
      "Thousand Dollars"
    } else if (Var1 == "Income") {
      "Current US$"
    } else if (Var1 == "Imm") {
      "Percent (%)"
    } else if (Var1 == "Sex") {
      "Percent (%)"
    } else if (Var1 == "Age") "Years"


  header2 <-
    if (Var2 == "Own") {
      "Owned\n Households"
    } else if (Var2 == "Rent") {
      "Median \n Monthly Rent"
    } else if (Var2 == "Cost") {
      "Housing \n Cost"
    } else if (Var2 == "Value") {
      "Median \n Housing Value"
    } else if (Var2 == "Income") {
      "Median \n Monthly Income"
    } else if (Var2 == "Imm") {
      "Immigrant \n Households"
    } else if (Var2 == "Sex") {
      "Female \n Householders"
    } else if (Var2 == "Age") "Median \n Householder Age"

  axist2 <-
    if (Var2 == "Own") {
      "Percent (%)"
    } else if (Var2 == "Rent") {
      "Current US$"
    } else if (Var2 == "Cost") {
      "Percent (%)"
    } else if (Var2 == "Value") {
      "Thousand Dollars"
    } else if (Var2 == "Income") {
      "Current US$"
    } else if (Var2 == "Imm") {
      "Percent (%)"
    } else if (Var2 == "Sex") {
      "Percent (%)"
    } else if (Var2 == "Age") "Years"

  axis2 <-
    if (Var2 == "Own") {
      axiscalc(Test_data$OwningProp)
    } else if (Var2 == "Rent") {
      axiscalc(Test_data$MedianRent)
    } else if (Var2 == "Cost") {
      axiscalc(Test_data$SheltBurd)
    } else if (Var2 == "Value") {
      axiscalc(Test_data$MedianHValue)
    } else if (Var2 == "Income") {
      axiscalc(Test_data$MedianIncome)
    } else if (Var2 == "Imm") {
      axiscalc(Test_data$ImmProp)
    } else if (Var2 == "Sex") {
      axiscalc(Test_data$FemaleProp)
    } else if (Var2 == "Age") axiscalc(Test_data$MedianAge)

  header3 <-
    if (Var3 == "Own") {
      "Owned\n Households"
    } else if (Var3 == "Rent") {
      "Median \n Monthly Rent"
    } else if (Var3 == "Cost") {
      "Housing \n Cost"
    } else if (Var3 == "Value") {
      "Median \n Housing Value"
    } else if (Var3 == "Income") {
      "Median \n Monthly Income"
    } else if (Var3 == "Imm") {
      "Immigrant \n Households"
    } else if (Var3 == "Sex") {
      "Female \n Householders"
    } else if (Var3 == "Age") "Median \n Householder Age"

  axis3 <-
    if (Var3 == "Own") {
      axiscalc(Test_data$OwningProp)
    } else if (Var3 == "Rent") {
      axiscalc(Test_data$MedianRent)
    } else if (Var3 == "Cost") {
      axiscalc(Test_data$SheltBurd)
    } else if (Var3 == "Value") {
      axiscalc(Test_data$MedianHValue)
    } else if (Var3 == "Income") {
      axiscalc(Test_data$MedianIncome)
    } else if (Var3 == "Imm") {
      axiscalc(Test_data$ImmProp)
    } else if (Var3 == "Sex") {
      axiscalc(Test_data$FemaleProp)
    } else if (Var3 == "Age") axiscalc(Test_data$MedianAge)

  axist3 <-
    if (Var3 == "Own") {
      "Percent (%)"
    } else if (Var3 == "Rent") {
      "Current US$"
    } else if (Var3 == "Cost") {
      "Percent (%)"
    } else if (Var3 == "Value") {
      "Thousand Dollars"
    } else if (Var3 == "Income") {
      "Current US$"
    } else if (Var3 == "Imm") {
      "Percent (%)"
    } else if (Var3 == "Sex") {
      "Percent (%)"
    } else if (Var3 == "Age") "Years"

  if (n == 3) {
    p1 <- mmplot(
      stat.data = Test_data,
      map.data = Polys,
      panel.types = c("map", "labels", "dot", "dot", "dot"),
      panel.data = list(NA, "sba", "v1", "v2", "v3"),
      ord.by = "v1",
      median.row = FALSE,
      rev.ord = TRUE,
      colors = brewer.pal(5, "Paired")[c(2, 1, 5, 3, 4)],
      grouping = rep(5, 11),
      vertical.align = "center",
      map.link = c("puma", "ID"),
      panel.att = list(
        list(1,
          inactive.border.color = gray(.7),
          inactive.border.size = .2,
          panel.width = .3,
          map.all = TRUE,
          nodata.fill = "white",
          nodata.border.color = gray(.7),
          nodata.border.size = .5,
          active.border.size = .2,
          active.border.color = "black",
          panel.width = .7
        ),
        list(2,
          header = "Sub-\nBorough",
          panel.width = .1,
          panel.header.size = 1,
          left.margin = -0.5,
          align = "right", text.size = .6
        ),
        list(3,
          header = header1,
          panel.width = .5,
          panel.header.size = 1,
          left.margin = -0.5,
          graph.bgcolor = (brewer.pal(5, "Greys"))[1],
          point.size = .7,
          xaxis.ticks = axiscalc(Test_data$v1),
          xaxis.labels = axiscalc(Test_data$v1),
          xaxis.title = axist1
        ),
        list(4,
          header = header2,
          panel.width = .5,
          panel.header.size = 1,
          left.margin = -0.5,
          graph.bgcolor = (brewer.pal(5, "Greys"))[1],
          point.size = .7,
          xaxis.ticks = axiscalc(Test_data$v2),
          xaxis.labels = axiscalc(Test_data$v2),
          xaxis.title = axist2
        ),
        list(5,
          header = header3,
          panel.width = .5, panel.header.size = 1,
          left.margin = -0.5,
          graph.bgcolor = brewer.pal(5, "Greys")[1],
          point.size = .7,
          xaxis.ticks = axiscalc(Test_data$v3),
          xaxis.labels = axiscalc(Test_data$v3),
          xaxis.title = axist3
        )
      )
    )
  } else if (n == 2) {
    p1 <- mmplot(
      stat.data = Test_data,
      map.data = Polys,
      panel.types = c("map", "labels", "dot", "dot"),
      panel.data = list(NA, "sba", "v1", "v2"),
      ord.by = "v1",
      median.row = FALSE,
      rev.ord = TRUE,
      colors = brewer.pal(5, "Paired")[c(2, 1, 5, 3, 4)],
      grouping = rep(5, 11),
      vertical.align = "center",
      map.link = c("puma", "ID"),
      panel.att = list(
        list(1,
          inactive.border.color = gray(.7),
          inactive.border.size = .2,
          panel.width = .2,
          map.all = TRUE,
          nodata.fill = "white",
          nodata.border.color = gray(.7),
          nodata.border.size = .5,
          active.border.size = .2,
          active.border.color = "black",
          panel.width = .7
        ),
        list(2,
          header = "Sub-\nBorough",
          panel.width = .1,
          panel.header.size = 1,
          left.margin = -0.5,
          align = "right", text.size = .6
        ),
        list(3,
          header = header1,
          panel.width = .59,
          panel.header.size = 1,
          left.margin = -0.5,
          graph.bgcolor = (brewer.pal(5, "Greys"))[1],
          point.size = .7,
          xaxis.ticks = axiscalc(Test_data$v1),
          xaxis.labels = axiscalc(Test_data$v1),
          xaxis.title = axist1
        ),
        list(4,
          header = header2,
          panel.width = .59,
          panel.header.size = 1,
          left.margin = -0.5,
          graph.bgcolor = (brewer.pal(5, "Greys"))[1],
          point.size = .7,
          xaxis.ticks = axiscalc(Test_data$v2),
          xaxis.labels = axiscalc(Test_data$v2),
          xaxis.title = axist2
        )
      )
    )
  }

  return(p1)
}

# FUNCTION 4: Smoothed Scatter plots

# The parameters of this functions are:
#
# DB: Selected Dataset. This was produced in 1.ObjectStep.R code.
#
# Year: 1991, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2014, or 2017.
#
# x: Selected Variable for x-axis.
# This includes "age" (householder age),
# "Rent" (household monthly rent), "lrent" (log10 of household monthly rent),
# "hvalue" (housing value), "mortint" (mortgage interest rate),
# "mhinc" (household monthly income),
# "lrent" (log10 of household monthly income), and
# "cost" (household montlhy cost as income percent)
#
# y: Selected variable for y-axis
# Same options as x
#
# col: discrete variable depicted in color
# This includes "sex" (householder sex), "imm" (householder immigration status),
# "ownrent" (housing ownership status), "mort" (housing mortgage status), and
# "borough" (NYC borough)
#
# shp: discrete variable depicted
# Same options as col
#
# spl: discrete variable to be used to plot the spline
#
# axismeth: Determine method to set axis
# This includes "extended" (to include all the data) and
# "95" (to include the middle 95% of the data)
#
# smooth: Smooth method based on geom_smooth (ggplot2)
# This includes "lm", "glm", "gam", "loess"

scatterplots <- function(DB = NY_data_t, Year = 2017, x = "linc", y = "lrent",
                         col = "borough", shp = "imm", spl = "imm",
                         axismeth = "extended", smooth = "loess") {
  Test_data <- DB[DB$year == Year, ]

  Test_data$sex <-
    ifelse(Test_data$sex == 0, "Male",
      ifelse(Test_data$sex == 1, "Female", NA)
    )

  Test_data$imm <-
    ifelse(Test_data$imm == 0, "US Citizen",
      ifelse(Test_data$imm == 1, "Immigrant", "Other")
    )

  Test_data$ownrent <-
    ifelse(Test_data$own.status == 1, "Own",
      ifelse(Test_data$rent.status == 1, "Rent", NA)
    )

  Test_data$mort_status <-
    ifelse(Test_data$mort.status == 1, "On debt",
      ifelse(Test_data$mort.status == 0, "Debt free", NA)
    )

  varx <-
    if (x == "age") {
      Test_data$age
    } else if (x == "rent") {
      Test_data$mgrent
    } else if (x == "lrent") {
      Test_data$mlogrent
    } else if (x == "hvalue") {
      Test_data$house_value
    } else if (x == "mortint") {
      Test_data$mort_int / 100
    } else if (x == "inc") {
      Test_data$mhhinc
    } else if (x == "linc") {
      Test_data$mloghhinc
    } else if (x == "cost") Test_data$sheltburd * 100

  xaxis <-
    if (x == "age") {
      "Householder Age (in years)"
    } else if (x == "rent") {
      "Household Monthly Gross Rent ($)"
    } else if (x == "lrent") {
      expression(paste(Log[10], "  Household Monthly Gross Rent ($)"))
    } else if (x == "hvalue") {
      "House Gross Value ($)"
    } else if (x == "mortint") {
      "Mortgage Interest Rate (%)"
    } else if (x == "inc") {
      "Household Monthly Gross Income ($)"
    } else if (x == "linc") {
      expression(paste(Log[10], "  Household Monthly Gross Income ($)"))
    } else if (x == "cost") "Housing Cost (%)"

  vary <-
    if (y == "age") {
      Test_data$age
    } else if (y == "rent") {
      Test_data$mgrent
    } else if (y == "lrent") {
      Test_data$mlogrent
    } else if (y == "hvalue") {
      Test_data$house_value
    } else if (y == "mortint") {
      Test_data$mort_int / 100
    } else if (y == "inc") {
      Test_data$mhhinc
    } else if (y == "linc") {
      Test_data$mloghhinc
    } else if (y == "cost") Test_data$sheltburd * 100

  yaxis <-
    if (y == "age") {
      "Householder Age (in years)"
    } else if (y == "rent") {
      "Household Monthly Gross Rent ($)"
    } else if (y == "lrent") {
      expression(paste(Log[10], "  Household Monthly Gross Rent ($)"))
    } else if (y == "hvalue") {
      "House Gross Value ($)"
    } else if (y == "mortint") {
      "Mortgage Interest Rate (%)"
    } else if (y == "inc") {
      "Household Monthly Gross Income ($)"
    } else if (y == "linc") {
      expression(paste(Log[10], "  Household Monthly Gross Income ($)"))
    } else if (y == "cost") "Housing Cost (%)"

  color <-
    if (col == "sex") {
      Test_data$sex
    } else if (col == "imm") {
      Test_data$imm
    } else if (col == "ownrent") {
      Test_data$ownrent
    } else if (col == "mort") {
      Test_data$mort_status
    } else if (col == "borough") Test_data$borough

  palette <-
    if (col == "sex") {
      c("#af8dc3", "#7fbf7b")
    } else if (col == "imm") {
      c("#d8b365", "#5ab4ac")
    } else if (col == "ownrent") {
      c("#e9a3c9", "#a1d76a")
    } else if (col == "mort") {
      c("#f1a340", "#998ec3")
    } else if (col == "borough") {
      c(
        "#b2182b", "#d6604d", "#92c5de",
        "#4393c3", "#2166ac"
      )
    }

  colname <-
    if (col == "sex") {
      "Householder Sex"
    } else if (col == "imm") {
      "Immigration Status"
    } else if (col == "ownrent") {
      "Home Own/Rent Status"
    } else if (col == "mort") {
      "Home Ownership Status"
    } else if (col == "borough") "Borough"

  shape <-
    if (shp == "sex") {
      Test_data$sex
    } else if (shp == "imm") {
      Test_data$imm
    } else if (shp == "ownrent") {
      Test_data$ownrent
    } else if (shp == "mort") {
      Test_data$mort_status
    } else if (shp == "borough") {
      Test_data$borough
    } else if (shp == "none") NULL

  shpname <-
    if (shp == "sex") {
      "Householder Sex"
    } else if (shp == "imm") {
      "Immigration Status"
    } else if (shp == "ownrent") {
      "Home Own/Rent Status"
    } else if (shp == "mort") {
      "Home Mortgage Status"
    } else if (shp == "borough") "Borough"

  Spline <-
    if (spl == "sex") {
      Test_data$sex
    } else if (spl == "imm") {
      Test_data$imm
    } else if (spl == "ownrent") {
      Test_data$ownrent
    } else if (spl == "mort") {
      Test_data$mort_status
    } else if (spl == "borough") {
      Test_data$borough
    } else if (spl == "none") NULL

  splname <-
    if (spl == "sex") {
      "Householder Sex"
    } else if (spl == "imm") {
      "Immigration Status"
    } else if (spl == "ownrent") {
      "Home Own/Rent Status"
    } else if (spl == "mort") {
      "Home Mortgage Status"
    } else if (spl == "borough") "Borough"

  axiscalc <- function(var) {
    var <- na.omit(var)

    axis <-
      if (axismeth == "extended") {
        extended(min(var), max(var), 5)
      } else if (axismeth == "95") {
        c(
          quantile(var, 0.025),
          0, 0, 0,
          quantile(var, 0.975)
        )
      }

    return(axis)
  }

  scplot <- ggplot(
    Test_data,
    aes(
      x = varx, y = vary, na.rm = TRUE,
      colors = color, shape = shape
    )
  ) +
    geom_point(aes(shape = shape, color = color),
      alpha = 0.5, na.rm = TRUE
    ) +
    labs(x = xaxis, y = yaxis) +
    xlim(axiscalc(var = varx)[1], axiscalc(var = varx)[5]) +
    # xlim(1 , 5) +
    # xlim(1.5 , 4) +

    ylim(axiscalc(var = vary)[1], axiscalc(var = vary)[5]) +
    # ylim(1.5 , 4) +
    # ylim(0 , 100) +

    theme(
      axis.text = element_text(size = 20),
      axis.title.y =
        element_text(
          margin = margin(t = 0, r = 20, b = 0, l = 0),
          size = 20
        ),
      axis.title.x =
        element_text(
          margin = margin(t = 20, r = 0, b = 0, l = 0),
          size = 20
        ),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 15)
    ) +
    scale_color_manual(
      name = colname,
      values = palette,
      na.translate = FALSE
    ) +
    scale_shape_discrete(
      name = shpname,
      solid = FALSE,
      na.translate = FALSE
    ) +
    geom_smooth(aes(
      group = color,
      color = color
    ),
    method = smooth,
    se = FALSE
    )

  return(scplot)
}

