# Main Author: Jhonatan Medri
# Article name: Housing Variables and Immigration:
#   An Exploratory Analysis in New York City
# This code was reviewed for the last time on 02/15/2022
# This code creates the information shown in 12 figures and 7 tables
# (Figures 2, 6, 10; 5, 9, 13; 4, 8, 12; 3, 7, 11 & Tables 1 - 7)
# presented in the article.

# Load R Packages
library(classInt)
library(dplyr)
library(formattable)
library(ggmap)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
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
#setwd("C:/Users/jhona/OneDrive/Documents/Classes/USU/Thesis/R Scripts")

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
# show.legend: "T" shows legend and "F" doesn't.

choropleths <- function(data = NY_data,
                        shapefile = NY_shape,
                        year = 2017,
                        variable = "Own",
                        color = "Greens",
                        breaks = 5,
                        dif_break = "N",
                        show.legend = "F") {
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
    "Home Ownership\nPercentage (%)"
  } else if (variable == "Rent") {
    #    "Median Monthly Gross Rent (Current US$)"
    "Median \nMonthly Rent \n(Current US$)"
  } else if (variable == "Cost" |
    variable == "Cost2") {
    "Housing Cost\nPercentage (%)"
  } else if (variable == "Value") {
    "Median Housing Value (US$)"
  } else if (variable == "Imm") {
    "Immigrant Households Percentage (%)"
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
    ) #+

  # ggtitle(year)

  return(choropleth_map)
}

# This code creates Figure 2, page 7.
# 8x10
choropleths(variable = "Own", dif_break = "Y")

# This code creates Figure 6, page 12.
# 8x10
choropleths(variable = "Rent", dif_break = "Y")

# This code creates Figure 10, page 16.
# 8x10
choropleths(variable = "Cost", dif_break = "Y")


#------------ Function 2: Linked micromaps plots  ------------------#

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

micromaps_pub <-
  function(DB = NY_data,
           Shp = NY_shape,
           n = 3,
           Year = 2017,
           Var1 = "Own",
           Var2 = "Rent",
           Var3 = "Cost") {
    DB$OwningProp <- DB$OwningProp * 100
    DB$MortProp <- DB$MortProp * 100
    DB$SheltBurd <- DB$SheltBurd * 100
    DB$ImmProp <- DB$ImmProp * 100

    Polys <- create_map_table(Shp, "puma")

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
      } else if (Var1 == "Age") {
        Test_data$MedianAge
      } else if (Var1 == "Mort") {
        Test_data$MortProp
      }

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
      } else if (Var2 == "Age") {
        Test_data$MedianAge
      } else if (Var2 == "Mort") {
        Test_data$MortProp
      }

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
      } else if (Var3 == "Age" & n == 3) {
        Test_data$MedianAge
      } else if (Var3 == "Mort") {
        Test_data$MortProp
      }

    Test_data <- Test_data[order(-Test_data$v1), ]

    axiscalc <- function(var) {
      axis <- extended(min(var), max(var), m = 3)

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
      } else if (Var1 == "Age") {
        "Median \n Householder Age"
      } else if (Var1 == "Mort") {
        "Households \n with Mortgage"
      }

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
      } else if (Var1 == "Age") {
        axiscalc(Test_data$MedianAge)
      } else if (Var1 == "Mort") {
        axiscalc(Test_data$MortProp)
      }

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
      } else if (Var1 == "Age") {
        "Years"
      } else if (Var1 == "Mort") {
        "Percent (%)"
      }


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
      } else if (Var2 == "Age") {
        "Median \n Householder Age"
      } else if (Var2 == "Mort") {
        "Households \n with Mortgage"
      }

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
      } else if (Var2 == "Age") {
        "Years"
      } else if (Var2 == "Mort") {
        "Percent (%)"
      }

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
      } else if (Var2 == "Age") {
        axiscalc(Test_data$MedianAge)
      } else if (Var2 == "Mort") {
        axiscalc(Test_data$MortProp)
      }

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
      } else if (Var3 == "Age") {
        "Median \n Householder Age"
      } else if (Var3 == "Mort") {
        "Households \n with Mortgage"
      }

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
      } else if (Var3 == "Age") {
        axiscalc(Test_data$MedianAge)
      }

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
      } else if (Var3 == "Age") {
        "Years"
      } else if (Var3 == "Mort") {
        "Percent (%)"
      }

    if (n == 3) {
      p1 <- mmplot(
        stat.data = Test_data[1:28, ],
        map.data = Polys,
        panel.types = c("map", "labels", "dot", "dot", "dot"),
        panel.data = list(NA, "sba", "v1", "v2", "v3"),
        ord.by = "v1",
        median.row = FALSE,
        rev.ord = TRUE,
        colors = brewer.pal(5, "Paired")[c(2, 1, 5, 3, 4)],
        grouping = c(5, 5, 4, 4, 5, 5),
        vertical.align = "center",
        map.link = c("puma", "ID"),
        panel.att = list(
          list(
            1,
            inactive.border.color = gray(.7),
            inactive.border.size = .2,
            panel.width = .7,
            map.all = TRUE,
            nodata.fill = "white",
            nodata.border.color = gray(.7),
            nodata.border.size = .5,
            active.border.size = .2,
            active.border.color = "black",
            panel.width = .7
          ),
          list(
            2,
            header = "Sub-\nBorough",
            panel.width = .25,
            panel.header.size = 1,
            left.margin = -0.5,
            align = "right",
            text.size = .6
          ),
          list(
            3,
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
          list(
            4,
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
          list(
            5,
            header = header3,
            panel.width = .5,
            panel.header.size = 1,
            left.margin = -0.5,
            graph.bgcolor = brewer.pal(5, "Greys")[1],
            point.size = .7,
            xaxis.ticks = axiscalc(Test_data$v3),
            xaxis.labels = axiscalc(Test_data$v3),
            xaxis.title = axist3
          )
        )
      )
    }

    p2 <- {
      mmplot(
        stat.data = Test_data[29:55, ],
        map.data = Polys,
        panel.types = c("map", "labels", "dot", "dot", "dot"),
        panel.data = list(NA, "sba", "v1", "v2", "v3"),
        ord.by = "v1",
        median.row = FALSE,
        rev.ord = TRUE,
        colors = brewer.pal(5, "Paired")[c(2, 1, 5, 3, 4)],
        grouping = c(5, 5, 4, 4, 4, 5),
        vertical.align = "center",
        map.link = c("puma", "ID"),
        panel.att = list(
          list(
            1,
            inactive.border.color = gray(.7),
            inactive.border.size = .2,
            panel.width = .7,
            map.all = TRUE,
            nodata.fill = "white",
            nodata.border.color = gray(.7),
            nodata.border.size = .5,
            active.border.size = .2,
            active.border.color = "black",
            panel.width = .7
          ),
          list(
            2,
            header = "Sub-\nBorough",
            panel.width = .25,
            panel.header.size = 1,
            left.margin = -0.5,
            align = "right",
            text.size = .6
          ),
          list(
            3,
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
          list(
            4,
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
          list(
            5,
            header = header3,
            panel.width = .5,
            panel.header.size = 1,
            left.margin = -0.5,
            graph.bgcolor = brewer.pal(5, "Greys")[1],
            point.size = .7,
            xaxis.ticks = axiscalc(Test_data$v3),
            xaxis.labels = axiscalc(Test_data$v3),
            xaxis.title = axist3
          )
        )
      )
    }


    lwidth <- p2$layout$widths
    nPanels <- 5
    lmLayout <- grid.layout(
      nrow = 1,
      ncol = length(lwidth) * 2,
      widths = unit(lwidth, rep("null", length(lwidth))),
      heights = unit(rep(1, length(lwidth)), rep("null", length(lwidth)))
    )
    subplot <-
      function(x, y) {
        viewport(layout.pos.row = x, layout.pos.col = y)
      }
    ## subplot is specifying which cell in the layout to populate
    ## i.e. subplot(row number, column number)
    ## again, panel number has to account for small blank
    ## spacing columns, hence the p*2 (where p is the panel number)
    return({
      grid.newpage()
      pushViewport(viewport(layout = lmLayout))
      suppressWarnings(for (p in 1:nPanels) {
        print(p1[[p]], vp = subplot(1, p * 2))
      })
      suppressWarnings(for (p in 1:nPanels) {
        print(p2[[p]], vp = subplot(1, nPanels * 2 + 1 +
          p *
            2))
      })
    })
  }

# Reset theme to default
theme_set(theme_grey())

# This code produces Figure 5, page 10.
micromaps_pub(
  Var1 = "Own",
  Var2 = "Mort",
  Var3 = "Imm",
  n = 3
)

# This code produces Figure 9, page 14.
micromaps_pub(
  Var1 = "Rent",
  Var2 = "Income",
  Var3 = "Imm",
  n = 3
)

# This code produces Figure 13, page 18.
micromaps_pub(
  Var1 = "Cost",
  Var2 = "Rent",
  Var3 = "Imm",
  n = 3
)


#------------ Function 3: Smoothed scatterplots  ------------------#

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

scatterplots <-
  function(DB = NY_data_t,
           Year = 2017,
           x = "linc",
           y = "lrent",
           col = "borough",
           shp = "imm",
           spl = "imm",
           smooth = "loess") {
    Test_data <- DB[DB$year == Year, ]

    Test_data$sex <-
      ifelse(Test_data$sex == 0,
        "Male",
        ifelse(Test_data$sex == 1, "Female", NA)
      )

    Test_data$imm <-
      ifelse(Test_data$imm == 0,
        "US Citizen",
        ifelse(Test_data$imm == 1, "Immigrant", "Other")
      )

    Test_data$ownrent <-
      ifelse(Test_data$own.status == 1,
        "Own",
        ifelse(Test_data$rent.status == 1, "Rent", NA)
      )

    Test_data$mort.status <-
      ifelse(
        Test_data$mort.status == 1,
        "On debt",
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
        Test_data$house.value
      } else if (x == "mortint") {
        Test_data$mort.int / 100
      } else if (x == "inc") {
        Test_data$mhhinc
      } else if (x == "linc") {
        Test_data$mloghhinc
      } else if (x == "cost") {
        Test_data$sheltburd * 100
      }

    xaxis <-
      if (x == "age") {
        "Householder Age (in years)"
      } else if (x == "rent") {
        "Household Monthly Rent (Current $)"
      } else if (x == "lrent") {
        expression(paste(Log[10], "  Household Monthly Rent (Current $)"))
      } else if (x == "hvalue") {
        "House Value (Current $)"
      } else if (x == "mortint") {
        "Mortgage Interest Rate (%)"
      } else if (x == "inc") {
        "Household Monthly Income (Current $)"
      } else if (x == "linc") {
        expression(paste(Log[10], "  Household Monthly Income (Current $)"))
      } else if (x == "cost") {
        "Housing Cost (%)"
      }

    vary <-
      if (y == "age") {
        Test_data$age
      } else if (y == "rent") {
        Test_data$mgrent
      } else if (y == "lrent") {
        Test_data$mlogrent
      } else if (y == "hvalue") {
        Test_data$house.value
      } else if (y == "mortint") {
        Test_data$mort.int / 100
      } else if (y == "inc") {
        Test_data$mhhinc
      } else if (y == "linc") {
        Test_data$mloghhinc
      } else if (y == "cost") {
        Test_data$sheltburd * 100
      }

    yaxis <-
      if (y == "age") {
        "Householder Age (in years)"
      } else if (y == "rent") {
        "Household Monthly Rent (Current US$)"
      } else if (y == "lrent") {
        expression(paste(Log[10], "  Household Monthly Rent (Current US$)"))
      } else if (y == "hvalue") {
        "House Value (Current US$)"
      } else if (y == "mortint") {
        "Mortgage Interest Rate (%)"
      } else if (y == "inc") {
        "Household Monthly Income (Current US$)"
      } else if (y == "linc") {
        expression(paste(Log[10], "  Household Monthly Income (Current US$)"))
      } else if (y == "cost") {
        "Housing Cost (%)"
      }

    color <-
      if (col == "sex") {
        Test_data$sex
      } else if (col == "imm") {
        Test_data$imm
      } else if (col == "ownrent") {
        Test_data$ownrent
      } else if (col == "mort") {
        Test_data$mort.status
      } else if (col == "borough") {
        Test_data$borough
      }

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
      } else if (col == "borough") {
        "Borough"
      }

    shape <-
      if (shp == "sex") {
        Test_data$sex
      } else if (shp == "imm") {
        Test_data$imm
      } else if (shp == "ownrent") {
        Test_data$ownrent
      } else if (shp == "mort") {
        Test_data$mort.status
      } else if (shp == "borough") {
        Test_data$borough
      } else if (shp == "None") {
        NULL
      }

    shpname <-
      if (shp == "sex") {
        "Householder Sex"
      } else if (shp == "imm") {
        "Immigration Status"
      } else if (shp == "ownrent") {
        "Home Own/Rent Status"
      } else if (shp == "mort") {
        "Home Mortgage Status"
      } else if (shp == "borough") {
        "Borough"
      }

    Spline <-
      if (spl == "sex") {
        Test_data$sex
      } else if (spl == "imm") {
        Test_data$imm
      } else if (spl == "ownrent") {
        Test_data$ownrent
      } else if (spl == "mort") {
        Test_data$mort.status
      } else if (spl == "borough") {
        Test_data$borough
      } else if (spl == "none") {
        NULL
      }

    splname <-
      if (spl == "sex") {
        "Householder Sex"
      } else if (spl == "imm") {
        "Immigration Status"
      } else if (spl == "ownrent") {
        "Home Own/Rent Status"
      } else if (spl == "mort") {
        "Home Mortgage Status"
      } else if (spl == "borough") {
        "Borough"
      }

    axiscalc <- function(var) {
      var <- na.omit(var)

      axis <- extended(min(var), max(var), 5)


      return(axis)
    }

    scplot <- ggplot(
      Test_data,
      aes(
        x = varx,
        y = vary,
        na.rm = TRUE,
        colors = color,
        shape = shape
      )
    ) +
      geom_point(aes(shape = shape, color = color),
        alpha = 0.5,
        na.rm = TRUE
      ) +
      labs(x = xaxis, y = yaxis) +
      xlim(axiscalc(var = varx)[1], axiscalc(var = varx)[5]) +
      ylim(axiscalc(var = vary)[1], axiscalc(var = vary)[5]) +
      theme(
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(margin = margin(
          t = 0,
          r = 20,
          b = 20,
          l = 0
        ), size = 20),
        axis.title.x = element_text(margin = margin(
          t = 20,
          r = 0,
          b = 20,
          l = 0
        ), size = 20),
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

# This feature sets graphs with a white background
theme_set(
  theme_bw() +
    theme(legend.position = "bottom")
)

# 8x10
# This code produces Figure 4, page 9.
scatterplots(
  y = "mortint", x = "inc", col = "imm", spl = "imm",
  shp = "None", Year = 2017
) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10^1.5, 10^5.5)
  ) +
  xlab(expression("Household Monthly Income (Current US$)"))

# This code produces Figure 8, page 13.
scatterplots(
  y = "rent", x = "inc", col = "imm", spl = "imm",
  shp = "None", Year = 2017
) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10^1.5, 10^4)
  ) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  ylab(expression("Household Monthly Rent (Current US$)")) +
  xlab(expression("Household Monthly Income (Current US$)")) +
  geom_abline(intercept = 0, slope = 1)

# This code produces Figure 12, page 17.
# 16x16
scatterplots(
  y = "cost", x = "rent", col = "imm", spl = "imm",
  shp = "None", Year = 2017
) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(10^1.5, 10^4)
  ) +
  xlab(expression("Household Monthly Rent (Current US$)")) +
  ylim(0, 100)



#------------ Function 4: Boxdot plots  ------------------#

# The parameters of this functions are:
#
# Datat: Selected Dataset. This was produced in 1.ObjectStep.R code and
# includes the raw data
#
# Data55: Selected Dataset. This was produced in 1.ObjectStep.R code and
# includes the aggregated data by the 55 sub-boroughs
#
# Year: 1991, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2014, or 2017.
#
# x: Fixed (can't be changed). Depicts each of the 5 boroughs.
# "BK" (Brooklyn), "BX" (The Bronx), "MN" (Manhattan),
# "QN" (Queens), "SI" (Staten Island)
#
# y: "Rate" (mortgage interest rate),
# "Cost" (household montlhy cost as income percent), and
# "Rent" ((household monthly rent))

boxdotplot <- function(Datat = NY_data_t,
                       Data55 = NY_data,
                       Year = 2017,
                       x = "borough",
                       y = "Rate") {
  Test_data_t <- NY_data_t[NY_data_t$year == Year, ]
  Test_data_55 <- NY_data[NY_data$year == Year, ]

  Test_data_55$vary <- if (y == "Rate") {
    Test_data_55$MedianRate * 10
  } else if (y == "Rent") {
    Test_data_55$MedianRent
  } else if (y == "Cost") {
    Test_data_55$SheltBurd * 100
  }

  Test_data_t$vary <- if (y == "Rate") {
    Test_data_t$mort.int / 100
  } else if (y == "Rent") {
    Test_data_t$mgrent
  } else if (y == "Cost") {
    Test_data_t$sheltburd * 100
  }

  YLIM <- if (y == "Rate") {
    c(0, 8)
  } else if (y == "Rent") {
    c(0, 6000)
  } else if (y == "Cost") {
    c(0, 100)
  }

  yname <- if (y == "Rate") {
    "Mortgage Interest Rate (%)"
  } else if (y == "Rent") {
    "Median Monthly Rent (Current US$)"
  } else if (y == "Cost") {
    "Housing Cost (%)"
  }

  plot <- ggplot(NULL) +
    geom_point(
      data = Test_data_55,
      aes(
        y = vary,
        x = borough,
        color = borough
      ),
      alpha = 0.5,
      size = 4
    ) +
    geom_boxplot(
      data = Test_data_t,
      aes(
        y = vary,
        x = borough,
        color = borough
      ),
      alpha = 0,
      lwd = 0.7
    ) +
    scale_color_manual(values = c(
      "#b2182b", "#d6604d", "#92c5de",
      "#4393c3", "#2166ac"
    )) +
    coord_flip() +
    xlab("Borough") +
    ylab(yname) +
    ylim(YLIM) +
    scale_x_discrete(limits = c("SI", "QN", "MN", "BX", "BK")) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(size = 20),
      axis.title = element_text(size = 20, face = "bold"),
      title = element_text(size = 22, face = "bold"),
      axis.title.y = element_text(margin = margin(
        t = 0,
        r = 20,
        b = 20,
        l = 0
      )),
      axis.title.x = element_text(margin = margin(
        t = 20,
        r = 0,
        b = 20,
        l = 0
      )),
      legend.position = "none"
    )

  return(plot)
}

# 8x10
# This code produces Figure 3, page 9.
boxdotplot(y = "Rate")

# This code produces Figure 7, page 13.
boxdotplot(y = "Rent")

# This code produces Figure 11, page 17.
boxdotplot(y = "Cost")


#------------ 5: Descriptive statistics ------------------#

# This code produces the Sub-borough Range in Table 1, page 3.

NY_data[NY_data$year == 2017, ] %>%
  summarise(
    MinOwn = min(OwningProp),
    MaxOwn = max(OwningProp),
    MinMort = min(MortProp),
    MaxMort = max(MortProp),
    MinMortInt = min(MedianRate * 10, na.rm = TRUE),
    MaxMortInt = max(MedianRate * 10, na.rm = TRUE),
    MinHValue = min(MedianHValue),
    MaxHValue = max(MedianHValue),
    MinRentProp = min(RentingProp),
    MaxRentProp = max(RentingProp),
    MinRent = min(MedianRent),
    MaxRent = max(MedianRent),
    MinHCost = min(SheltBurd),
    MaxHCost = max(SheltBurd),
    MinImmP = min(ImmProp),
    MaxImmProp = max(ImmProp),
    MinSex = 1 - max(MaleProp),
    MaxSex = 1 - min(MaleProp),
    MinAge = min(MedianAge),
    MaxAge = max(MedianAge),
    MinInc = min(MedianIncome / 12),
    MaxInc = max(MedianIncome / 12)
  )

# This code produces information in Table 2, page 5.

NY_data %>%
  filter(year == 2017) %>%
  group_by(borough) %>%
  dplyr::summarize(
    OwnerProp = mean(OwningProp),
    MortProp = mean(MortProp),
    MortRate = mean(MedianRate * 10, na.rm = TRUE),
    MedianHValue = mean(MedianHValue)
  )

NY_data %>%
  filter(year == 2017) %>%
  dplyr::summarize(
    OwnerProp = mean(OwningProp),
    MortProp = mean(MortProp),
    MortRate = mean(MedianRate * 10, na.rm = TRUE),
    MedianHValue = mean(MedianHValue)
  )

# This code produces information in Table 3, page 6.

NY_data %>%
  filter(year == 2017) %>%
  group_by(borough) %>%
  dplyr::summarize(
    RentProp = mean(RentingProp),
    Rent = mean(MedianRent),
    Income = mean(MedianIncome / 12),
    Cost = mean(Rent / Income)
  )

NY_data %>%
  filter(year == 2017) %>%
  dplyr::summarize(
    RentProp = mean(RentingProp),
    Rent = mean(MedianRent),
    Income = mean(MedianIncome / 12),
    Cost = mean(Rent / Income)
  )

# This code produces information in Table 4, page 6.

NY_data %>%
  filter(year == 2017) %>%
  group_by(borough) %>%
  dplyr::summarize(
    ImmProp = mean(ImmProp),
    FemaleProp = mean(FemaleProp),
    Age = mean(MedianAge)
  )

NY_data %>%
  filter(year == 2017) %>%
  dplyr::summarize(
    ImmProp = mean(ImmProp),
    FemaleProp = mean(FemaleProp),
    Age = mean(MedianAge)
  )

#------------ 6: Correlations ------------------#

# Raw Data Correlations shown in Table 5, page 11;
# Table 6, page 15 & Table 7, page 19.
# Numbers were manually inserted later into the paper.

# Correlations in NYC, in all boroughs
cor(NY_data_t[
  NY_data_t$year == 2017,
  c(20, 22, 9, 18, 25, 28)
],
use = "pairwise.complete.obs"
)

# Correlations in the Brooklyn borough
cor(NY_data_t[
  NY_data_t$year == 2017 &
    NY_data_t$borough == "BK",
  c(20, 22, 9, 18, 25, 28)
],
use = "pairwise.complete.obs"
)

# Correlations in the Bronx borough
cor(NY_data_t[
  NY_data_t$year == 2017 &
    NY_data_t$borough == "BX",
  c(20, 22, 9, 18, 25, 28)
],
use = "pairwise.complete.obs"
)

# Correlations in the Manhattan borough
cor(NY_data_t[
  NY_data_t$year == 2017 &
    NY_data_t$borough == "MN",
  c(20, 22, 9, 18, 25, 28)
],
use = "pairwise.complete.obs"
)

# Correlations in the Queens borough
cor(NY_data_t[
  NY_data_t$year == 2017 &
    NY_data_t$borough == "QN",
  c(20, 22, 9, 18, 25, 28)
],
use = "pairwise.complete.obs"
)

# Correlations in the Staten Island borough
cor(NY_data_t[
  NY_data_t$year == 2017 &
    NY_data_t$borough == "SI",
  c(20, 22, 9, 18, 25, 28)
],
use = "pairwise.complete.obs"
)

# Aggregated Data Correlations shown in Table 5, page 11;
# Table 6, page 15 & Table 7, page 19.
# Numbers were manually inserted later into the paper.

# Correlations in NYC, in all boroughs
cor(NY_data[
  NY_data$year == 2017,
  c(17, 20, 9, 13, 12, 22)
],
use = "pairwise.complete.obs"
)

# Correlations in NYC, in the Brooklyn borough
cor(NY_data[
  NY_data$year == 2017 &
    NY_data$borough == "BK",
  c(17, 20, 9, 13, 12, 22)
],
use = "pairwise.complete.obs"
)

# Correlations in NYC, in the Bronx borough
cor(NY_data[
  NY_data$year == 2017 &
    NY_data$borough == "BX",
  c(17, 20, 9, 13, 12, 22)
],
use = "pairwise.complete.obs"
)

## Correlations in NYC, in the Manhattan borough
cor(NY_data[
  NY_data$year == 2017 &
    NY_data$borough == "MN",
  c(17, 20, 9, 13, 12, 22)
],
use = "pairwise.complete.obs"
)

# Correlations in NYC, in the Queens borough
cor(NY_data[
  NY_data$year == 2017 &
    NY_data$borough == "QN",
  c(17, 20, 9, 13, 12, 22)
],
use = "pairwise.complete.obs"
)

# Correlations in NYC, in the Staten Island borough
cor(NY_data[
  NY_data$year == 2017 &
    NY_data$borough == "SI",
  c(17, 20, 9, 13, 12, 22)
],
use = "pairwise.complete.obs"
)

