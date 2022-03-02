# Main Author: Jhonatan Medri
# Article name: Housing Variables and Immigration:
#   An Exploratory Analysis in New York City
# This code was reviewed for the last time on 02/15/2022
# This code builds spatial autocorrelation functions and presents some results.
# This code produces 5 figures (Figures 1, 14-17) presented in the article.

# Load R Packages
library(directlabels)
library(dplyr)
library(ggmap)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(plyr)
library(RColorBrewer)
library(rgdal)
library(sp)
library(spdep)
library(styler)
library(tidyr)

# Set Work Directory and Seed
#setwd("C:/Users/jhona/OneDrive/Documents/Classes/USU/Thesis/R Scripts")
set.seed(6901)

# Read Shapefile and Data, and create a dataframe
NY_shape <- readOGR(
  dsn = "./NYPUMA",
  layer = "nypuma",
  verbose = TRUE
)
NY_data <- read.csv("NYCHVS55.csv")
NY_data$puma <- as.character(NY_data$puma)
NY_shape@data$id <- rownames(NY_shape@data)
NY_shapedf <- join(fortify(NY_shape), NY_shape@data, by = "id")

# NYC boroughs and sub-boroughs
# The following code produces Figure 1, page 4
NY_shapedf$Borough <-
  ifelse(NY_shapedf$Bor == "BX", "The Bronx",
    ifelse(NY_shapedf$Bor == "BK", "Brooklyn",
      ifelse(NY_shapedf$Bor == "QN", "Queens",
        ifelse(NY_shapedf$Bor == "SI", "Staten Island",
          "Manhattan"
        )
      )
    )
  )

qmplot(
  long,
  lat,
  group = group,
  data = NY_shapedf,
  source = "osm",
  maptype = "roadmap",
  zoom = 11,
  geom = "tile",
  darken = .1,
  mapcolor = "bw"
) +
  geom_polygon(
    data = NY_shapedf,
    aes(
      x = long,
      y = lat,
      group = group,
      fill = Bor
    ),
    colour = "black",
    alpha = 0.5,
    linetype = 2
  ) +

  directlabels::geom_dl(aes(label = Borough),
    method = list("smart.grid", cex = 2)
  ) +
  scale_fill_manual(
    name = "Borough",
    values = c(
      "#b2182b", "#d6604d", "#92c5de",
      "#4393c3", "#2166ac"
    ),
    labels = c(
      "Brooklyn", "Bronx", "Manhattan",
      "Queens", "Staten Island"
    )
  ) +

  theme(legend.position = "none")


#--------- Function for spatial autocorrelation tests  ---------------#
#
# The following code produces a function 
# to conduct spatial autocorrelation tests
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
# Year. 1991, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2014, or 2017.
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
# Alt. Alternative hypothesis. a character string specifying the alternative
# hypothesis, must be one of "greater" (default), "less" or "two.sided".


statdeptests <- function(DB = NY_data,
                         Shp = NY_shape,
                         Var = "Own",
                         Year = 2017,
                         Test = "Moran",
                         NbW = "Queens",
                         kn = 0,
                         dist = 0,
                         Alt = "greater") {
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
    } else if (NbW == "KNN") {
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

  Test_results <-
    suppressWarnings(
      if (Variable == "Check Variable Spelling") {
        c("Check Variable Spelling")
      } else if (Test == "Moran") {
        c(
          moran.test(Variable, randomisation = FALSE, Weights)$estimate[1],
          moran.test(Variable, randomisation = FALSE, Weights)$p.value,
          Test,
          Var,
          Year,
          NbW,
          kn,
          dist
        )
      } else if (Test == "Geary") {
        c(
          geary.test(Variable, randomisation = FALSE, Weights)$estimate[1],
          geary.test(Variable, randomisation = FALSE, Weights)$p.value,
          Test,
          Var,
          Year,
          NbW,
          kn,
          dist
        )
      } else {
        "Check Test Spelling"
      }
    )

  return(Test_results)
}

# We test the function with different parameters
statdeptests(Test = "Moran", NbW = "Queens")
statdeptests(Test = "Moran", NbW = "KNN", kn = 6)
statdeptests(Test = "Geary", NbW = "Dist", dist = 8)

# The following code produces a loop to calculate the spatial autocorrelation
# tests in all 10 surveys and 55 sub-boroughs, tuning  different methods or
# parameters. We tune for the 2, 3, 4, and 5 closest neighbors in the kn method,
# and for 8, 9, 10, and 11 kns in the proximity method.

alltests <- c()
for (i in c("Moran", "Geary")) {
  for (j in c(
    "Own", "Rent", "Cost", "Value",
    "Income", "Imm", "Sex", "Age"
  )) {
    for (k in c(
      "1991",
      "1993",
      "1996",
      "1999",
      "2002",
      "2005",
      "2008",
      "2011",
      "2014",
      "2017"
    )) {
      for (l in c("Queens", "KNN", "Dist")) {
        for (m in c("0", "2", "3", "4", "5")) {
          for (n in c("0", "08", "09", "10", "11")) {
            if (l == "Queens" & m == "0" & n == "0" |
              l == "KNN" & m != "0" & n == "0" |
              l == "Dist" & m == "0" & n != "0") {
              alltests <- rbind(
                alltests,
                statdeptests(
                  Test = i,
                  Var = j,
                  Year = k,
                  NbW = l,
                  kn = m,
                  dist = n
                )
              )
            } else {

            }
          }
        }
      }
    }
  }
}

alltests <- as.data.frame(alltests)
colnames(alltests) <- c(
  "Test_Statistic",
  "Pvalue",
  "Test",
  "Variable",
  "Year",
  "Weight",
  "kn",
  "maxdist"
)

#--------- Function for spatial autocorrelation plots  ---------------#

# This code produces a function to plot spatial autocorrelation results.
# Function displays 2 plots: One for Moran's I and one for Geary's C statistics.
#
# The parameters of the function are:
#
# data = This dataset was produced in the previous step, and named "alltests"
# Var = Same variables as in the statdeptests functions.
# NbW = Same methods as in the statdeptests functions.
# k_n = Includes the 2, 3, 4, and 5 closest neighbors
# dist = Includes 8, 9, 10, and 11 kms.

plot_tests <- function(data = alltests, Var = "Own",
                       NbW = "Queens", k_n = 0, dist = 0) {
  data$signif <- ifelse(as.numeric(data$Pvalue) < 0.05,
    "Significant", "Not significant"
  )

  Var_name <-
    if (Var == "Own") {
      "Home Ownership Rate (%)"
    } else if (Var == "Rent") {
      "Monthly Gross Rent (US$)"
    } else if (Var == "Cost") {
      "Housing Cost (%)"
    } else if (Var == "Value") {
      "Housing Value (US$)"
    } else if (Var == "Income") {
      "Householder Monthly Gross Income (US$)"
    } else if (Var == "Imm") {
      "Immigration Proportion (%)"
    } else if (Var == "Sex") {
      "Female Proportion (%)"
    } else if (Var == "Age") {
      "Householder Age (in years)"
    } else {
      "Undefined Variable"
    }

  plot1yaxis <-
    if (Var == "Own" & NbW == "KN") {
      c(0.4, 0.7)
    } else if (Var == "Rent" & NbW == "KN") {
      c(0.3, 0.6)
    } else if (Var == "Cost" & NbW == "KN") {
      c(0.05, 0.5)
    } else if (Var == "Imm" & NbW == "KN") {
      c(0.15, 0.7)
    } else if (Var == "Own" & NbW == "Dist") {
      c(0.3, 0.65)
    } else if (Var == "Rent" & NbW == "Dist") {
      c(-0.05, 0.3)
    } else if (Var == "Cost" & NbW == "Dist") {
      c(-0.05, 0.25)
    } else if (Var == "Imm" & NbW == "Dist") {
      c(0, 0.4)
    } else if (Var == "Value" & NbW == "All") {
      c(0, 0.5)
    } else if (Var == "Income" & NbW == "All") {
      c(0.2, 0.6)
    } else if (Var == "Sex" & NbW == "All") {
      c(0.1, 0.7)
    } else if (Var == "Age" & NbW == "All") {
      c(0, 0.6)
    } else if (Var == "Own" & NbW == "All") {
      c(0.45, 0.75)
    } else if (Var == "Rent" & NbW == "All") {
      c(0, 0.6)
    } else if (Var == "Cost" & NbW == "All") {
      c(-0.1, 0.6)
    } else if (Var == "Imm" & NbW == "All") c(0.1, 0.7)


  plot2yaxis <-
    if (Var == "Own" & NbW == "KN") {
      c(0.25, 0.5)
    } else if (Var == "Rent" & NbW == "KN") {
      c(0.35, 0.7)
    } else if (Var == "Cost" & NbW == "KN") {
      c(0.5, 1)
    } else if (Var == "Imm" & NbW == "KN") {
      c(0.3, 0.8)
    } else if (Var == "Own" & NbW == "Dist") {
      c(0.3, 0.6)
    } else if (Var == "Rent" & NbW == "Dist") {
      c(0.65, 1.05)
    } else if (Var == "Cost" & NbW == "Dist") {
      c(0.7, 1.05)
    } else if (Var == "Imm" & NbW == "Dist") {
      c(0.6, 1)
    } else if (Var == "Value" & NbW == "All") {
      c(0.4, 0.95)
    } else if (Var == "Income" & NbW == "All") {
      c(0.4, 0.8)
    } else if (Var == "Sex" & NbW == "All") {
      c(0.3, 0.8)
    } else if (Var == "Age" & NbW == "All") {
      c(0.35, 0.95)
    } else if (Var == "Own" & NbW == "All") {
      c(0.25, 0.55)
    } else if (Var == "Rent" & NbW == "All") {
      c(0.3, 0.9)
    } else if (Var == "Cost" & NbW == "All") {
      c(0.4, 1.1)
    } else if (Var == "Imm" & NbW == "All") c(0.35, 0.9)


  theme_set(theme_bw())

  plot1 <-
    ggplot(
      data = data[data$Test == "Moran" &
        data$Variable == Var & {
        if (NbW == "KN") {
          data$Weight == "KN"
        } else if (NbW == "Dist") {
          data$Weight == "Dist"
        } else if (NbW == "Queens") {
          data$Weight == "Queens"
        } else if (NbW == "All") {
          data$kn %in% c(0, k_n) &
            data$maxdist %in% c(0, dist)
        }
      }, ],
      aes(x = Year, y = as.numeric(Test_Statistic))
    ) +
    geom_point(
      aes(
        color = if (NbW == "KN") {
          kn
        } else if (NbW == "Dist") {
          maxdist
        } else if (NbW == "Queens") {
          "red"
        } else if (NbW == "All") {
          Weight
        },
        shape = signif
      ),
      size = 3,
      stroke = 1
    ) +
    labs(x = "Year", y = "I Values") +
    scale_color_manual(
      name = {
        if (NbW == "KN") {
          "KNN"
        } else if (NbW == "Dist") {
          "Distance (km)"
        } else {
          "Method"
        }
      },
      labels = if (NbW == "All") {
        c("Distance", "KNN", "Queen")
      },
      values = c("#1b9e77", "#d95f02", "#7570b3")
    ) +
    scale_shape_manual(
      name = "Significance",
      values = {if(Var == "Cost") {c(1, 16)}
        else {c(16,1)}}
    ) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 14),
      plot.title = element_text(hjust = 0.5, size = 22)
    ) +
    ylim(plot1yaxis) +
    ggtitle("Moran's I Statistic")

  plot2 <-
    ggplot(
      data = data[data$Test == "Geary" &
        data$Variable == Var & {
        if (NbW == "KN") {
          data$Weight == "KN"
        } else if (NbW == "Dist") {
          data$Weight == "Dist"
        } else if (NbW == "Queens") {
          data$Weight == "Queens"
        } else if (NbW == "All") {
          data$kn %in% c(0, k_n) &
            data$maxdist %in% c(0, dist)
        }
      }, ],
      aes(x = Year, y = as.numeric(Test_Statistic))
    ) +
    geom_point(
      aes(
        color = if (NbW == "KN") {
          kn
        } else if (NbW == "Dist") {
          maxdist
        } else if (NbW == "Queens") {
          "red"
        } else if (NbW == "All") {
          Weight
        },
        shape = signif
      ),
      size = 3,
      stroke = 1
    ) +
    labs(x = "Year", y = "C Values") +
    scale_color_manual(
      name = {
        if (NbW == "KN") {
          "KNN"
        } else if (NbW == "Dist") {
          "Distance (km)"
        } else {
          "Method"
        }
      },
      labels = if (NbW == "All") {
        c("Distance", "KNN", "Queen")
      },
      values = c("#1b9e77", "#d95f02", "#7570b3")
    ) +
    scale_shape_manual(
      name = "Significance",
      values = {if(Var == "Cost") {c(1, 16)}
      else {c(16,1)}}
    ) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 14),
      plot.title = element_text(hjust = 0.5, size = 22)
    ) +
    ylim(plot2yaxis) +
    ggtitle("Geary's C Statistic")

  plots <- ggarrange(
    plot1,
    plot2,
    ncol = 2,
    common.legend = TRUE,
    legend = "right"
  )

  return(plots)
}

# After looking at different tests, we identified that k_n = 3 and dist = 8
# produced the best statistics in the k-neighbor and the maximum proximity
# methods respectively. That is why, we plot those two methods, and Queens.

# Figure 14, page 23.
# NYC home ownership percentage spatial autocorrelation 1991 - 2017.
# 7x14
plot_tests(
  Var = "Own",
  NbW = "All",
  k_n = 3,
  dist = "08"
)

# Figure 15, page 24.
# NYC median household rent spatial autocorrelation 1991 - 2017.
# 7x14
plot_tests(
  Var = "Rent",
  NbW = "All",
  k_n = 3,
  dist = "08"
)

# Figure 16, page 24.
# NYC housing cost percentage spatial autocorrelation 1991 - 2017.
# Remember to fix ylim in the function just for this plot
# 7x14
plot_tests(
  Var = "Cost",
  NbW = "All",
  k_n = 3,
  dist = "08"
)

# Figure 17, page 25.
# NYC immigrant household percentage spatial autocorrelation 1991 - 2017.
# 7x14
plot_tests(
  Var = "Imm",
  NbW = "All",
  k_n = 3,
  dist = "08"
)

