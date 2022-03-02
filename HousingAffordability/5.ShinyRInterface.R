# Main Author: Jhonatan Medri
# Article name: Housing Variables and Immigration:
#   An Exploratory Analysis in New York City
# This code was reviewed for the last time on 02/15/2022
# This code builds the functions used in the Shiny R interface
# Inspired by https://shiny.rstudio.com/gallery/covid19-tracker.html and
# https://statsandr.com/blog/a-shiny-app-for-inferential-statistics-by-hand/

# Load R Packages
library(dplyr)
library(micromap)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(styler)

# Set Work Directory and Source File
# setwd("C:/Users/jhona/OneDrive/Documents/Classes/USU/Thesis/R Scripts")
source("4.ShinyRSource.R")

# Create Auxiliary Variables for Later
years <- c(
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
)

variables <- c(
  "Home Ownership (%)",
  "Monthly Gross Rent (US$)",
  "Housing Cost (%)",
  "Housing Value (US$)",
  "Immigration Proportion (%)",
  "Yearly Gross Income (US$)",
  "Householder Female Proportion (%)",
  "Householder Age (Years)"
)

chvalues <- c(
  "Own", "Rent",
  "Cost", "Value",
  "Imm", "Income",
  "Sex", "Age"
)

mmvalues <- c(
  "Home Ownership (%)",
  "Monthly Gross Rent (US$)",
  "Housing Cost (%)",
  "Housing Value (US$)",
  "Immigration Proportion (%)",
  "Monthly Gross Income (US$)",
  "Householder Female Proportion (%)",
  "Householder Age (Years)"
)

scvalues <- c(
  "Householder Age (in years)",
  "Household Monthly Rent (Current $)",
  "Log Household Monthly Rent (Current $)",
  "House Value (Current $)",
  "Mortgage Interest Rate (%)",
  "Household Monthly Income (Current $)",
  "Log Household Monthly Income (Current $)",
  "Housing Cost (%)"
)

sc2values <- c(
  "Householder Sex",
  "Immigration Status",
  "Home Own/Rent Status",
  "Home Ownership Status",
  "Borough",
  "None"
)

#--------- Shiny R: Interface  ---------------#

# Create User Interface

ui <- bootstrapPage(
  # Definte Theme

  navbarPage(
    theme = shinytheme("flatly"),
    "NYC Housing Affordability",
    id = "nav",

    # Define Choropleth Maps Interface

    tabPanel(
      "Choropleth Maps",
      sidebarLayout(
        sidebarPanel(
          pickerInput("chvar", "Variable:",
            choices = variables
          ),
          sliderTextInput(
            inputId = "chy",
            label = "Year:",
            choices = years,
            selected = 2017,
            grid = TRUE
          ),
          radioButtons(
            "chcol",
            "Color:",
            choices = c(
              "Blue", "Green", "Grey",
              "Orange", "Purple", "Red"
            ),
            selected = "Green",
            inline = TRUE
          ),
          sliderInput(
            "chbr",
            "Number of breaks:",
            min = 2,
            max = 8,
            value = 5,
            step = 1
          ),
          radioButtons(
            "chbreak",
            "Breaks over time:",
            choices = c("Different", "Same"),
            selected = "Different",
            inline = TRUE
          ),
          actionButton("chgo", "Click here to Plot",
            style = "background-color: #2d3e50"
          )
        ),
        mainPanel(fluidRow(
          plotOutput(
            outputId = "choropleth",
            height = 600
          )
        ))
      )
    ),

    # Define Linked Micromap Plots Interface

    tabPanel(
      "Micromap Plots",
      sidebarLayout(
        sidebarPanel(
          sliderTextInput(
            inputId = "mmy",
            label = "Year:",
            choices = years,
            selected = 2017,
            grid = TRUE
          ),
          radioButtons(
            "mmn",
            "Number of Panels:",
            choices = c(2, 3),
            selected = 2,
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.mmn == '2' || input.mmn == '3'",
            pickerInput(
              "mmvar1",
              "Variable 1:",
              choices = mmvalues,
              selected = "Home Ownership (%)"
            )
          ),
          conditionalPanel(
            condition = "input.mmn == '2' || input.mmn == '3'",
            pickerInput(
              "mmvar2",
              "Variable 2:",
              choices = mmvalues,
              selected = "Monthly Gross Rent (US$)"
            )
          ),
          conditionalPanel(
            condition = "input.mmn == '3'",
            pickerInput(
              "mmvar3",
              "Variable 3:",
              choices = mmvalues,
              selected = "Housing Cost (%)"
            )
          ),
          actionButton("mmgo", "Click here to Plot",
            style = "background-color: #2d3e50"
          )
        ),
        mainPanel(fluidRow(
          plotOutput(
            outputId = "micromap",
            height = 800
          )
        ))
      )
    ),

    # Define Smoothed Scatterplots Interface

    tabPanel(
      "Smoothed Scatterplots",
      sidebarLayout(
        sidebarPanel(
          sliderTextInput(
            inputId = "scaty",
            label = "Year:",
            choices = years,
            selected = 2017,
            grid = TRUE
          ),
          pickerInput(
            "scatvarx",
            "X-Variable:",
            choices = scvalues,
            selected =
              "Log Household Monthly Income (Current $)"
          ),
          pickerInput(
            "scatvary",
            "Y-Variable:",
            choices = scvalues,
            selected =
              "Log Household Monthly Rent (Current $)"
          ),
          pickerInput(
            "scatcol",
            "Discrete Variable (Color and Smoothness): ",
            choices = sc2values,
            selected = "Borough"
          ),
          pickerInput(
            "scatshp",
            "Discrete Variable (Shape): ",
            choices = sc2values,
            selected = "Immigration Status"
          ),
          radioButtons(
            "scataxis",
            "Axis Calculation: ",
            choices = c("Extended", "Middle 95%"),
            selected = "Extended",
            inline = TRUE
          ),
          radioButtons(
            "scatsmo",
            "Smooth Method: ",
            choices = c("lm", "glm", "gam", "loess"),
            selected = "loess",
            inline = TRUE
          ),
          actionButton("scatgo", "Click here to Plot",
            style = "background-color: #2d3e50"
          )
        ),
        mainPanel(fluidRow(
          plotOutput(
            outputId = "scatterplot",
            height = 600
          )
        ))
      )
    ),

    # Define Spatial Dependence Tests Interface

    tabPanel(
      "Spatial Dependence Tests",
      sidebarLayout(
        sidebarPanel(
          pickerInput("sptvar", "Variable:",
            choices = variables
          ),
          sliderTextInput(
            inputId = "spty",
            label = "Year:",
            choices = years,
            selected = 2017,
            grid = TRUE
          ),
          radioButtons(
            "spttype",
            "Test Type:",
            choices = c(
              "Moran's I (Global Test)",
              "Geary's C (Global Test)"
            ),
            selected = "Moran's I (Global Test)",
            inline = TRUE
          ),
          radioButtons(
            "sptalt",
            "Alternative:",
            choices = c(
              "Greater", "Less",
              "Two-Sided"
            ),
            selected = "Greater",
            inline = TRUE
          ),
          radioButtons(
            "sptw",
            "Weight Type:",
            choices = c(
              "Queen", "K-Nearest Neighbor",
              "Maximum Distance"
            ),
            selected = "Queen",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.sptw == 'K-Nearest Neighbor'",
            numericInput(
              "sptkneigh",
              "Number of K-nearest neighbors:",
              value = 3,
              min = 1,
              max = 49
            )
          ),
          conditionalPanel(
            condition = "input.sptw == 'K Neighbor'",
            p(
              strong("Note: "),
              "Input should be an integer between 1 and 54. "
            )
          ),
          conditionalPanel(
            condition = "input.sptw == 'Maximum Distance'",
            numericInput(
              "sptmdist",
              "Maximum Distance (in km):",
              value = 8,
              min = 0,
              max = 50
            ),
            p(
              strong("Note: "),
              "Input minimun distance should be 7.89km"
            )
          ),
          actionButton("sptgo", "Click here to Run Test",
            style = "background-color: #2d3e50"
          )
        ),
        mainPanel(fluidRow(
          p(
            h3(
              strong("Data Information for "),
              textOutput(outputId = "dfname")
            ),
            br(),
            h4(strong("Number of Observations: "), "55"),
            h4(
              strong("Sample Average: "),
              textOutput(outputId = "dfave")
            ),
            h4(
              strong("Standard Deviation: "),
              textOutput(outputId = "dfsd")
            ),
            br(),
            h3(strong(
              "Hypothesis Test for ",
              "Spatial Autocorrelation"
            )),
            h4(
              strong("Null Hypothesis: "),
              textOutput(outputId = "sptnull")
            ),
            h4(
              strong("Alternative Hypothesis: "),
              textOutput(outputId = "sptalt")
            ),
            h4(
              strong("Test Statistic: "),
              textOutput(outputId = "spttstat")
            ),
            h4(
              strong("P-value: "),
              textOutput(outputId = "sptpvalue")
            ),
            h4(
              strong("Conclusion: "),
              textOutput(outputId = "sptconclusion")
            ),
            br(),
            h3(strong("Interpretation")),
            textOutput(outputId = "sptint")
          )
        ))
      )
    )
  )
)

#---------------------SERVER PART------------------------------#

server <- function(input, output) {
  # Choropleth Output
  output$choropleth <- renderPlot({
    if (input$chgo == 0) {
      return("")
    }

    isolate(
      choropleths(
        dif_break = switch(input$chbreak,
          "Same" = "N",
          "Different" = "Y"
        ),
        variable = switch(input$chvar,
          "Home Ownership (%)" = "Own",
          "Monthly Gross Rent (US$)" = "Rent",
          "Housing Cost (%)" = "Cost",
          "Housing Value (US$)" = "Value",
          "Immigration Proportion (%)" = "Imm",
          "Yearly Gross Income (US$)" = "Income",
          "Householder Female Proportion (%)" = "Sex",
          "Householder Age (Years)" = "Age"
        ),
        color = switch(input$chcol,
          "Blue" = "Blues",
          "Green" = "Greens",
          "Orange" = "Oranges",
          "Purple" = "Purples",
          "Red" = "Reds",
          "Grey" = "Greys"
        ),
        breaks = input$chbr,
        year = input$chy
      )
    )
  })

  # Micromap Output

  output$micromap <- renderPlot({
    if (input$mmgo == 0) {
      return("")
    }

    isolate(
      micromaps(
        n = as.numeric(input$mmn),
        Year = input$mmy,
        Var1 = switch(input$mmvar1,
          "Home Ownership (%)" = "Own",
          "Monthly Gross Rent (US$)" = "Rent",
          "Housing Cost (%)" = "Cost",
          "Housing Value (US$)" = "Value",
          "Immigration Proportion (%)" = "Imm",
          "Monthly Gross Income (US$)" = "Income",
          "Householder Female Proportion (%)" = "Sex",
          "Householder Age (Years)" = "Age"
        ),
        Var2 = switch(input$mmvar2,
          "Home Ownership (%)" = "Own",
          "Monthly Gross Rent (US$)" = "Rent",
          "Housing Cost (%)" = "Cost",
          "Housing Value (US$)" = "Value",
          "Immigration Proportion (%)" = "Imm",
          "Monthly Gross Income (US$)" = "Income",
          "Householder Female Proportion (%)" = "Sex",
          "Householder Age (Years)" = "Age"
        ),
        Var3 = switch(input$mmvar3,
          "Home Ownership (%)" = "Own",
          "Monthly Gross Rent (US$)" = "Rent",
          "Housing Cost (%)" = "Cost",
          "Housing Value (US$)" = "Value",
          "Immigration Proportion (%)" = "Imm",
          "Monthly Gross Income (US$)" = "Income",
          "Householder Female Proportion (%)" = "Sex",
          "Householder Age (Years)" = "Age"
        )
      )
    )
  })



  # Statistical Dependence Output

  #  DataInfo

  output$dfname <- renderPrint({
    if (input$sptgo == 0) {
      return("")
    }

    isolate(
      statdeptests(
        Var = switch(input$sptvar,
          "Home Ownership (%)" = "Own",
          "Monthly Gross Rent (US$)" = "Rent",
          "Housing Cost (%)" = "Cost",
          "Housing Value (US$)" = "Value",
          "Immigration Proportion (%)" = "Imm",
          "Yearly Gross Income (US$)" = "Income",
          "Householder Female Proportion (%)" = "Sex",
          "Householder Age (Years)" = "Age"
        ),
        Year = input$spty,
        Test = switch(input$spttype,
          "Moran's I (Global Test)" = "Moran",
          "Geary's C (Global Test)" = "Geary"
        ),
        Alt = switch(input$sptalt,
          "Greater" = "greater",
          "Less" = "less",
          "Two-Sided" = "two.sided"
        ),
        NbW = switch(input$sptw,
          "Queen" = "Queens",
          "K-Nearest Neighbor" = "KN",
          "Maximum Distance" = "Dist"
        ),
        kn = input$sptkneigh,
        dist = input$sptmdist,
        mode = "name"
      )
    )
  })

  output$dfave <- renderPrint({
    if (input$sptgo == 0) {
      return("")
    }

    isolate(
      statdeptests(
        Var = switch(input$sptvar,
          "Home Ownership (%)" = "Own",
          "Monthly Gross Rent (US$)" = "Rent",
          "Housing Cost (%)" = "Cost",
          "Housing Value (US$)" = "Value",
          "Immigration Proportion (%)" = "Imm",
          "Yearly Gross Income (US$)" = "Income",
          "Householder Female Proportion (%)" = "Sex",
          "Householder Age (Years)" = "Age"
        ),
        Year = input$spty,
        Test = switch(input$spttype,
          "Moran's I (Global Test)" = "Moran",
          "Geary's C (Global Test)" = "Geary"
        ),
        Alt = switch(input$sptalt,
          "Greater" = "greater",
          "Less" = "less",
          "Two-Sided" = "two.sided"
        ),
        NbW = switch(input$sptw,
          "Queen" = "Queens",
          "K-Nearest Neighbor" = "KN",
          "Maximum Distance" = "Dist"
        ),
        kn = input$sptkneigh,
        dist = input$sptmdist,
        mode = "ave"
      )
    )
  })

  output$dfsd <- renderPrint({
    if (input$sptgo == 0) {
      return("")
    }

    isolate(
      statdeptests(
        Var = switch(input$sptvar,
          "Home Ownership (%)" = "Own",
          "Monthly Gross Rent (US$)" = "Rent",
          "Housing Cost (%)" = "Cost",
          "Housing Value (US$)" = "Value",
          "Immigration Proportion (%)" = "Imm",
          "Yearly Gross Income (US$)" = "Income",
          "Householder Female Proportion (%)" = "Sex",
          "Householder Age (Years)" = "Age"
        ),
        Year = input$spty,
        Test = switch(input$spttype,
          "Moran's I (Global Test)" = "Moran",
          "Geary's C (Global Test)" = "Geary"
        ),
        Alt = switch(input$sptalt,
          "Greater" = "greater",
          "Less" = "less",
          "Two-Sided" = "two.sided"
        ),
        NbW = switch(input$sptw,
          "Queen" = "Queens",
          "K-Nearest Neighbor" = "KN",
          "Maximum Distance" = "Dist"
        ),
        kn = input$sptkneigh,
        dist = input$sptmdist,
        mode = "sd"
      )
    )
  })

  #  Null and Alternative Hypothesis

  output$sptnull <- renderPrint({
    if (input$sptgo == 0) {
      return("")
    }

    isolate(
      statdeptests(
        Var = switch(input$sptvar,
          "Home Ownership (%)" = "Own",
          "Monthly Gross Rent (US$)" = "Rent",
          "Housing Cost (%)" = "Cost",
          "Housing Value (US$)" = "Value",
          "Immigration Proportion (%)" = "Imm",
          "Yearly Gross Income (US$)" = "Income",
          "Householder Female Proportion (%)" = "Sex",
          "Householder Age (Years)" = "Age"
        ),
        Year = input$spty,
        Test = switch(input$spttype,
          "Moran's I (Global Test)" = "Moran",
          "Geary's C (Global Test)" = "Geary"
        ),
        Alt = switch(input$sptalt,
          "Greater" = "greater",
          "Less" = "less",
          "Two-Sided" = "two.sided"
        ),
        NbW = switch(input$sptw,
          "Queen" = "Queens",
          "K-Nearest Neighbor" = "KN",
          "Maximum Distance" = "Dist"
        ),
        kn = input$sptkneigh,
        dist = input$sptmdist,
        mode = "null"
      )
    )
  })

  output$sptalt <- renderPrint({
    if (input$sptgo == 0) {
      return("")
    }

    isolate(
      statdeptests(
        Var = switch(input$sptvar,
          "Home Ownership (%)" = "Own",
          "Monthly Gross Rent (US$)" = "Rent",
          "Housing Cost (%)" = "Cost",
          "Housing Value (US$)" = "Value",
          "Immigration Proportion (%)" = "Imm",
          "Yearly Gross Income (US$)" = "Income",
          "Householder Female Proportion (%)" = "Sex",
          "Householder Age (Years)" = "Age"
        ),
        Year = input$spty,
        Test = switch(input$spttype,
          "Moran's I (Global Test)" = "Moran",
          "Geary's C (Global Test)" = "Geary"
        ),
        Alt = switch(input$sptalt,
          "Greater" = "greater",
          "Less" = "less",
          "Two-Sided" = "two.sided"
        ),
        NbW = switch(input$sptw,
          "Queen" = "Queens",
          "K-Nearest Neighbor" = "KN",
          "Maximum Distance" = "Dist"
        ),
        kn = input$sptkneigh,
        dist = input$sptmdist,
        mode = "alt"
      )
    )
  })

  #  Hypothesis test info

  output$spttstat <- renderPrint({
    if (input$sptgo == 0) {
      return("")
    }

    isolate(
      statdeptests(
        Var = switch(input$sptvar,
          "Home Ownership (%)" = "Own",
          "Monthly Gross Rent (US$)" = "Rent",
          "Housing Cost (%)" = "Cost",
          "Housing Value (US$)" = "Value",
          "Immigration Proportion (%)" = "Imm",
          "Yearly Gross Income (US$)" = "Income",
          "Householder Female Proportion (%)" = "Sex",
          "Householder Age (Years)" = "Age"
        ),
        Year = input$spty,
        Test = switch(input$spttype,
          "Moran's I (Global Test)" = "Moran",
          "Geary's C (Global Test)" = "Geary"
        ),
        Alt = switch(input$sptalt,
          "Greater" = "greater",
          "Less" = "less",
          "Two-Sided" = "two.sided"
        ),
        NbW = switch(input$sptw,
          "Queen" = "Queens",
          "K-Nearest Neighbor" = "KN",
          "Maximum Distance" = "Dist"
        ),
        kn = input$sptkneigh,
        dist = input$sptmdist,
        mode = "tstat"
      )
    )
  })

  output$sptpvalue <- renderPrint({
    if (input$sptgo == 0) {
      return("")
    }

    isolate(
      statdeptests(
        Var = switch(input$sptvar,
          "Home Ownership (%)" = "Own",
          "Monthly Gross Rent (US$)" = "Rent",
          "Housing Cost (%)" = "Cost",
          "Housing Value (US$)" = "Value",
          "Immigration Proportion (%)" = "Imm",
          "Yearly Gross Income (US$)" = "Income",
          "Householder Female Proportion (%)" = "Sex",
          "Householder Age (Years)" = "Age"
        ),
        Year = input$spty,
        Test = switch(input$spttype,
          "Moran's I (Global Test)" = "Moran",
          "Geary's C (Global Test)" = "Geary"
        ),
        Alt = switch(input$sptalt,
          "Greater" = "greater",
          "Less" = "less",
          "Two-Sided" = "two.sided"
        ),
        NbW = switch(input$sptw,
          "Queen" = "Queens",
          "K-Nearest Neighbor" = "KN",
          "Maximum Distance" = "Dist"
        ),
        kn = input$sptkneigh,
        dist = input$sptmdist,
        mode = "pvalue"
      )
    )
  })

  output$sptconclusion <- renderPrint({
    if (input$sptgo == 0) {
      return("")
    }

    isolate(
      statdeptests(
        Var = switch(input$sptvar,
          "Home Ownership (%)" = "Own",
          "Monthly Gross Rent (US$)" = "Rent",
          "Housing Cost (%)" = "Cost",
          "Housing Value (US$)" = "Value",
          "Immigration Proportion (%)" = "Imm",
          "Yearly Gross Income (US$)" = "Income",
          "Householder Female Proportion (%)" = "Sex",
          "Householder Age (Years)" = "Age"
        ),
        Year = input$spty,
        Test = switch(input$spttype,
          "Moran's I (Global Test)" = "Moran",
          "Geary's C (Global Test)" = "Geary"
        ),
        Alt = switch(input$sptalt,
          "Greater" = "greater",
          "Less" = "less",
          "Two-Sided" = "two.sided"
        ),
        NbW = switch(input$sptw,
          "Queen" = "Queens",
          "K-Nearest Neighbor" = "KN",
          "Maximum Distance" = "Dist"
        ),
        kn = input$sptkneigh,
        dist = input$sptmdist,
        mode = "conclusion"
      )
    )
  })

  #  Interpretation

  output$sptint <- renderPrint({
    if (input$sptgo == 0) {
      return("")
    }

    isolate(
      statdeptests(
        Var = switch(input$sptvar,
          "Home Ownership (%)" = "Own",
          "Monthly Gross Rent (US$)" = "Rent",
          "Housing Cost (%)" = "Cost",
          "Housing Value (US$)" = "Value",
          "Immigration Proportion (%)" = "Imm",
          "Yearly Gross Income (US$)" = "Income",
          "Householder Female Proportion (%)" = "Sex",
          "Householder Age (Years)" = "Age"
        ),
        Year = input$spty,
        Test = switch(input$spttype,
          "Moran's I (Global Test)" = "Moran",
          "Geary's C (Global Test)" = "Geary"
        ),
        Alt = switch(input$sptalt,
          "Greater" = "greater",
          "Less" = "less",
          "Two-Sided" = "two.sided"
        ),
        NbW = switch(input$sptw,
          "Queen" = "Queens",
          "K-Nearest Neighbor" = "KN",
          "Maximum Distance" = "Dist"
        ),
        kn = input$sptkneigh,
        dist = input$sptmdist,
        mode = "interpretation"
      )
    )
  })




  # Smoothed Scatterplot Output
  output$scatterplot <- renderPlot({
    if (input$scatgo == 0) {
      return("")
    }

    isolate(
      scatterplots(
        Year = input$scaty,
        x = switch(input$scatvarx,
          "Householder Age (in years)" = "age",
          "Household Monthly Rent (Current $)" = "rent",
          "Log Household Monthly Rent (Current $)" = "lrent",
          "House Value (Current $)" = "hvalue",
          "Mortgage Interest Rate (%)" = "mortint",
          "Household Monthly Income (Current $)" = "inc",
          "Log Household Monthly Income (Current $)" = "linc",
          "Housing Cost (%)" = "cost"
        ),
        y = switch(input$scatvary,
          "Householder Age (in years)" = "age",
          "Household Monthly Rent (Current $)" = "rent",
          "Log Household Monthly Rent (Current $)" = "lrent",
          "House Value (Current $)" = "hvalue",
          "Mortgage Interest Rate (%)" = "mortint",
          "Household Monthly Income (Current $)" = "inc",
          "Log Household Monthly Income (Current $)" = "linc",
          "Housing Cost (%)" = "cost"
        ),
        axismeth = switch(input$scataxis,
          "Extended" = "extended",
          "Middle 95%" = "95"
        ),
        col = switch(input$scatcol,
          "Householder Sex" = "sex",
          "Immigration Status" = "imm",
          "Home Own/Rent Status" = "ownrent",
          "Home Ownership Status" = "mort",
          "Borough" = "borough"
        ),
        shp = switch(input$scatshp,
          "Householder Sex" = "sex",
          "Immigration Status" = "imm",
          "Home Own/Rent Status" = "ownrent",
          "Home Ownership Status" = "mort",
          "Borough" = "borough",
          "None" = "None"
        ),
        smooth = input$scatsmo
      )
    )
  })
}

shinyApp(ui, server)
