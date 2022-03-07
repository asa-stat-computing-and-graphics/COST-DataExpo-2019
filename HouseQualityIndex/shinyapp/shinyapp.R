library(shinydashboard)
library(leaflet)
library(tigris)
library(dplyr)
library(sp)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(png)
library(grid)
## Only run this example in interactive R sessions
if (interactive()) 
  library(shiny)

load("R2.RData")

#states <- geojsonio::geojson_read("Community_Districts.geojson",what = "sp")
#ans <- read.csv("state_data.csv")
states$id = 1:71
states$Name = as.character(ans$Name)
states$Random = 100*runif(71)
my_new_choices <- split(1:71, states$Name)

bins <- c(0,10,20,50,Inf)
pal <- colorBin("YlOrRd", domain= states$Random, bins=bins)
labels <- sprintf("<strong>%s</strong><br/>%g " , states$Name , states$Random) %>% lapply(htmltools::HTML)
titles <- tags$a(href = 'https://www.google.com',tags$img(src = "GW.jpg",height='50',width = '50'),"An Analysis of Immigrants and House Condition in New York
City")

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = titles, titleWidth = 1500),
  dashboardSidebar(
    sidebarMenu(id = "sidebarmenu",
                menuItem("Index Maps", tabName = "a", icon = icon("globe")),
                menuItem("Model Description", tabName = "b", icon = icon("cubes", lib = "font-awesome")),
                menuItem("Find your home", tabName = "c", icon = icon("search", lib = "font-awesome")),
                conditionalPanel("input.sidebarmenu == 'a'",
                                 sliderInput("slider1", h3("Calendar Year"),min = 2000, max = 2010, value = 2009),
                                 selectInput("select", h3("sub-bourough"),choices =my_new_choices, selected = 71)
                                 
                )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem(
        tabName = "a",
        fluidRow(
          box(width = 9,
              title = "NYC Map",status = "primary", solidHeader = TRUE,
              collapsible = FALSE,
              leafletOutput("Index_map",width="100%",height = 800)
          ),
          
          box(width = 3,
              title = "Highlights", solidHeader = TRUE, status = "warning",
              "Now you can right your highlights in this box"
          )
        )
      ))))


server <- function(input, output) {
  output$Index_map <- renderLeaflet({
    leaflet(states) %>% 
      setView(lng = -74, lat = 40.71, zoom = 11) %>% 
      addTiles() %>%  
      addPolygons(data = states,fillColor = ~pal(Random), group = "chosen_year", weight=2 , opacity= 1, color = "white", dashArray = "3", fillOpacity = "0.2",highlight = highlightOptions(weight =5, color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),label=labels,labelOptions = labelOptions(style = list("font-weight"= "normal", padding="3px 8px"), textsize = "15px",direction = "auto")) %>% 
      addPolygons(data = subset(states,states$Name == "Mid-Island0"),group = "chosen_shape",fillColor = ~pal(Random), weight=3 , opacity= 1, color = "white", dashArray = "3", fillOpacity = "0.2",label=labels,labelOptions = labelOptions(style = list("font-weight"= "normal", padding="3px 8px"), textsize = "15px",direction = "auto")) %>% 
      addProviderTiles("CartoDB.Positron")})
  
  
  observe({
    leafletProxy("Index_map") %>% clearGroup("chosen_shape")%>%addPolygons(data = subset(states,states$id == input$select),group = "lalala",fillColor = ~pal(Random), weight=3 , opacity= 1, color = "white", dashArray = "3", fillOpacity = "0.2",label=labels,labelOptions = labelOptions(style = list("font-weight"= "normal", padding="3px 8px"), textsize = "15px",direction = "auto"))
    output$value <- renderPrint({states$Random[as.integer(input$select)]})
  })
  
}

shinyApp(ui, server)

