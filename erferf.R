library(shiny)
library(maps)
library(sf)
library(sp)
library(leaflet)
library(tidyverse)
library(stringr)
library(stringi)
library(ggplot2)
library(RColorBrewer)
library(rgdal)
library(magrittr)
library(devtools)
library(rgeos)
library(maptools)
library(viridis)

setwd("C:/Users/guill/Documents/GitHub/R-leaflet-Fr-pop-data")

shp <- st_read("departements-20180101.shp")
pop <- read_csv("pop.csv")
shp <- shp[-c(66,22,80,1,94,65),]
shp$code_insee[shp$code_insee == '69D'] <- '69'
result_map <- left_join(shp, pop, by ='code_insee')
attr(shp, "sf_column")
InputChoices <- colnames(result_map)
InputChoices <- InputChoices[-c(1,2,3,4,5,6,25)]

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("ageClass", "Classe d'âge", InputChoices))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(result_map) %>% addTiles() %>%
      addProviderTiles("Stamen.Watercolor") %>%
      setView(lng = 2.6, lat = 46.830475, zoom = 6)
  })
  
  observe({
    colorBy <- input$ageClass
    colorData <- result_map[[colorBy]]
    pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = result_map,
                  fillColor = pal(colorData),
                  color = "white",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1.0,
                  fillOpacity = 0.5,
                  highlightOptions = highlightOptions(
                    color = "white",
                    weight = 2,
                    bringToFront = TRUE
                  ),
                  addLegend("bottomleft", pal=pal, values=colorData, title=colorBy))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
