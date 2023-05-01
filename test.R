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

shp <- st_read("departements-20180101.shp")
pop <- read_csv("pop.csv")
shp <- shp[-c(66,22,80,1,94,65),]
shp$code_insee[shp$code_insee == '69D'] <- '69'
result_map <- left_join(shp, pop, by ='code_insee')
attr(shp, "sf_column")
InputChoices <- colnames(result_map)
InputChoices <- InputChoices[-c(1,2,3,4,5,6,25)]

## UI ##########
ui <- fluidPage(
  selectInput("class", label = "Select a class", choices = InputChoices),
  leafletOutput("map")
)

## SERVER ##########
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet()  %>% 
      addTiles() %>% 
      setView(lng = 2.6, lat = 46.830475, zoom = 6)
  })
  
  observe({
    req(input$class)
    pal = colorFactor(input$class, domain = factor(result_map$nom.x))
    leafletProxy("map") %>%
      addPolygons(data = result_map, color = ~pal(factor(result_map$nom.x)))
  })
}

shinyApp(ui, server)

