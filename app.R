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
library(DT)

setwd("C:/Users/guill/Documents/GitHub/R-leaflet-Fr-pop-data")

shp <- st_read("departements-20180101.shp")
pop <- read_csv("pop.csv")
pop <- pop[-c(9:20)]
shp <- shp[-c(66,22,80,1,94,65),]
shp$code_insee[shp$code_insee == '69D'] <- '69'
result_map <- left_join(shp, pop, by ='code_insee')
attr(shp, "sf_column")
ChoicesData <- result_map[-c(1,2,3,4,5,6,25)]
InputChoices <- colnames(ChoicesData)

a <- result_map %>% select(un)
a


# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$style(HTML(
    "html, body {width:100%;height:100%;z-index:auto}
    #controls{
            background-color: rgba(255,255,255,0.8);
            color:#555555;
            border-radius: 10px;
            padding: 5px 5px 0 5px;
            z-index = 2;
            box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;
            font-size: large;
            
    }"
  )),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id = "controls" ,top = 10, right = 10,
  selectInput("ageClass", "Classe d'âge :", choices = c(
                                                          "Zero_dix-neuf" = "un",
                                                          "vingt_trente-neuf" = "deux",
                                                          "quarante_cinquante-neuf" = "trois",
                                                          "soixante_soixante-quatorze" = "quatre",
                                                          "soixante-quinze&plus" = "cinq",
                                                          "Total" = "total"
                                                          ))
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  filteredData <- reactive({
    result_map[,c("nom.x", "geometry", input$ageClass), drop = FALSE]
  })
  
  colorpal <- reactive({
    res <- st_drop_geometry(result_map)
    a <- input$ageClass
    colorBin(rocket(10) , domain = res[,a])
  })

  InputLabels <- reactive({
    res <- st_drop_geometry(result_map)
    a <- input$ageClass
    sprintf(
    "<strong>%s</strong><br/>%s Personnes",
    result_map$nom.x, res[,a]
    ) %>% lapply(htmltools::HTML)
  })
  
  output$map <- renderLeaflet({
    leaflet(result_map) %>% addTiles() %>%
      setView(lng = 2.6, lat = 46.830475, zoom = 6)
  })
  
  observe({
    pal <- colorpal()
    finalFilteredData <- filteredData()
    labels <- InputLabels()
    res <- st_drop_geometry(result_map)
    a <- input$ageClass
    
    leafletProxy("map", data = result_map) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = finalFilteredData,
                  fillColor = ~pal(res[,a]),
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
                  label = labels) %>%
                  addLegend("bottomright", pal = pal, values = ~res[,a],
                            title = "Population :",
                            opacity = 1
                  )
  })
  

    }

# Run the application 
shinyApp(ui = ui, server = server)

