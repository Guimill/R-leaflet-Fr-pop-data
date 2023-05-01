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

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
  selectInput("ageClass", "Classe d'âge", choices = InputChoices))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  filteredData <- reactive({
    result_map %>% select(nom.x, geometry, input$ageClass)
  })
  
  colorpal <- reactive({
    colorNumeric(input$ageClass, result_map$`0-19`)
  })

  InputLabels <- reactive({sprintf(
    "<strong>%s</strong><br/>%s population",
    result_map$nom.x, input$ageClass
  ) %>% lapply(htmltools::HTML)})
  
  output$map <- renderLeaflet({
    leaflet(result_map) %>% addTiles() %>%
      addProviderTiles("Stamen.Watercolor") %>%
      setView(lng = 2.6, lat = 46.830475, zoom = 6)
  })
  
  observe({
    pal <- colorpal()
    labels <- InputLabels()
    
    leafletProxy("map", data = filteredData() %>%
      clearShapes() %>%
        addLegend(pal = pal,
                  values = ~sexRationFH,
                  opacity = 0.7,
                  title = NULL,
                  position = "bottomright") %>%
        addPolygons(fillColor = ~pal(),
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
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))
    )
  })
  
    }

# Run the application 
shinyApp(ui = ui, server = server)
