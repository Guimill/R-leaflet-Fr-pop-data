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

class(shp)
attr(shp, "sf_column")
class(result_map)



# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  sexRationFH <- (result_map$TotalF/result_map$Total)*100
  
  bins <- c(48, 49, 50, 51, 52, 53, Inf)

  pal <- colorBin("YlOrRd", domain = sexRationFH, bins = bins)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g Pourcentage de Femme",
    result_map$nom.x, sexRationFH
  ) %>% lapply(htmltools::HTML)
  
  output$map = renderLeaflet({
          leaflet(result_map) %>%
          addLegend(pal = pal, values = ~sexRationFH, opacity = 0.7, title = NULL,
                    position = "bottomright") %>%
          addTiles() %>%
          setView(lng = 2.6, lat = 46.830475, zoom = 6) %>%
          clearShapes() %>%
          addProviderTiles("Stamen.Watercolor") %>%
          addPolygons(fillColor = ~pal((TotalF/Total)*100),
                  color = "white", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
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
        })
    }

# Run the application 
shinyApp(ui = ui, server = server)
