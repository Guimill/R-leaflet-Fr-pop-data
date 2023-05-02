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

factpal <- colorFactor(magma(10), unique(result_map$total))

mylabels <- paste(
  "Département: ", result_map$nom.x, "<br/>",
  "Population: ", result_map$total
) %>%
  lapply(htmltools::HTML)

leaflet(result_map) %>%
  addPolygons(
    fillColor = ~factpal(result_map$total),
    stroke = TRUE, 
    color = 'White', 
    weight = 1.5,
    label = mylabels,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  )



