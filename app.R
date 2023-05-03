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
library(MASS)

pop <- read_csv("data/pop.csv")

shp <- st_read("data/georef-france-departement-millesime.shp")
shp$dep_code <- substr(shp$dep_code,3,4)
shp <- shp[-c(15,19,27,91,70),]
colnames(shp)[4] <- "code_insee"

result_map <- left_join(shp, pop, by ='code_insee')
result_map <- result_map[-c(1:7,9:14)]
colnames(result_map)[1] <- "nom"
attr(shp, "sf_column")


sexrUn <- (result_map$unH/result_map$unF)
sexrDeux <- (result_map$deuxH/result_map$deuxF)
sexrTrois <- (result_map$troisH/result_map$troisF)
sexrQuatre <- (result_map$quatreH/result_map$quatreF)
sexrCinq <- (result_map$cinqH/result_map$cinqF)
sexrTotal <- (result_map$totalH/result_map$totalF)
popActive <- ((result_map$deux + result_map$trois)/result_map$total)*100
popActiveF <- ((result_map$deuxF + result_map$troisF)/result_map$totalF)*100
popActiveH <- ((result_map$deuxH + result_map$troisH)/result_map$totalH)*100
  
result_map <- cbind(result_map,sexrUn, sexrDeux, sexrTrois, sexrQuatre, sexrCinq, sexrTotal, popActive, popActiveF, popActiveH)

inputChoice = c(
  "Population de 0 à 19 ans",
  "Population de 20 à 39 ans",
  "Population de 40 à 59 ans",
  "Population de 60 à 74 ans",
  "Population de 75+",
  "Population total",
  "Estimation en pourcentage de la population active (20-59ans)",
  "Estimation en pourcentage de la population active féminine (20-59ans)",
  "Estimation en pourcentage de la population active masculine (20-59ans)",
  "Sex-ratio de la population ayant entre 0 & 19 ans",
  "Sex-ratio de la population ayant entre 20 & 39 ans",
  "Sex-ratio de la population ayant entre 40 & 59 ans",
  "Sex-ratio de la population ayant entre 60 & 74 ans",
  "Sex-ratio de la population ayant 75+",
  "Sex-ratio de la population total"
)
choice <- colnames(result_map) 
choice <- choice[-c(1,8:19,29)]
choice <- rbind(choice)
colnames(choice) <- as.character(choice[1,])
choice <- rbind(choice, inputChoice)
choice <- data.frame(choice)
choice <- choice[-1,]

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$style(HTML(
    "html, body {width:100%;height:100%;z-index:auto}
    #tittle{white-space : pre-wrap}
    #controls{
            background-color: rgba(255,255,255,0.8);
            color:#555555;
            border-radius: 10px;
            padding: 15px 15px 0 15px;
            z-index = 2;
            box-shadow: rgba(99, 99, 99, 0.2) 0px 2px 8px 0px;
            font-size: large;
    }"
  )),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id = "controls" ,top = 10, right = 10,
                helpText(id = "tittle",
                "    Répartition de la population
    en France métropolitaine,
    sex ratio et population active par département"),
                hr(),
                selectInput("ageClass", "Données :", choices = c(
                                                                        "Population de 0 à 19 ans" = "un",
                                                                        "Population de 20 à 39 ans" = "deux",
                                                                        "Population de 40 à 59 ans" = "trois",
                                                                        "Population de 60 à 74 ans" = "quatre",
                                                                        "Population de 75+" = "cinq",
                                                                        "Population total" = "total",
                                                                        "Estimation en pourcentage de la population active (20-59ans)" = "popActive",
                                                                        "Estimation en pourcentage de la population active féminine (20-59ans)" = "popActiveF",
                                                                        "Estimation en pourcentage de la population active masculine (20-59ans)" = "popActiveH",
                                                                        "Sex-ratio de la population ayant entre 0 & 19 ans" = "sexrUn",
                                                                        "Sex-ratio de la population ayant entre 20 & 39 ans" = "sexrDeux",
                                                                        "Sex-ratio de la population ayant entre 40 & 59 ans" = "sexrTrois",
                                                                        "Sex-ratio de la population ayant entre 60 & 74 ans" = "sexrQuatre",
                                                                        "Sex-ratio de la population ayant 75+" = "sexrCinq",
                                                                        "Sex-ratio de la population total" = "sexrTotal"
                                                                        ))
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  filteredData <- reactive({
    result_map[,c("nom", "geometry", input$ageClass), drop = FALSE]
  })
  
  colorpal <- reactive({
    res <- st_drop_geometry(result_map)
    ageC <- input$ageClass
    colorBin("plasma" , domain = res[,ageC])
  })

  InputLabels <- reactive({
    res <- st_drop_geometry(result_map)
    ageC <- input$ageClass
    sprintf(
    "<strong>%s</strong><br/>%s",
    result_map$nom, res[,ageC]
    ) %>% lapply(htmltools::HTML)
  })
  
  output$map <- renderLeaflet({
    leaflet(result_map) %>%
      addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
      setView(lng = 2.6, lat = 46.830475, zoom = 6)
  })
  
  observe({
    pal <- colorpal()
    finalFilteredData <- filteredData()
    labels <- InputLabels()
    res <- st_drop_geometry(result_map)
    ageC <- input$ageClass
    
    
    leafletProxy("map", data = result_map) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = finalFilteredData,
                  fillColor = ~pal(res[,ageC]),
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
                  addLegend("bottomright", pal = pal, values = ~res[,ageC],
                            title = choice[1,ageC],
                            opacity = 1
                  )
  })
  

    }

# Run the application 
shinyApp(ui = ui, server = server)

