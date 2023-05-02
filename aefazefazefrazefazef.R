output$map <- renderLeaflet({
  leaflet(result_map) %>% addTiles() %>%
    addProviderTiles("Stamen.Watercolor") %>%
    setView(lng = 2.6, lat = 46.830475, zoom = 6)
})

observe({
  pal <- colorpal()
  finalFilteredData <- filteredData()
  labels <- InputLabels()
  
  leafletProxy("map", data = result_map) %>%
    clearShapes() %>%
    addPolygons(data = finalFilteredData,
                fillColor = ~pal,
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
                label = labels)
})


DT::dataTableOutput("bleu"),
output$bleu <- DT::renderDataTable({
  res <- st_drop_geometry(result_map)
  resInput <- result_map[,c("nom.x", input$ageClass), drop = FALSE]
  aData <- res %>% select(input$ageClass)
  aaa <- as.data.frame(aData)
  bbb<- data.frame(input$ageClass)
  a <- input$ageClass
  res[,a]
  result_map[,a]
  
})



