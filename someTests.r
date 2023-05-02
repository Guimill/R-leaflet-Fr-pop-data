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

data <- map_data("france")
data <- data[order(data$region),]
region <- unique(data$region)
pop <- read_csv("pop.csv")
pop$region <- stri_trans_general(pop$region, "Latin-ASCII") %>%
  str_replace_all("Cote-d'Or", "Cote-Dor") %>%
  str_replace_all("Cotes-d'Armor", "Cotes-Darmor") %>%
  str_replace_all("Corse-du-Sud", "Corse du Sud") %>%
  str_replace_all("Val-d'Oise", "Val-Doise")

view(pop)
result_map <- left_join(x = data[,-6], y = pop)

mean((pop$TotalF/pop$total)*100)
mean((pop$x75F/pop$x75)*100)
mean((pop$`x60-74F`/pop$`x60-74`))
mean((pop$`x20-39F`/pop$`x20-39`))


ggplot(result_map, aes(long,lat, group = group, fill = ((`x60-74F`/`x60-74`)*100))) +
  scale_colour_gradientn(colours=rainbow(4)) +
  geom_polygon(colour = "black") +
  coord_map() +
  theme_dark() +
  scale_fill_gradient(name = "sex ratio F/H", low = "yellow", high = "red", na.value = NA) +
  labs(x = "", 
       y = "", 
       title = "Sex ration par département en 2023", 
       subtitle = "Données via insee.fr")




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

data <- map_data("france")
data <- data[order(data$region),]
region <- unique(data$region)
pop <- read_csv("pop.csv")
pop$region <- stri_trans_general(pop$region, "Latin-ASCII") %>%
  str_replace_all("Cote-d'Or", "Cote-Dor") %>%
  str_replace_all("Cotes-d'Armor", "Cotes-Darmor") %>%
  str_replace_all("Corse-du-Sud", "Corse du Sud") %>%
  str_replace_all("Val-d'Oise", "Val-Doise")

result_map <- left_join(x = data[,-6], y = pop)

prj4string <- "+proj=longlat +datum=WGS84"

my.projection <- st_crs(prj4string)

a <- st_as_sf(result_map,coords = c("long", "lat"), crs = my.projection)
st_crs(a)
a <- st_transform(a, my.projection)
st_crs(a)

class(a$geometry)



# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "POP", min(a$total), max(a$total),
                            value = range(a$total), step = 100000
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  filteredData <- reactive({
    a[a$total >= input$range[1] & a$total <= input$range[2],]
  })
  output$map = renderLeaflet({
    leaflet(a) %>%
      addTiles() %>%
      setView(lng = 2.6, lat = 46.830475, zoom = 5)
  })
  
  colorpal <- reactive({
    colorNumeric(input$colors, a$total)
  })
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addPolygons(data = filteredData()$geometry,
                  weight = 1, color = "#777777",
                  fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
                  fillOpacity = 0.7, popup = ~paste(total),
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
