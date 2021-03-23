library(shiny)
library(shinyWidgets)
library(dplyr)
library(leaflet)

Dataset <- data.frame(
  "Type" = c("A", "B", "A", "B"),
  "Value" = c(1000000, 200, 4000000, 150),
  "Lat" = c(40.7, 41.8, 42.4, 43.1), 
  "Long" = c(-3.2, -2.1, -1.6, -3.1)
)
ui <- bootstrapPage(
  absolutePanel(
    top = 10,
    right = 10,
    sliderInput(
      "range",
      "Value",
      min(Dataset$Value, na.rm = TRUE),
      max(Dataset$Value, na.rm = TRUE),
      value = range(Dataset$Value, na.rm = FALSE),
      step = 1000
    ),
    pickerInput(
      "Type",
      "Type",
      choices = c("A", "B"),
      selected = c("A", "B"),
      multiple = T,
      options = list(`actions-box` = TRUE)
    ),
  ),
  leafletOutput("map", width = "50%")
)

server <- function(input, output, session) {
  
  filter_type <- reactive({
    Dataset %>%
      filter(Type %in% input$Type)
  })
  
  observeEvent(input$Type, {
    updateSliderInput(
      session = session,
      inputId = "range",
      min = min(filter_type()$Value),
      max = max(filter_type()$Value),
      value = range(filter_type()$Value, na.rm = FALSE)
    )
  })
  
  filter_range <- reactive({
    filter_type() %>% 
      filter(Value >= input$range[1]) %>% 
      filter(Value <= input$range[2])
  })
  
  output$map <- renderLeaflet({
    leaflet(Dataset) %>% 
      addTiles() %>% 
      addMarkers(data = filter_range(), lng = ~Long, lat = ~Lat)
  })
}

shinyApp(ui, server)