library(shiny)
library(shinyWidgets)
library(dplyr)
library(leaflet)

# Dataset <- data.frame(
#   "Type" = c("A", "B", "A", "B"),
#   "Value" = c(1000000, 200, 4000000, 150),
#   "Lat" = c(40.7, 41.8, 42.4, 43.1), 
#   "Long" = c(-3.2, -2.1, -1.6, -3.1)
# )
# ui <- bootstrapPage(
#   absolutePanel(
#     top = 10,
#     right = 10,
#     sliderInput(
#       "range",
#       "Value",
#       min(Dataset$Value, na.rm = TRUE),
#       max(Dataset$Value, na.rm = TRUE),
#       value = range(Dataset$Value, na.rm = FALSE),
#       step = 1000
#     ),
#     pickerInput(
#       "Type",
#       "Type",
#       choices = c("A", "B"),
#       selected = c("A", "B"),
#       multiple = T,
#       options = list(`actions-box` = TRUE)
#     ),
#   ),
#   leafletOutput("map", width = "50%")
# )
# 
# server <- function(input, output, session) {
#   
#   filter_type <- reactive({
#     Dataset %>%
#       filter(Type %in% input$Type)
#   })
#   
#   observeEvent(input$Type, {
#     updateSliderInput(
#       session = session,
#       inputId = "range",
#       min = min(filter_type()$Value),
#       max = max(filter_type()$Value),
#       value = range(filter_type()$Value, na.rm = FALSE)
#     )
#   })
#   
#   filter_range <- reactive({
#     filter_type() %>% 
#       filter(Value >= input$range[1]) %>% 
#       filter(Value <= input$range[2])
#   })
#   
#   output$map <- renderLeaflet({
#     leaflet(Dataset) %>% 
#       addTiles() %>% 
#       addMarkers(data = filter_range(), lng = ~Long, lat = ~Lat)
#   })
# }
# 
# shinyApp(ui, server)
# 
# 


#NOT RUN {
if (interactive()) {

  ui <- fluidPage(
    tags$h1("radioGroupButtons examples"),

    radioGroupButtons(
      inputId = "somevalue1",
      label = "Make a choice: ",
      choiceNames = c("A", "B", "C"),
      choiceValues = c(1,2,3)
    ),
    verbatimTextOutput("value1"),

    radioGroupButtons(
      inputId = "somevalue2",
      label = "With custom status:",
      choices = names(iris),
      status = "primary"
    ),
    verbatimTextOutput("value2"),

    radioGroupButtons(
      inputId = "somevalue3",
      label = "With icons:",
      choices = names(mtcars),
      checkIcon = list(
        yes = icon("check-square"),
        no = icon("square-o")
      )
    ),
    verbatimTextOutput("value3")
  )
  server <- function(input, output) {

    output$value1 <- renderPrint({ input$somevalue1 })
    output$value2 <- renderPrint({ input$somevalue2 })
    output$value3 <- renderPrint({ input$somevalue3 })

  }
  shinyApp(ui, server)

}
# }





# ui <- shinyUI(fluidPage(
#     mainPanel(
#       tabsetPanel(
#         # using iframe along with tags() within tab to display pdf with scroll, height and width could be adjusted
#         # tabPanel("Reference", 
#         #          tags$iframe(style="height:600px; width:90%; scrolling=yes", 
#         #                      src="input_data/2018PolicyandProcedureManual.pdf")),
#         tabPanel("Metadata", uiOutput("help")),
#         tabPanel("Plot")
#       )
#     ))
#   
# )
# 
# server <- 
#   shinyServer(function(input, output,session){
#     addResourcePath("resources", "input_data/")
#     output$help <- renderUI({
#       tags$iframe(
#         seamless="seamless",
#         src="resources/2018PolicyandProcedureManual.pdf",
#         style="height:600px; width:90%; scrolling:yes")
#     })
#   })




ui <- fluidPage(
  radioButtons("dist", "Distribution type:",
               c("Normal" = "norm",
                 "Uniform" = "unif",
                 "Log-normal" = "lnorm",
                 "Exponential" = "exp")),
  plotOutput("distPlot")
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    hist(dist(500))
  })
}

shinyApp(ui, server)

ui <- fluidPage(
  radioButtons("rb", "Choose one:",
               choiceNames = list(
                 icon("calendar"),
                 HTML("<p style='color:red;'>Red Text</p>"),
                 "Normal text"
               ),
               choiceValues = list(
                 "icon", "html", "text"
               )),
  textOutput("txt")
)

server <- function(input, output) {
  output$txt <- renderText({
    paste("You chose", input$rb)
  })
}

shinyApp(ui, server)

shinyApp(ui, server)
