# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(zipcodeR)) install.packages("zipcodeR", repos = "http://cran.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
#if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(reticulate)) install.packages("reticulate", repos = "http://cran.us.r-project.org")
if(!require(survival)) install.packages("survival", repos = "http://cran.us.r-project.org")
if(!require(survminer)) install.packages("survminer", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("gthemes", repos = "http://cran.us.r-project.org")


data_mm_polis = "input_data/data_mm_polis.csv"

# Read the polis data, rename fips column, set age = NA for age = 999
mm_polis_raw <- as.data.frame(read.csv(data_mm_polis, header = TRUE, sep = ",")) %>% 
  dplyr::rename('fips' = 'County.FIPS.at.Diagnosis') %>% 
  filter(Age.at.Dignosis != 999)


# polis_survival <- mm_polis_raw %>% 
#   select(c(Date.of.Diagnosis, Date.of.last.contact, Age.at.Dignosis, Sex, Primary.Site, Histology.TypeICDO2, Chemotherapy, 
#            Immunotherapy.BRM, Other.treatment, Stage.by..clinical.stage., SEER.Summary.Stage.1977,
#            SEER.Summary.Stage.2000, Vital.Status))
# polis_survival$survival_days <- (as.Date(polis_survival$Date.of.last.contact)-as.Date(polis_survival$Date.of.Diagnosis)) /365.25

# polis_survival_curve = function(polis_survival, variable=1) {
#   survival_plot <- ggsurvplot(survfit(as.formula(paste("Surv(survival_days, Vital.Status) ~", variable)), 
#                                       data = polis_survival), data = polis_survival, pval = TRUE)
#   survival_plot
# }

ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Multiple Myeloma - Polis Center</a>'), id="nav",
             windowTitle = "MM - POLIS Center",
             
             tabPanel("Survival Analysis",
                      mainPanel(
                        tabsetPanel(
                          tabPanel("KM Survival Curve", 
                                   fluidRow(br(), 
                                            radioGroupButtons("surv_independent", "Choose independent variable:",
                                                              choiceNames = c("Immunotherapy", "Sex", "Chemotherapy"),
                                                              choiceValues = c("Immunotherapy.BRM", "Sex", "Chemotherapy")), align = "center"),
                                   fluidRow(plotOutput("polis_survival_plot")))
                        )
                      )
             )
  )
)

server = function(input, output, session) {
  polis_survival_reactive = reactive({
    polis_survival <- mm_polis_raw %>% 
      select(c(Date.of.Diagnosis, Date.of.last.contact, Age.at.Dignosis, Sex, Primary.Site, Histology.TypeICDO2, Chemotherapy, 
               Immunotherapy.BRM, Other.treatment, Stage.by..clinical.stage., SEER.Summary.Stage.1977,
               SEER.Summary.Stage.2000, Vital.Status))
    polis_survival$survival_days <- (as.Date(polis_survival$Date.of.last.contact)-as.Date(polis_survival$Date.of.Diagnosis)) /365.25
    polis_survival
  })
  
  output$polis_survival_plot <- renderPlot({
    polis_survival <- polis_survival_reactive()
    survival_plot <- ggsurvplot(survfit(Surv(survival_days, Vital.Status) ~ input$independent, 
                                        data = polis_survival), data = polis_survival, pval = TRUE)
    survival_plot
    #polis_survival_curve(polis_survival_reactive(), input$independent)
    # print(input$independent)
    # polis_survival <- polis_survival_reactive()
    # survival_plot <- ggsurvplot(survfit(as.formula(paste('Surv(survival_days, Vital.Status) ~', input$independent)), 
    #                                     data = polis_survival), data = polis_survival, pval = TRUE)
    # survival_plot
  })
}

shinyApp(ui, server)

  
  
  
  