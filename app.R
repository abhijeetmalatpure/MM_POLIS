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

source("variable_map.R")

### County information for Indiana map from Census.gov ###
county_zip <- "input_data/county_map.zip"
county_file <- "cb_2019_us_county_500k.shp"

if(!file.exists(paste0("input_data/counties/",county_file))) {
  # Download zip file from census.gov for the FIPS codes 
  download.file("https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_county_500k.zip", county_zip)
  
  #Unzip the ZIP file, and load counties data from shp file, 
  # add fips column combining STATEFP, COUNTYFP
  unzip(county_zip, exdir = "input_data/counties")
}

counties <- st_read(paste0("input_data/counties/",county_file)) %>% 
  mutate(fips = as.numeric(paste0(STATEFP, COUNTYFP))) 

# Delete directory, files and zip file
#unlink("input_data/counties", recursive = TRUE)
#unlink(county_zip)

# Only store Indiana counties
counties <- counties[counties$STATEFP==18,]

### DATA PROCESSING: POLIS CENTER ###
use_python(Sys.which("python"), required = TRUE)

data_mm_polis = "input_data/data_mm_polis.csv"

if(!file.exists(data_mm_polis)) {
  print("Running clean_polis_data script to fix date issues")
  if(!py_module_available("pandas")) {
    py_install("pandas")
  }
  source_python("clean_polis_data.py")
}

# Read the polis data, rename fips column, set age = NA for age = 999
mm_polis_raw <- as.data.frame(read.csv(data_mm_polis, header = TRUE, sep = ",")) %>% 
  dplyr::rename('fips' = 'County.FIPS.at.Diagnosis') 

polis_county_summary <- mm_polis_raw %>% group_by(fips) %>% summarise(age = round(mean(Age.at.Dignosis), 1), 
                                                                      total_count = n(),
                                                                      minYearDiag = min(YearOfDiagnosis),
                                                                      maxYearDiag = max(YearOfDiagnosis),
                                                                      death_count = sum(Vital.Status==0),
                                                                      death_rate = sum(Vital.Status==0)/total_count, .groups = 'keep')

polis_county_summary <- st_as_sf(polis_county_summary %>% left_join(counties, by = 'fips'))
polis_county_summary <- sf::st_transform(polis_county_summary, "+proj=longlat +datum=WGS84")

polis_age <- mm_polis_raw %>% select(c(fips, Age.at.Dignosis))

polis_diagnosis <- mm_polis_raw %>% select(c(fips, Diagnostic.confirmation))


polis_patient_info <- mm_polis_raw %>% 
  select(c(fips, Age.at.Dignosis, Diagnostic.confirmation, Race1, Postal.code.at.diagnosis)) %>% 
  left_join(counties, by = 'fips')

polis_survival <- mm_polis_raw %>% 
  select(c(Date.of.Diagnosis, Date.of.last.contact, Age.at.Dignosis, Sex, Primary.Site, Histology.TypeICDO2, Chemotherapy, 
           Immunotherapy.BRM, Other.treatment, Stage.by..clinical.stage., SEER.Summary.Stage.1977,
           SEER.Summary.Stage.2000, Vital.Status)) %>% filter(Age.at.Dignosis != 999)
polis_survival$survival_days <- (as.Date(polis_survival$Date.of.last.contact)-as.Date(polis_survival$Date.of.Diagnosis)) /365.25

agePalette <- colorNumeric(palette = "Reds", domain=polis_county_summary$age)
drPalette <- colorNumeric(palette = "Blues", domain=polis_county_summary$death_rate)

polis_age_plot = function(polis_age) {
  label <- "Age Distribution:"
  county_name <- unique(polis_age$NAME)
  if(length(county_name) == 1) {
    label <- paste(label, county_name, "County")
  }
  else
    label <- paste(label, "Indiana")
  
  age_distribution <- ggplot(polis_age, 
                             aes(x = `Age.at.Dignosis`)) + 
    geom_density(na.rm = TRUE, fill = "indianred3") +
    labs(title = label, x = "Age", y = "Proportion") 
  age_distribution
}

polis_diagnosis_plot = function(polis_diagnosis) {
  label <- "Diagnosis Counts:"
  county_name <- unique(polis_diagnosis$NAME)
  if(length(county_name) == 1) {
    label <- paste(label, county_name, "County")
  }
  else
    label <- paste(label, "Indiana")
  
  diagnosis_distribution <- ggplot(polis_diagnosis, 
                                   aes(x = factor(Diagnostic.confirmation))) + 
    geom_bar(position="dodge", na.rm = TRUE, fill = "Blue") +
    labs(title = label, x = "Diagnosis", y = "Count") 
  diagnosis_distribution
}


labels <- sprintf(
  "<strong>%s</strong><br/>Average age: %g years,<br/> # of subjects: %g",
  polis_county_summary$NAME, polis_county_summary$age, polis_county_summary$total_count
) %>% lapply(htmltools::HTML)

labels_death <- sprintf(
  "<strong>%s</strong><br/>Death rate: %g,<br/> # of deaths: %g,<br/> # of subjects: %g",
  polis_county_summary$NAME, polis_county_summary$death_rate, polis_county_summary$death_count, polis_county_summary$total_count
) %>% lapply(htmltools::HTML)

polismap =
  leaflet(polis_county_summary) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              smoothFactor = 0.2,
              fillOpacity = .7,
              fillColor = ~agePalette(polis_county_summary$age),
              color = "white",
              dashArray = "1",
              group = "Age",
              weight = 1,
              popup = polis_county_summary$NAME,
              highlight = highlightOptions(
                weight = 2,
                color = "#666",
                dashArray = "",
                bringToFront = TRUE),
              layerId = polis_county_summary$ALAND,
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
  ) %>%
  addPolygons(stroke=TRUE,
              smoothFactor = 0.2,
              fillOpacity = .7,
              fillColor = ~drPalette(polis_county_summary$death_rate),
              color = "white",
              dashArray = "1",
              group = "Death Rate",
              weight = 1,
              popup = polis_county_summary$NAME,
              highlight = highlightOptions(
                weight = 2,
                color = "#666",
                dashArray = "",
                bringToFront = TRUE),
              label = labels_death,
              layerId = polis_county_summary$ALAND,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
  ) %>%
  addLayersControl(
    baseGroups = c("Age", "Death Rate"),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend("bottomright", pal = agePalette, values = ~polis_county_summary$age, group = "Age",
            title = "<small>Average Age<br/></small>") %>%
  addLegend("bottomright", pal = drPalette, values = ~polis_county_summary$death_rate, group = "DeathRate",
            title = "<small>Death Rate</small>")

patientmap =
  leaflet(polis_county_summary) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -86.12, lat = 40.27, zoom = 6.5)


polis_survival_curve_immuno = function(polis_survival) {
  fit <- survfit(Surv(survival_days, Vital.Status) ~ Immunotherapy.BRM, data = polis_survival)
  ggsurvplot(fit, data = polis_survival, pval = TRUE, xlab = "Time in years", ylab = "Survival", legend.labs = legends)
}

polis_survival_curve_chemo = function(polis_survival) {
  fit <- survfit(Surv(survival_days, Vital.Status) ~ Chemotherapy, data = polis_survival)
  ggsurvplot(fit, data = polis_survival, pval = TRUE, xlab = "Time in years", ylab = "Survival")
}

polis_survival_curve_other = function(polis_survival) {
  fit <- survfit(Surv(survival_days, Vital.Status) ~ Other.treatment, data = polis_survival)
  ggsurvplot(fit, data = polis_survival, pval = TRUE, xlab = "Time in years", ylab = "Survival")
}

polis_survival_curve_sex = function(polis_survival) {
  legends <- sort(names(map_sex[unique(polis_survival$Sex)]))
  fit <- survfit(Surv(survival_days, Vital.Status) ~ Sex, data = polis_survival)
  ggsurvplot(fit, data = polis_survival, pval = TRUE, xlab = "Time in years", ylab = "Survival",
             legend.labs = legends)
}

polis_survival_curve_site = function(polis_survival) {
  if(length(unique(polis_survival$Primary.Site)) >= 10)
    return(TRUE)
  fit <- survfit(Surv(survival_days, Vital.Status) ~ Primary.Site, data = polis_survival)
  ggsurvplot(fit, data = polis_survival, pval = TRUE, xlab = "Time in years", ylab = "Survival")
}

polis_survival_curve_histology = function(polis_survival) {
  fit <- survfit(Surv(survival_days, Vital.Status) ~ Histology.TypeICDO2, data = polis_survival)
  ggsurvplot(fit, data = polis_survival, pval = TRUE, xlab = "Time in years", ylab = "Survival")
}

polis_survival_curve_stage = function(polis_survival) {
  fit <- survfit(Surv(survival_days, Vital.Status) ~ Stage.by..clinical.stage., data = polis_survival)
  ggsurvplot(fit, data = polis_survival, pval = TRUE, xlab = "Time in years", ylab = "Survival")
}

polis_survival_curve_seer1977 = function(polis_survival) {
  fit <- survfit(Surv(survival_days, Vital.Status) ~ SEER.Summary.Stage.1977, data = polis_survival)
  ggsurvplot(fit, data = polis_survival, pval = TRUE, xlab = "Time in years", ylab = "Survival")
}

polis_survival_curve_seer2000 = function(polis_survival) {
  fit <- survfit(Surv(survival_days, Vital.Status) ~ SEER.Summary.Stage.2000, data = polis_survival)
  ggsurvplot(fit, data = polis_survival, pval = TRUE, xlab = "Time in years", ylab = "Survival")
}

polis_piechart <- function(pie_data, chart_name) {
  plot_ly(pie_data, labels = ~label, values = ~Freq, type = "pie", showlegend = FALSE, 
          textposition = "inside") %>% 
    layout(title = chart_name)
  }

### SHINY UI ###
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Multiple Myeloma - Polis Center</a>'), id="nav",
             windowTitle = "MM - POLIS Center",
             
             tabPanel("Survival Analysis",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          span(tags$i(h5("Select values from filters below to select a subset:")), style="color:#045a8d"),
                          sliderInput("km_age_group",
                                      "Age Group:",
                                      min = as.numeric(min(polis_survival$Age.at.Dignosis)),
                                      max = max(as.numeric(polis_survival[polis_survival$Age.at.Dignosis<999,"Age.at.Dignosis"])),
                                      value=(c(min(polis_survival$Age.at.Dignosis), max(as.numeric(polis_survival[polis_survival$Age.at.Dignosis<999,"Age.at.Dignosis"])))),
                                      step = 1,
                                      ),
                          pickerInput("km_sex", "Gender:",
                                      choices = map_sex,
                                      options = list(`actions-box` = TRUE),
                                      multiple = TRUE),
                          
                          pickerInput("km_primary_site", "Primary Site:",
                                      options = list(`actions-box` = TRUE),
                                      choices = map_primary_site,
                                      multiple = TRUE),
                          
                          pickerInput("km_histology", "Histology TypeICDO2:",
                                      options = list(`actions-box` = TRUE),
                                      choices = map_histology,
                                      multiple = TRUE),
                          
                          pickerInput("km_chemotherapy", "Chemotherapy:",
                                      options = list(`actions-box` = TRUE),
                                      choices = map_chemotherapy,
                                      selected = c(0,1,2,3),
                                      multiple = TRUE),
                          
                          pickerInput("km_immunotherapy", "Immunotherapy:",
                                      options = list(`actions-box` = TRUE),
                                      choices = map_immunotherapy,
                                      selected = c(0,1),
                                      multiple = TRUE),
                          
                          pickerInput("km_othertrt", "Other Treatment:",
                                      options = list(`actions-box` = TRUE),
                                      choices = map_other_trt,
                                      multiple = TRUE),
                          
                          pickerInput("km_clinical_stage", "Stage by Clinical-stage:",
                                      options = list(`actions-box` = TRUE),
                                      choices = sort(unique(polis_survival$Stage.by..clinical.stage.)),
                                      multiple = TRUE),
                          
                          pickerInput("km_seer_1977", "SEER Summary Stage 1977:",
                                      options = list(`actions-box` = TRUE),
                                      choices = map_seer_stages,
                                      multiple = TRUE),
                          
                          pickerInput("km_seer_2000", "SEER Summary Stage 2000:",
                                      options = list(`actions-box` = TRUE),
                                      choices = map_seer_stages,
                                      multiple = TRUE),
                          ),
                        
                        mainPanel(
                              fluidRow(
                                tabsetPanel(type = "pills",
                                  tabPanel("Gender", 
                                           fluidRow(plotOutput("polis_survival_sex"))
                                  ),
                                  tabPanel("Site", 
                                           fluidRow(column(width = 8, offset = 2, "Please choose primary site(s) from the Primary Site dropdown filter for this chart.")),
                                           fluidRow(plotOutput("polis_survival_site"))
                                  ),
                                  tabPanel("Histology", 
                                           fluidRow(plotOutput("polis_survival_histology")),
                                  ),
                                  tabPanel("Chemotherapy", 
                                           fluidRow(plotOutput("polis_survival_chemo")),
                                           ),
                                  tabPanel("Immunotherapy",
                                           fluidRow(plotOutput("polis_survival_immuno")),
                                           ),
                                  tabPanel("Other Treatments",
                                           fluidRow(plotOutput("polis_survival_other_trt"))
                                  ),
                                  tabPanel("Clinical Stage",
                                           fluidRow(plotOutput("polis_survival_stage"))
                                           ),
                                  tabPanel("SEER-1977",
                                           fluidRow(plotOutput("polis_survival_seer1977"))
                                  ),
                                  tabPanel("SEER-2000",
                                           fluidRow(plotOutput("polis_survival_seer2000"))
                                           )
                                  )
                                ),
                              fluidRow(br(),br(),
                                fluidRow(column(width = 6, plotlyOutput("pie_chemo")),
                                         column(width = 6, plotlyOutput("pie_immuno"))
                                         ),br(),
                                fluidRow(column(width = 6, plotlyOutput("pie_other_trt")),
                                         column(width = 6, plotlyOutput("pie_histology"))
                                         ),
                                )
                              )
                        )
                      ),
             
             tabPanel("County-level Maps",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("polis", width="100%", height="100%"),
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        bottom = 40, left = 20, width = 350, fixed=TRUE,
                                        position = "bottomleft", draggable = FALSE, height = "auto",
                                        plotOutput("polis_diagnosis_plot", height="200px", width="100%"),
                                        plotOutput("polis_age_plot", height="200px", width="100%"),
                                        sliderInput("year_range",
                                                    "Diagnosis Year Range:",
                                                    min = as.numeric(min(polis_county_summary$minYearDiag)),
                                                    max = as.numeric(max(polis_county_summary$maxYearDiag)),
                                                    value=(c(min(polis_county_summary$minYearDiag), as.numeric(max(polis_county_summary$maxYearDiag)))),
                                                    step = 1,
                                        )
                          ),
                          #verbatimTextOutput("county_table"),
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.iu.edu', tags$img(src='IU.H_WEB.png',height="10%",width="10%"))),
                          
                      )
             ),
             
             
             tabPanel("Data",
                      numericInput("maxrows", "Rows to show", 25),
                      verbatimTextOutput("rawtable"),
                      downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                      "Source Code: ", tags$a(href="https://github.com/iuabhmalat/MM_POLIS", 
                                              "Indiana University")
             ),
             
             tabPanel("About this site",
                      span(tags$i(h5("This is a site to display Multiple Myeloma data from the IUPUI Polis Center. Version 0.1. Please see the file below for metadata")), style="color:#045a8d"),
                      uiOutput("metadata"),
                      absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, fixed=TRUE, draggable = FALSE, height = "auto",
                                    tags$a(href='https://www.iu.edu', tags$img(src='IU.H_WEB.png',height="10%",width="10%")))
             )
             
  )          
)

### SHINY SERVER ###

server = function(input, output, session) {
  polis_county_summary_reactive_db = reactive({
    polis_county_summary <- mm_polis_raw %>% 
      filter(between(YearOfDiagnosis, input$year_range[1], input$year_range[2])) %>% 
      group_by(fips) %>% 
      summarise(age = round(mean(Age.at.Dignosis), 1), 
                total_count = n(),
                minYearDiag = min(YearOfDiagnosis),
                maxYearDiag = max(YearOfDiagnosis),
                death_count = sum(Vital.Status==0),
                death_rate = sum(Vital.Status==0)/total_count, .groups = 'keep')
    
    polis_county_summary <- st_as_sf(polis_county_summary %>% left_join(counties, by = 'fips'))
    polis_county_summary <- sf::st_transform(polis_county_summary, "+proj=longlat +datum=WGS84")
    polis_county_summary
  })
  
  polis_age_reactive = reactive({
    county_id <- input$polis_shape_click$id
    polis_age <- mm_polis_raw %>% 
      filter(between(YearOfDiagnosis, input$year_range[1], input$year_range[2])) %>% 
      filter(Age.at.Dignosis <=150) %>% 
      select(c(fips, Age.at.Dignosis)) %>% left_join(counties, by = 'fips')
    if(!(is.null(county_id))) {
      polis_age <- polis_age %>% filter(ALAND == county_id)
    } 
    polis_age
  })
  
  polis_diagnosis_reactive = reactive({
    county_id <- input$polis_shape_click$id
    polis_diagnosis <- mm_polis_raw %>% 
      filter(between(YearOfDiagnosis, input$year_range[1], input$year_range[2])) %>% 
      select(c(fips, Diagnostic.confirmation)) %>% left_join(counties, by = 'fips')
    if(!(is.null(county_id))) {
      polis_diagnosis <- polis_diagnosis %>% filter(ALAND == county_id)
    } 
    polis_diagnosis
  })
  
  polis_survival_reactive = reactive({
    survival_reactive <- mm_polis_raw %>% 
      select(c(Date.of.Diagnosis, Date.of.last.contact, Age.at.Dignosis, Sex, Primary.Site, Histology.TypeICDO2, Chemotherapy, 
               Immunotherapy.BRM, Other.treatment, Stage.by..clinical.stage., SEER.Summary.Stage.1977,
               SEER.Summary.Stage.2000, Vital.Status))
    survival_reactive$survival_days <- (as.Date(survival_reactive$Date.of.last.contact)-as.Date(survival_reactive$Date.of.Diagnosis)) /365.25
    
    filt_sex <- input$km_sex
    if(!is.null(filt_sex)) {
      survival_reactive <- survival_reactive %>% dplyr::filter(Sex %in% filt_sex)
    }
    filt_agegroup <- input$km_age_group
    if(!is.null(filt_agegroup)) {
      survival_reactive <- survival_reactive %>% dplyr::filter(between(Age.at.Dignosis, filt_agegroup[1], filt_agegroup[2]))
    }
    filt_primary_site <- input$km_primary_site
    if(!is.null(filt_primary_site)) {
      survival_reactive <- survival_reactive %>% dplyr::filter(Primary.Site %in% filt_primary_site)
    }
    filt_histology <- input$km_histology
    if(!is.null(filt_histology)) {
      survival_reactive <- survival_reactive %>% dplyr::filter(Histology.TypeICDO2 %in% filt_histology)
    }
    filt_chemotherapy <- input$km_chemotherapy
    if(!is.null(filt_chemotherapy)) {
      survival_reactive <- survival_reactive %>% dplyr::filter(Chemotherapy %in% filt_chemotherapy)
    }
    filt_immunotherapy <- input$km_immunotherapy
    if(!is.null(filt_immunotherapy)) {
      survival_reactive <- survival_reactive %>% dplyr::filter(Immunotherapy.BRM %in% filt_immunotherapy)
    }
    filt_othertrt <- input$km_othertrt
    if(!is.null(filt_othertrt)) {
      survival_reactive <- survival_reactive %>% dplyr::filter(Other.treatment %in% filt_othertrt)
    }
    filt_clinical_stage <- input$km_clinical_stage
    if(!is.null(filt_clinical_stage)) {
      survival_reactive <- survival_reactive %>% dplyr::filter(Stage.by..clinical.stage. %in% filt_clinical_stage)
    }
    filt_seer_1977 <- input$km_seer_1977
    if(!is.null(filt_seer_1977)) {
      survival_reactive <- survival_reactive %>% dplyr::filter(SEER.Summary.Stage.1977 %in% filt_seer_1977)
    }
    filt_seer_2000 <- input$km_seer_2000
    if(!is.null(filt_seer_2000)) {
      survival_reactive <- survival_reactive %>% dplyr::filter(SEER.Summary.Stage.2000 %in% filt_seer_2000)
    }
    survival_reactive
  })
  
  output$polis <- renderLeaflet({ 
    polismap
  })
  
  output$patient <- renderLeaflet({ 
    patientmap
  })
  
  observeEvent(input$year_range, {
    filtered_polis_county_summary = polis_county_summary_reactive_db()
    labels <- sprintf("<strong>%s</strong><br/>Average age: %g years,<br/> # of subjects: %g",
                      filtered_polis_county_summary$NAME, filtered_polis_county_summary$age, filtered_polis_county_summary$total_count) %>% lapply(htmltools::HTML)
    labels_death <- sprintf(
      "<strong>%s</strong><br/>Death rate: %g,<br/> # of deaths: %g,<br/> # of subjects: %g", 
      filtered_polis_county_summary$NAME, filtered_polis_county_summary$death_rate, filtered_polis_county_summary$death_count, filtered_polis_county_summary$total_count
    ) %>% lapply(htmltools::HTML)
    
    agePalette <- colorNumeric(palette = "Reds", domain=filtered_polis_county_summary$age)
    drPalette <- colorNumeric(palette = "Blues", domain=filtered_polis_county_summary$death_rate)
    
    leafletProxy("polis", data = filtered_polis_county_summary) %>%
      removeControl("legend") %>%
      clearShapes() %>%
      clearTiles() %>% 
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke=TRUE,
                  smoothFactor = 0.2,
                  fillOpacity = .7,
                  fillColor = ~agePalette(filtered_polis_county_summary$age),
                  color = "white",
                  dashArray = "1",
                  group = "Age",
                  weight = 1,              
                  popup = filtered_polis_county_summary$NAME,
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#666",
                    dashArray = "",
                    bringToFront = TRUE),
                  label = labels,
                  layerId = filtered_polis_county_summary$ALAND,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      ) %>%
      addPolygons(stroke=TRUE,
                  smoothFactor = 0.2,
                  fillOpacity = .7,
                  fillColor = ~drPalette(filtered_polis_county_summary$death_rate),
                  color = "white",
                  dashArray = "1",
                  group = "Death Rate",
                  weight = 1,
                  popup = filtered_polis_county_summary$NAME,
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#666",
                    dashArray = "",
                    bringToFront = TRUE),
                  label = labels_death,
                  layerId = filtered_polis_county_summary$ALAND,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      )
  })
  
  observeEvent(input$polis_click, {
    data <- reactiveValues(clickedMarker=NULL)
    data$clickedMarker <- NULL
  })
  
  output$polis_age_plot <- renderPlot({
    polis_age_plot(polis_age_reactive())
  })
  
  output$polis_diagnosis_plot <- renderPlot({
    polis_diagnosis_plot(polis_diagnosis_reactive())
  })
  
  output$polis_survival_immuno <- renderPlot({
    polis_survival_curve_immuno(polis_survival_reactive())
  })
  
  output$polis_survival_chemo <- renderPlot({
    polis_survival_curve_chemo(polis_survival_reactive())
  })
  
  output$polis_survival_sex <- renderPlot({
    polis_survival_curve_sex(polis_survival_reactive())
  })
  
  output$polis_survival_site <- renderPlot({
    polis_survival_curve_site(polis_survival_reactive())
  })

    output$polis_survival_histology <- renderPlot({
    polis_survival_curve_histology(polis_survival_reactive())
  })
  
  output$polis_survival_stage <- renderPlot({
    polis_survival_curve_stage(polis_survival_reactive())
  })

    output$polis_survival_other_trt <- renderPlot({
    polis_survival_curve_other(polis_survival_reactive())
  })

  output$polis_survival_seer1977 <- renderPlot({
    polis_survival_curve_seer1977(polis_survival_reactive())
  })
  
  output$polis_survival_seer2000 <- renderPlot({
    polis_survival_curve_seer2000(polis_survival_reactive())
  })

    # Pie charts for chemo, immuno, other treatments
  output$pie_chemo <- renderPlotly({
    chemo <- data.frame(table(polis_survival_reactive() %>% dplyr::select(Chemotherapy)))
    chemo$label <- names(map_chemotherapy[match(chemo$Var1, unname(map_chemotherapy))])
    polis_piechart(chemo, "Chemotherapy")
  })

  output$pie_immuno <- renderPlotly({
    immuno <- data.frame(table(polis_survival_reactive() %>% dplyr::select(Immunotherapy.BRM)))
    immuno$label <- names(map_immunotherapy[match(immuno$Var1, unname(map_immunotherapy))])
    polis_piechart(immuno, "Immunotherapy BRM")
  })

  output$pie_other_trt <- renderPlotly({
    other_trt <- data.frame(table(polis_survival_reactive() %>% dplyr::select(Other.treatment)))
    other_trt$label <- names(map_other_trt[match(other_trt$Var1, unname(map_other_trt))])
    polis_piechart(other_trt, "Other Treatments")
  })
  
  output$pie_histology <- renderPlotly({
    histology <- data.frame(table(polis_survival_reactive() %>% dplyr::select(Histology.TypeICDO2)))
    histology$label <- histology$Var1
    polis_piechart(histology, "Histology")
  })
  
  output$records <- renderText({nrow(polis_survival_reactive())})
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste0("MM_POLIS_", cv_today$date[1], ".csv")
    },
    content = function(file) {
      write.csv(mm_polis_raw, file, row.names = FALSE)
    }
  )
  
  output$metadata <- renderUI({
    addResourcePath("resources", "input_data/")
    tags$iframe(
      seamless="seamless",
      src="resources/2018PolicyandProcedureManual.pdf",
      style="height:550px; width:100%; border:0; scrolling:yes")
  })
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(head(mm_polis_raw[,colSums(is.na(mm_polis_raw))<nrow(mm_polis_raw)], input$maxrows), row.names = FALSE)
    options(orig)
  })
  
}

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
