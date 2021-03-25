## COVID-2019 interactive mapping tool
## Edward Parker, London School of Hygiene & Tropical Medicine (edward.parker@lshtm.ac.uk), last updated April 2020

## includes code adapted from the following sources:
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example

# update data with automated script
#source("jhu_data_daily_cases.R") # option to update daily cases
#source("jhu_data_weekly_cases.R") # run locally to update numbers, but not live on Rstudio server /Users/epp11/Dropbox (VERG)/GitHub/nCoV_tracker/app.R(to avoid possible errors on auto-updates)
#source("ny_data_us.R") # run locally to update numbers, but not live on Rstudio server (to avoid possible errors on auto-updates)

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
if(!require(reticulate)) install.packages("reticulate", repos = "https://cloud.r-project.org/")

# set mapping colour for each outbreak
covid_col = "#cc4c02"
covid_other_col = "#662506"
sars_col = "#045a8d"
h1n1_col = "#4d004b"
ebola_col = "#016c59"

# import data
cv_cases = read.csv("input_data/coronavirus.csv")
sars_cases = read.csv("input_data/sars.csv")
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
ebola_cases = read.csv("input_data/ebola.csv")
h1n1_cases = read.csv("input_data/h1n1.csv")
worldcountry = geojson_read("input_data/50m.geojson", what = "sp")
country_geoms = read.csv("input_data/country_geoms.csv")
cv_states = read.csv("input_data/coronavirus_states.csv")


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

# Read Polis data, merge with counties 
# polis_dtypes = c("numeric","date","date","numeric","numeric","text","text","numeric","numeric",
#                  "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
#                  "numeric","numeric","numeric","numeric","numeric","numeric","text","numeric",
#                  "text","numeric","numeric","numeric","date","numeric","text","numeric","numeric",
#                  "numeric","numeric","numeric","numeric","numeric","numeric","numeric","date",
#                  "numeric","numeric","text","numeric","numeric","numeric","numeric","text","text",
#                  "numeric","numeric","text","text")
# mm_polis = read_excel(data_mm_polis, sheet = "DataFile_MultipleMyeloma") %>% 
#   dplyr::rename('fips' = 'County FIPS at Diagnosis')

mm_polis_raw <- as.data.frame(read.csv(data_mm_polis, header = TRUE, sep = ",")) %>% dplyr::rename('fips' = 'County.FIPS.at.Diagnosis')

polis_county_summary <- mm_polis_raw %>% group_by(fips) %>% summarise(age = round(mean(Age.at.Dignosis), 1), 
                                                          total_count = n(),
                                                          minYearDiag = min(YearOfDiagnosis),
                                                          maxYearDiag = max(YearOfDiagnosis),
                                                          death_count = sum(Vital.Status==0),
                                                          death_rate = sum(Vital.Status==0)/total_count)

polis_county_summary <- st_as_sf(polis_county_summary %>% left_join(counties))
polis_county_summary <- sf::st_transform(polis_county_summary, "+proj=longlat +datum=WGS84")

# create color palettes
#yodPalette <- colorNumeric(palette = "Oranges", domain=polis_county_summary$YearOfDiagnosis)
agePalette <- colorNumeric(palette = "Reds", domain=polis_county_summary$age)
drPalette <- colorNumeric(palette = "Blues", domain=polis_county_summary$death_rate)
#chemPalette <- colorNumeric(palette = "Purples", domain=polis_county_summary$Chemotherapy)
# 
# 
# # create map
# leaflet(polis_county_summary) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(stroke=FALSE,
#               smoothFactor = 0.2,
#               fillOpacity = .8,
#               popup = agepopup,
#               color = ~agePalette(polis_county_summary$Age.at.Dignosis),
#               group = "Age"
#   ) %>% 
#   
#   addPolygons(stroke=FALSE,
#               smoothFactor = 0.2,
#               fillOpacity = .8,
#               popup = diagpopup,
#               color = ~diagPalette(polis_county_summary$Diagnostic.confirmation),
#               group = "Diagnostic confirmation"
#   ) %>%
#   
#   addPolygons(stroke=FALSE,
#               smoothFactor = 0.2,
#               fillOpacity = .8,
#               popup = chempopup,
#               color = ~chemPalette(polis_county_summary$Chemotherapy),
#               group = "Chemotherapy"
#   ) %>%
#   
#   addPolygons(stroke=FALSE,
#               smoothFactor = 0.2,
#               fillOpacity = .8,
#               popup = yodpopup,
#               color = ~yodPalette(polis_county_summary$YearOfDiagnosis),
#               group = "Year of Diagnosis"
#   ) %>%
#   
#   addLayersControl(
#     baseGroups=c("Age", "Diagnostic confirmation", "Chemotherapy", "Year of Diagnosis"),
#     position = "bottomleft",
#     options = layersControlOptions(collapsed = FALSE)
#   )


### MAP FUNCTIONS ###
# function to plot cumulative COVID cases by date
cumulative_plot = function(cv_aggregated, plot_date) {
  plot_df = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("Cumulative cases") +  xlab("Date") + theme_bw() + 
    scale_colour_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
  plot_df_new = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df_new, aes(x = date, y = new, colour = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    # geom_bar(position="stack", stat="identity") + 
    ylab("New cases (weekly)") + xlab("Date") + theme_bw() + 
    scale_colour_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}


# function to plot new cases by region
country_cases_plot = function(cv_cases, start_point=c("Date", "Week of 100th confirmed case", "Week of 10th death"), plot_start_date) {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = new_outcome, fill = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
      xlim(c(plot_start_date,(current_date+5))) + xlab("Date")
  }
  
  if (start_point=="Week of 100th confirmed case") {
    cv_cases = subset(cv_cases, weeks_since_case100>0)
    g = ggplot(cv_cases, aes(x = weeks_since_case100, y = new_outcome, fill = region, group = 1,
                             text = paste0("Week ",weeks_since_case100, "\n", region, ": ",new_outcome)))+
      xlab("Weeks since 100th confirmed case") #+ xlim(c(plot_start_date,(current_date+5))) 
  }
  
  if (start_point=="Week of 10th death") {
    cv_cases = subset(cv_cases, weeks_since_death10>0)
    g = ggplot(cv_cases, aes(x = weeks_since_death10, y = new_outcome, fill = region, group = 1,
                             text = paste0("Week ",weeks_since_death10, "\n", region, ": ",new_outcome))) +
      xlab("Weeks since 10th death") #+ xlim(c(plot_start_date,(current_date+5))) 
  }
  
  g1 = g +
    geom_bar(position="stack", stat="identity") + 
    ylab("New (weekly)") + theme_bw() + 
    scale_fill_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative cases by region
country_cases_cumulative = function(cv_cases, start_point=c("Date", "Week of 100th confirmed case", "Week of 10th death"), plot_start_date) {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      xlim(c(plot_start_date,(current_date+1))) + xlab("Date")
  }
  
  if (start_point=="Week of 100th confirmed case") {
    cv_cases = subset(cv_cases, weeks_since_case100>0)
    g = ggplot(cv_cases, aes(x = weeks_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Week ", weeks_since_case100,"\n", region, ": ",outcome))) +
      xlab("Weeks since 100th confirmed case")
  }
  
  if (start_point=="Week of 10th death") {
    cv_cases = subset(cv_cases, weeks_since_death10>0)
    g = ggplot(cv_cases, aes(x = weeks_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Week ", weeks_since_death10,"\n", region, ": ",outcome))) +
      xlab("Weeks since 10th death")
  }
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("Cumulative") + theme_bw() + 
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative cases by region on log10 scale
country_cases_cumulative_log = function(cv_cases, start_point=c("Date", "Week of 100th confirmed case", "Week of 10th death"), plot_start_date)  {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      xlim(c(plot_start_date,(current_date+1))) + xlab("Date")
  }
  
  if (start_point=="Week of 100th confirmed case") {
    cv_cases = subset(cv_cases, weeks_since_case100>0)
    g = ggplot(cv_cases, aes(x = weeks_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Week ",weeks_since_case100, "\n", region, ": ",outcome))) +
      xlab("Weeks since 100th confirmed case")
  }
  
  if (start_point=="Week of 10th death") {
    cv_cases = subset(cv_cases, weeks_since_death10>0)
    g = ggplot(cv_cases, aes(x = weeks_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Week ",weeks_since_death10, "\n", region, ": ",outcome))) +
      xlab("Weeks since 10th death")
  }
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("Cumulative (log10)") + theme_bw() +
    scale_y_continuous(trans="log10") +
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to render plotly of epidemic comparison depending on selected outcome
comparison_plot = function(epi_comp, comparison) {
  epi_comp$outcome = epi_comp[,comparison] 
  epi_comp = epi_comp[order(epi_comp$outcome),]
  epi_comp$outbreak = factor(epi_comp$outbreak, levels=epi_comp$outbreak)
  
  p1 <- ggplot(epi_comp, aes(x = outbreak, y = outcome, fill=outbreak, text = paste0(outbreak, ": ",outcome))) + geom_bar(alpha = 0.8, stat="identity") +
    ylab("N") + xlab("") + theme_bw() + 
    scale_fill_manual(values=c("2019-COVID"=covid_col, "2003-SARS"=sars_col, "2014-Ebola"=ebola_col,"2009-H1N1 (swine flu)"=h1n1_col)) +
    theme(legend.position = "")
  
  if(comparison == "cfr") { p1 = p1 + ylab("%") }
  if(comparison == "deaths") { p1 = p1 + scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) }
  if(comparison == "cases") { p1 = p1 + scale_y_continuous(trans='log10', limits = c(1,1e8), breaks=c(1,1000,1e6,1e9), labels = function(l) {trans = l / 1000; paste0(trans, "K")}) }
  ggplotly(p1 + coord_flip(), tooltip = c("text")) %>% layout(showlegend = FALSE)
}



# ### DATA PROCESSING - POLIS data ###


### DATA PROCESSING: COVID-19 ###

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1) 

# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) { print("Error: inconsistent country names")}

# extract dates from cv data
if (any(grepl("/", cv_cases$date))) { 
  cv_cases$date = format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d") }
cv_cases$date = as.Date(cv_cases$date)
cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases = merge(cv_cases, countries, by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$cases_per_million = as.numeric(format(round(cv_cases$cases/(cv_cases$population/1000000),1),nsmall=1))
cv_cases$new_cases_per_million = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/1000000),1),nsmall=1))
cv_cases$million_pop = as.numeric(cv_cases$population>1e6)
cv_cases$deaths_per_million = as.numeric(format(round(cv_cases$deaths/(cv_cases$population/1000000),1),nsmall=1))
cv_cases$new_deaths_per_million = as.numeric(format(round(cv_cases$new_deaths/(cv_cases$population/1000000),1),nsmall=1))

# add variable for weeks since 100th case and 10th death
cv_cases$weeks_since_case100 = cv_cases$weeks_since_death10 = 0
for (i in 1:length(unique(cv_cases$country))) {
  country_name = as.character(unique(cv_cases$country))[i]
  country_db = subset(cv_cases, country==country_name)
  country_db$weeks_since_case100[country_db$cases>=100] = 0:(sum(country_db$cases>=100)-1)
  country_db$weeks_since_death10[country_db$deaths>=10] = 0:(sum(country_db$deaths>=10)-1)
  cv_cases$weeks_since_case100[cv_cases$country==country_name] = country_db$weeks_since_case100
  cv_cases$weeks_since_death10[cv_cases$country==country_name] = country_db$weeks_since_death10
}

# creat variable for today's data
cv_today = subset(cv_cases, date==current_date) 
current_case_count = sum(cv_today$cases)
current_case_count_China = sum(cv_today$cases[cv_today$country=="Mainland China"])
current_case_count_other = sum(cv_today$cases[cv_today$country!="Mainland China"])
current_death_count = sum(cv_today$deaths)

# create subset of state data for today's data
if (any(grepl("/", cv_states$date))) { 
  cv_states$date = format(as.Date(cv_states$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_states$date = as.Date(cv_states$date, format="%Y-%m-%d") }
cv_states_today = subset(cv_states, date==max(cv_states$date))

# create subset for countries with at least 1000 cases
cv_today_reduced = subset(cv_today, cases>=1000)

# write current day's data
write.csv(cv_today %>% select(c(country, date, update, cases, new_cases, deaths, new_deaths,
                                cases_per_million, new_cases_per_million,
                                deaths_per_million, new_deaths_per_million,
                                weeks_since_case100, weeks_since_death10)), "input_data/coronavirus_today.csv")

# aggregate at continent level
cv_cases_continent = subset(cv_cases, !is.na(continent_level)) %>% select(c(cases, new_cases, deaths, new_deaths, date, continent_level)) %>% group_by(continent_level, date) %>% summarise_each(funs(sum)) %>% data.frame()

# add variable for weeks since 100th case and 10th death
cv_cases_continent$weeks_since_case100 = cv_cases_continent$weeks_since_death10 = 0
cv_cases_continent$continent = cv_cases_continent$continent_level
for (i in 1:length(unique(cv_cases_continent$continent))) {
  continent_name = as.character(unique(cv_cases_continent$continent))[i]
  continent_db = subset(cv_cases_continent, continent==continent_name)
  continent_db$weeks_since_case100[continent_db$cases>=100] = 0:(sum(continent_db$cases>=100)-1)
  continent_db$weeks_since_death10[continent_db$deaths>=10] = 0:(sum(continent_db$deaths>=10)-1)
  cv_cases_continent$weeks_since_case100[cv_cases_continent$continent==continent_name] = continent_db$weeks_since_case100
  cv_cases_continent$weeks_since_death10[cv_cases_continent$continent==continent_name] = continent_db$weeks_since_death10
}

# add continent populations
cv_cases_continent$pop = NA
cv_cases_continent$pop[cv_cases_continent$continent=="Africa"] = 1.2e9
cv_cases_continent$pop[cv_cases_continent$continent=="Asia"] = 4.5e9
cv_cases_continent$pop[cv_cases_continent$continent=="Europe"] = 7.4e8
cv_cases_continent$pop[cv_cases_continent$continent=="North America"] = 5.8e8
cv_cases_continent$pop[cv_cases_continent$continent=="Oceania"] = 3.8e7
cv_cases_continent$pop[cv_cases_continent$continent=="South America"] = 4.2e8

# add normalised counts
cv_cases_continent$cases_per_million =  as.numeric(format(round(cv_cases_continent$cases/(cv_cases_continent$pop/1000000),1),nsmall=1))
cv_cases_continent$new_cases_per_million =  as.numeric(format(round(cv_cases_continent$new_cases/(cv_cases_continent$pop/1000000),1),nsmall=1))
cv_cases_continent$deaths_per_million =  as.numeric(format(round(cv_cases_continent$deaths/(cv_cases_continent$pop/1000000),1),nsmall=1))
cv_cases_continent$new_deaths_per_million =  as.numeric(format(round(cv_cases_continent$new_deaths/(cv_cases_continent$pop/1000000),1),nsmall=1))
write.csv(cv_cases_continent, "input_data/coronavirus_continent.csv")

# aggregate at global level
cv_cases_global = cv_cases %>% select(c(cases, new_cases, deaths, new_deaths, date, global_level)) %>% group_by(global_level, date) %>% summarise_each(funs(sum)) %>% data.frame()
cv_cases_global$weeks_since_case100 = cv_cases_global$weeks_since_death10 = 0:(nrow(cv_cases_global)-1)

# add normalised counts
cv_cases_global$pop = 7.6e9
cv_cases_global$cases_per_million =  as.numeric(format(round(cv_cases_global$cases/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$new_cases_per_million =  as.numeric(format(round(cv_cases_global$new_cases/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$deaths_per_million =  as.numeric(format(round(cv_cases_global$deaths/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$new_deaths_per_million =  as.numeric(format(round(cv_cases_global$new_deaths/(cv_cases_global$pop/1000000),1),nsmall=1))
write.csv(cv_cases_global, "input_data/coronavirus_global.csv")

# select large countries for mapping polygons
cv_large_countries = cv_today %>% filter(alpha3 %in% worldcountry$ADM0_A3)
if (all(cv_large_countries$alpha3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

# create plotting parameters for map
bins = c(0,10,50,100,500,1000,Inf)
cv_pal <- colorBin("Oranges", domain = cv_large_countries$cases_per_million, bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% cv_large_countries$alpha3, ]

# creat cv base map 
# Abhi - this is where the map is generated
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("2019-COVID (new)", "2019-COVID (cumulative)", "2009-H1N1 (swine flu)"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("2019-COVID (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola")) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-60,~60,70) %>%
  addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$deaths_per_million,
            title = "<small>Deaths per mill</small>") 

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
              highlight = highlightOptions(
                weight = 2,
                color = "#666",
                dashArray = "",
                bringToFront = TRUE),
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
              highlight = highlightOptions(
                weight = 2,
                color = "#666",
                dashArray = "",
                bringToFront = TRUE),
              label = labels_death,
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
         title = "<small>Death Rate</small>") #%>%
  # htmlwidgets::onRender("
  #   function(el, x) {
  #     var updateLegend = function () {
  #         var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
  # 
  #         document.querySelectorAll('.legend').forEach(a => a.hidden=true);
  #         document.querySelectorAll('.legend').forEach(l => {
  #           if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
  #         });
  #     };
  #     updateLegend();
  #     this.on('baselayerchange', el => updateLegend());
  #   }")

  # addPolygons(stroke=FALSE,
  #             smoothFactor = 0.2,
  #             fillOpacity = .8,
  #             popup = diagpopup,
  #             color = ~diagPalette(polis_county_summary$Diagnostic.confirmation),
  #             group = "Diagnostic confirmation"
  # ) %>%
  # 
  # addPolygons(stroke=FALSE,
  #             smoothFactor = 0.2,
  #             fillOpacity = .8,
  #             popup = chempopup,
  #             color = ~chemPalette(polis_county_summary$Chemotherapy),
  #             group = "Chemotherapy"
  # ) %>%
  # 
  # addPolygons(stroke=FALSE,
  #             smoothFactor = 0.2,
  #             fillOpacity = .8,
  #             popup = yodpopup,
  #             color = ~yodPalette(polis_county_summary$YearOfDiagnosis),
  #             group = "Year of Diagnosis"
  # ) %>%

  # addLayersControl(
  #   position = "bottomright",
  #   baseGroups=c("Age", "Diagnostic confirmation", "Chemotherapy", "Year of Diagnosis"),
  #   options = layersControlOptions(collapsed = FALSE)
  # ) %>%
  # 
  # sliderInput(inputId = "num", label = "Pick a number", 
  #             min = min(polis_county_summary$YearOfDiagnosis), 
  #             max = max(polis_county_summary$YearOfDiagnosis), step = 1, value = c(min(polis_county_summary$YearOfDiagnosis)+min(polis_county_summary$YearOfDiagnosis)/4, 
  #                                                                      min(polis_county_summary$YearOfDiagnosis)+min(polis_county_summary$YearOfDiagnosis)*3/4))


# sum cv case counts by date
cv_aggregated = aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)
names(cv_aggregated) = c("date", "cases")

# add variable for new cases in last 7 days
for (i in 1:nrow(cv_aggregated)) { 
  if (i==1) { cv_aggregated$new[i] = 0 }
  if (i>1) { cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] }
}

# add plotting region
cv_aggregated$region = "Global"
cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")

# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)
cls_names = c(as.character(unique(cv_cases$country)), as.character(unique(cv_cases_continent$continent)), as.character(unique(cv_states$state)),"Global")
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names




### SHINY UI ###
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Multiple Myeloma - Polis Center</a>'), id="nav",
             windowTitle = "MM - POLIS Center",
             
             # tabPanel("COVID-19 mapper",
             #          div(class="outer",
             #              tags$head(includeCSS("styles.css")),
             #              leafletOutput("mymap", width="100%", height="100%"),
             #              
             #              absolutePanel(id = "controls", class = "panel panel-default",
             #                            top = 75, left = 55, width = 250, fixed=TRUE,
             #                            draggable = TRUE, height = "auto",
             #                            
             #                            span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
             #                            h3(textOutput("reactive_case_count"), align = "right"),
             #                            h4(textOutput("reactive_death_count"), align = "right"),
             #                            h6(textOutput("clean_date_reactive"), align = "right"),
             #                            h6(textOutput("reactive_country_count"), align = "right"),
             #                            plotOutput("epi_curve", height="130px", width="100%"),
             #                            plotOutput("cumulative_plot", height="130px", width="100%"),
             #                            
             #                            sliderTextInput("plot_date",
             #                                            label = h5("Select mapping date"),
             #                                            choices = format(unique(cv_cases$date), "%d %b %y"),
             #                                            selected = format(current_date, "%d %b %y"),
             #                                            grid = FALSE,
             #                                            animate=animationOptions(interval = 3000, loop = FALSE))
             #                            
             #              ),
             #              
             #              absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
             #                            tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
             #              
             #              absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
             #                            actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
             #                                         onclick = sprintf("window.open('%s')", 
             #                                                           "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
             #              
             #              
             #          )
             # ),
             
             tabPanel("County-level Maps",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("polis", width="100%", height="100%"),
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        bottom = 40, left = 20, width = 250, fixed=TRUE,
                                        position = "bottomleft", draggable = FALSE, height = "auto",
                                        plotOutput("cumulative_plot", height="130px", width="100%"),
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

             # tabPanel("Region plots",
             #          
             #          sidebarLayout(
             #            sidebarPanel(
             #              
             #              span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
             #              span(tags$i(h6("Occasional anomalies (e.g. spikes in daily case counts) are generally caused by changes in case definitions.")), style="color:#045a8d"),
             #              
             #              pickerInput("level_select", "Level:",   
             #                          choices = c("Global", "Continent", "Country", "US state"), 
             #                          selected = c("Country"),
             #                          multiple = FALSE),
             #              
             #              pickerInput("region_select", "Country/Region:",   
             #                          choices = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country), 
             #                          options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
             #                          selected = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country)[1:10],
             #                          multiple = TRUE), 
             #              
             #              pickerInput("outcome_select", "Outcome:",   
             #                          choices = c("Deaths per million", "Cases per million", "Cases (total)", "Deaths (total)"), 
             #                          selected = c("Deaths per million"),
             #                          multiple = FALSE),
             #              
             #              pickerInput("start_date", "Plotting start date:",   
             #                          choices = c("Date", "Week of 100th confirmed case", "Week of 10th death"), 
             #                          options = list(`actions-box` = TRUE),
             #                          selected = "Date",
             #                          multiple = FALSE), 
             #              
             #              sliderInput("minimum_date",
             #                          "Minimum date:",
             #                          min = as.Date(cv_min_date,"%Y-%m-%d"),
             #                          max = as.Date(current_date,"%Y-%m-%d"),
             #                          value=as.Date(cv_min_date),
             #                          timeFormat="%d %b"),
             #              
             #              "Select outcome, regions, and plotting start date from drop-down menues to update plots. Countries with at least 1000 confirmed cases are included."
             #            ),
             #            
             #            mainPanel(
             #              tabsetPanel(
             #                tabPanel("Cumulative", plotlyOutput("country_plot_cumulative")),
             #                tabPanel("New", plotlyOutput("country_plot")),
             #                tabPanel("Cumulative (log10)", plotlyOutput("country_plot_cumulative_log"))
             #              )
             #            )
             #          )
             # ),
             

             tabPanel("Data",
                      numericInput("maxrows", "Rows to show", 25),
                      verbatimTextOutput("rawtable"),
                      downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                      "Source Code: ", tags$a(href="https://github.com/iuabhmalat/MM_POLIS", 
                                                                         "Indiana University")
             ),
             
             tabPanel("About this site",
                      tags$div(
                        "This is a site to display Multiple Myeloma data from the IUPUI Polis Center. Version 0.1"
                      ),
                      absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, fixed=TRUE, draggable = FALSE, height = "auto",
                                    tags$a(href='https://www.iu.edu', tags$img(src='IU.H_WEB.png',height="10%",width="10%")))
             )
             
  )          
)





### SHINY SERVER ###

server = function(input, output, session) {
  
  # covid tab 
  formatted_date = reactive({
    format(as.Date(input$plot_date, format="%d %b %y"), "%Y-%m-%d")
  })
  
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(formatted_date()),"%d %B %Y")
  })
  
  reactive_db = reactive({
    cv_cases %>% filter(date == formatted_date())
  })
  
  polis_county_summary_reactive_db = reactive({
    polis_county_summary <- mm_polis_raw %>% 
      filter(between(YearOfDiagnosis, input$year_range[1], input$year_range[2])) %>% 
      group_by(fips) %>% 
      summarise(age = round(mean(Age.at.Dignosis), 1), 
                total_count = n(),
                minYearDiag = min(YearOfDiagnosis),
                maxYearDiag = max(YearOfDiagnosis),
                death_count = sum(Vital.Status==0),
                death_rate = sum(Vital.Status==0)/total_count)
    
    polis_county_summary <- st_as_sf(polis_county_summary %>% left_join(counties))
    polis_county_summary <- sf::st_transform(polis_county_summary, "+proj=longlat +datum=WGS84")
    polis_county_summary
    })
  
  reactive_db_last7d = reactive({
    cv_cases %>% filter(date == formatted_date() & new_cases>0)
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
    #large_countries = reactive %>% filter(alpha3 %in% worldcountry$ADM0_A3)
    worldcountry_subset = worldcountry[worldcountry$ADM0_A3 %in% large_countries$alpha3, ]
    large_countries = large_countries[match(worldcountry_subset$ADM0_A3, large_countries$alpha3),]
    large_countries
  })
  
  reactive_db_large_last7d = reactive({
    large_countries = reactive_db_last7d() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
    large_countries = large_countries[order(large_countries$alpha3),]
    large_countries
  })
  
  reactive_polygons = reactive({
    worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large()$alpha3, ]
  })
  
  reactive_polygons_last7d = reactive({
    worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large_last7d()$alpha3, ]
  })
  
  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " cases")
  })
  
  output$reactive_death_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$deaths), big.mark=","), " deaths")
  })
  
  output$reactive_country_count <- renderText({
    paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " countries/regions affected")
  })
  
  output$reactive_new_cases_7d <- renderText({
    paste0(round((cv_aggregated %>% filter(date == formatted_date() & region=="Global"))$new/7,0), " 7-day average")
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })

  output$polis <- renderLeaflet({ 
    polismap
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      # setView(lng = -86.12, lat = 40.27, zoom = 6.5) %>%
      
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5.5), 
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
                       label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Cases per million: %g<br/>Deaths per million: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths, reactive_db()$cases_per_million, reactive_db()$deaths_per_million) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")) %>%  
      
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$deaths_per_million)) %>%
      
      addCircleMarkers(data = reactive_db_last7d(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5.5), 
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (new)",
                       label = sprintf("<strong>%s (7-day average)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Cases per million: %g<br/>Deaths per million: %g", reactive_db_last7d()$country, round(reactive_db_last7d()$new_cases/7,0), round(reactive_db_last7d()$new_deaths/7,0), round(reactive_db_last7d()$new_cases_per_million/7,1), round(reactive_db_last7d()$new_deaths_per_million/7,1)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto"))
  })
  
  observeEvent(input$year_range, {
    filtered_polis_county_summary = polis_county_summary_reactive_db()
    labels <- sprintf("<strong>%s</strong><br/>Average age: %g years,<br/> Count: %g",
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
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#666",
                    dashArray = "",
                    bringToFront = TRUE),
                  label = labels,
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
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#666",
                    dashArray = "",
                    bringToFront = TRUE),
                  label = labels_death,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      )
  })
  
  output$cumulative_plot <- renderPlot({
    cumulative_plot(cv_aggregated, formatted_date())
  })
  
  output$epi_curve <- renderPlot({
    new_cases_plot(cv_aggregated, formatted_date())
  })

  # add footnote for cases
  output$epi_notes_1 <- renderText({
    if(input$comparison_metric=="cases") { paste0("Note that the axis is on a log10 scale so moves in 10-fold increments.
                                                  The 60.8 million estimated cases of H1N1 dwarf all other outbreaks of plotted on a standard linear scale.") }
    })
  
  # add footnote for deaths
  output$epi_notes_2 <- renderText({
    if(input$comparison_metric=="deaths") { 
      paste0("For H1N1, the number of laboratory-confirmed deaths reported by the WHO is displayed. Subsequent modelling studies have estimated the actual number to be in the range of 123,000 to 203,000.")
    }
  })
  
  # add note for cfr
  output$epi_notes_3 <- renderText({
    if(input$comparison_metric=="cfr") { 
      paste0("For COVID-19, this displays the proportion of confirmed cases who have subsequently died. When factoring in mild or asymptomatic infections that are not picked up by case surveillance efforts, current estimates place the case fatality rate in the range of 0.3-1%.")
    }
  })
  
  # update region selections
  observeEvent(input$level_select, {
    if (input$level_select=="Global") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = "Global", selected = "Global")
    }
    
    if (input$level_select=="Continent") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = c("Africa", "Asia", "Europe", "North America", "South America"), 
                        selected = c("Africa", "Asia", "Europe", "North America", "South America"))
    }
    
    if (input$level_select=="US state") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = as.character(cv_states_today[order(-cv_states_today$cases),]$state), 
                        selected = as.character(cv_states_today[order(-cv_states_today$cases),]$state)[1:10])
    }
    
    if (input$level_select=="Country") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country), 
                        selected = as.character(cv_states_today[order(-cv_states_today$cases),]$state)[1:10])
    }
  }, ignoreInit = TRUE)
  
  # create dataframe with selected countries
  country_reactive_db = reactive({
    if (input$level_select=="Global") { 
      db = cv_cases_global
      db$region = db$global_level
    }
    if (input$level_select=="Continent") { 
      db = cv_cases_continent 
      db$region = db$continent
    }
    if (input$level_select=="Country") { 
      db = cv_cases
      db$region = db$country
    }
    if (input$level_select=="US state") { 
      db = cv_states
      db$region = db$state
    }
    
    if (input$outcome_select=="Cases (total)") { 
      db$outcome = db$cases
      db$new_outcome = db$new_cases
    }
    
    if (input$outcome_select=="Deaths (total)") { 
      db$outcome = db$deaths 
      db$new_outcome = db$new_deaths 
    }
    
    if (input$outcome_select=="Cases per million") { 
      db$outcome = db$cases_per_million 
      db$new_outcome = db$new_cases_per_million 
    }
    
    if (input$outcome_select=="Deaths per million") { 
      db$outcome = db$deaths_per_million 
      db$new_outcome = db$new_deaths_per_million 
    }
    
    db %>% filter(region %in% input$region_select)
  })
  
  # country-specific plots
  output$country_plot <- renderPlotly({
    country_cases_plot(country_reactive_db(), start_point=input$start_date, input$minimum_date)
  })
  
  # country-specific plots
  output$country_plot_cumulative <- renderPlotly({
    country_cases_cumulative(country_reactive_db(), start_point=input$start_date, input$minimum_date)
  })
  
  # country-specific plots
  output$country_plot_cumulative_log <- renderPlotly({
    country_cases_cumulative_log(country_reactive_db(), start_point=input$start_date, input$minimum_date)
  })
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste0("MM_POLIS_", cv_today$date[1], ".csv")
    },
    content = function(file) {
      write.csv(mm_polis_raw, file, row.names = FALSE)
    }
  )
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(head(mm_polis_raw[,colSums(is.na(mm_polis_raw))<nrow(mm_polis_raw)], input$maxrows), row.names = FALSE)
    options(orig)
  })
  
  }

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")

