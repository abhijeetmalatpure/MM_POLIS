# get shapefiles (download shapefiles [here][1] : http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_5m.zip )

usgeo <- st_read("input_data/cb_2014_us_county_5m/cb_2014_us_county_5m.shp") %>%
  mutate(fips = as.numeric(paste0(STATEFP, COUNTYFP)))


### alternatively, this code *should* allow you download data ### 
### directly, but somethings slightly wrong. I'd love to know what. ####
# temp <- tempfile()
# download.file("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_5m.zip",temp)
# data <- st_read(unz(temp, "cb_2014_us_county_5m.shp"))
# unlink(temp)
########################################################

# create fake data
polis <- read_excel("input_data/DataFile_MultipleMyeloma_Polis.xlsx", sheet = "DataFile_MultipleMyeloma") %>% 
  dplyr::select(c("County FIPS at Diagnosis", "Date of Diagnosis", "Age at Dignosis", "Diagnostic confirmation", "Chemotherapy"))

colnames(polis)[1] <- 'fips'
polis$fips <- as.double(polis$fips)

polis$yod <- format(polis$`Date of Diagnosis`, format = "%Y")
polis$yod <- as.numeric(polis$yod)
# example <- data.frame(fips = rep(as.numeric(c("18003", "18141", "18167", "18075", "18179", "18055", "18103", "18097", "18089", "18163"), 4)),
#                       year = c(rep(1990, 10), rep(1991, 10), rep(1992, 10), rep(1993, 10)),
#                       life = sample(1:100, 40, replace=TRUE),
#                       income = sample(8000:1000000, 40, replace=TRUE),
#                       pop = sample(80000:1000000, 40, replace=TRUE))
# join fake data with shapefiles
example <- st_as_sf(polis %>%
                      left_join(usgeo))
# drop layers (not sure why, but won't work without this)
example$geometry <- st_zm(example$geometry, drop = T, what = "ZM")
# filter for a single year (which I don't want to have to do)
example <- example %>% filter(year == 2011)
# change projection
example <- sf::st_transform(example, "+proj=longlat +datum=WGS84")


# create popups
yodpopup <- paste0("County: ", example$NAME, ", Avg Year of diagnosis = ", example$yod)
agepopup <- paste0("County: ", example$NAME, ", Avg Age = ", example$`Age at Dignosis`)
diagpopup <- paste0("County: ", example$NAME, ", Avg Diagnostic confirmation = ", example$`Diagnostic confirmation`)
chempopup <- paste0("County: ", example$NAME, ", Avg chemotherapy = ", example$`Chemotherapy`)

# create color palettes
yodPalette <- colorNumeric(palette = "Oranges", domain=example$yod)
agePalette <- colorNumeric(palette = "Reds", domain=example$`Age at Dignosis`)
diagPalette <- colorNumeric(palette = "Blues", domain=example$`Diagnostic confirmation`)
chemPalette <- colorNumeric(palette = "Purples", domain=example$`Chemotherapy`)


# # create popups
# incomepopup <- paste0("County: ", example$NAME, ", avg income = $", example$income)
# poppopup <- paste0("County: ", example$NAME, ", avg pop = ", example$pop)
# yearpopup <- paste0("County: ", example$NAME, ", avg year = ", example$year)
# lifepopup <- paste0("County: ", example$NAME, ", avg life expectancy = ", example$life)
# 
# # create color palettes
# yearPalette <- colorNumeric(palette = "Blues", domain=example$year)
# lifePalette <- colorNumeric(palette = "Purples", domain=example$life)
# incomePalette <- colorNumeric(palette = "Reds", domain=example$income)
# popPalette <- colorNumeric(palette = "Oranges", domain=example$pop)

# create map
leaflet(example) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=FALSE,
              smoothFactor = 0.2,
              fillOpacity = .8,
              popup = agepopup,
              color = ~agePalette(example$`Age at Dignosis`),
              group = "Age"
  ) %>% 
  
  addPolygons(stroke=FALSE,
              smoothFactor = 0.2,
              fillOpacity = .8,
              popup = diagpopup,
              color = ~diagPalette(example$`Diagnostic confirmation`),
              group = "Diagnostic confirmation"
  ) %>%
  
  addPolygons(stroke=FALSE,
              smoothFactor = 0.2,
              fillOpacity = .8,
              popup = chempopup,
              color = ~chemPalette(example$Chemotherapy),
              group = "Chemotherapy"
  ) %>%
  
  addPolygons(stroke=FALSE,
              smoothFactor = 0.2,
              fillOpacity = .8,
              popup = yodpopup,
              color = ~yodPalette(example$yod),
              group = "Year of Diagnosis"
  ) %>%
  
  addLayersControl(
    baseGroups=c("Age", "Diagnostic confirmation", "Chemotherapy", "Year of Diagnosis"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  )