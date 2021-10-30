# Load Libraries
library(tidyverse)
library(sf)
library(leaflet)
library(viridis)

# Read in the Shape File
municipalities <- read_sf("./dutchesscounty4326/muni_boundaries-polygon.shp")

# Read in the geospatial data for Dutchess County Fire & EMS Stations.
tax_info <- read_csv("Dutchess_County_Tax_Info_Filtered.csv") %>% 
  st_as_sf(coords = c("X", "Y"), 
           crs = 4326, agr = "field") 


# Read in the geospatial data for Dutchess County Police Stations.
Restaurants <- read_csv("Dutchess_County_Resturants.csv") %>% 
  st_as_sf(coords = c("X", "Y"), 
           crs = 4326, agr = "field") 

# Transform the data
capwords <- function(s) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {tolower(substring(s, 2))},
                           sep = "", collapse = " ")
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# Municipalities dataset transformation to proper projection system (4326 or WGS 84)
municipalities_clean <- municipalities %>% 
  st_transform(4326) %>% 
  mutate(municipalities_label = paste0('<b>Location:</b> ', 
                                       capwords(NAME)))

tax_info_clean <- tax_info %>% 
  rename(ANA = `ANA`) %>% 
  mutate(popup_label = paste(paste0('<b>City: ', MCN, '</b>'),
                             paste0('Address: ', AAD), 
                             sep = '<br/>'))
Restaurants_clean <- Restaurants %>% 
  mutate(popup_label = paste(paste0('<b>Dept.: ', CovertFromBusinessCodes, '</b>'),
                             paste0('Address: ', LOCATION), 
                             sep = '<br/>'))

# Build the Map Using Leaflet

tax_info_clean$MCN <- factor(sample.int(5L, nrow(tax_info_clean), TRUE))

factpal <- colorFactor(topo.colors(1), tax_info_clean$MCN)

map <-leaflet() %>% 
  addTiles(group="Dutchess County") %>% 
  addPolygons(data = municipalities_clean,
              color = 'white',
              weight = 1.5,
              opacity = 1,
              fillColor = 'black',
              fillOpacity = .2,
              highlightOptions = highlightOptions(color = "#FFF1BE", 
                                                  weight = 5),
              popup = ~municipalities_label) %>% 
  addCircleMarkers(data = tax_info_clean,
                   popup = ~popup_label,
                   stroke = F,
                   radius = 4,
                   fillColor = ~factpal(MCN),
                   fillOpacity = 1,
                   group = "All Businesses") %>% 
  addMarkers(data = Restaurants_clean,
             popup = ~popup_label,
             group = "Restaurants") %>% 
  
  addLayersControl(
    baseGroups = c("Dutchess County"),
#    overlayGroups = c("All Businesses", "Restaurants"), 
    options=layersControlOptions(collapsed=FALSE))
map
