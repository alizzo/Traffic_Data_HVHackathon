# Load Libraries
library(tidyverse)
library(sf)
library(leaflet)
library(viridis)

# Read in the Shape File
municipalities <- read_sf("./dutchesscounty4326/muni_boundaries-polygon.shp")

# Read in the geospatial data for Dutchess County Fire & EMS Stations.
fire_ems <- read_csv("fire_ems_stations-point.csv") %>% 
  st_as_sf(coords = c("X", "Y"), 
           crs = 4326, agr = "field") 

# Read in the geospatial data for Dutchess County Police Stations.
police <- read_csv("police_stations.csv") %>% 
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

fire_ems_clean <- fire_ems %>% 
  rename(ANA = `ANA`) %>% 
  mutate(popup_label = paste(paste0('<b>City: ', MCN, '</b>'),
                             paste0('Address: ', AAD), 
                             sep = '<br/>'))
police_clean <- police %>% 
  mutate(popup_label = paste(paste0('<b>Dept.: ', DEPARTMENT, '</b>'),
                             paste0('Address: ', LOCATION), 
                             sep = '<br/>'))

# Build the Map Using Leaflet

fire_ems_clean$MCN <- factor(sample.int(5L, nrow(fire_ems_clean), TRUE))

factpal <- colorFactor(topo.colors(5), fire_ems_clean$MCN)

map <-leaflet(fire_ems) %>% 
  addTiles(group="Dutchess County") %>% 
  addPolygons(data = municipalities_clean,
              color = 'white',
              weight = 1.5,
              opacity = 1,
              fillColor = 'black',
              fillOpacity = .8,
              highlightOptions = highlightOptions(color = "#FFF1BE", 
                                                  weight = 5),
              popup = ~municipalities_label) %>% 
  addCircleMarkers(data = fire_ems_clean,
                   popup = ~popup_label,
                   stroke = F,
                   radius = 4,
                   fillColor = ~factpal(MCN),
                   fillOpacity = 1,
                   group = "Fire-EMS") %>% 
  addMarkers(data = police_clean,
                   popup = ~popup_label,
                   group = "Police") %>% 

  addLayersControl(
            baseGroups = c("Dutchess County"),
            overlayGroups = c("Fire-EMS", "Police"), 
            options=layersControlOptions(collapsed=FALSE))
map
