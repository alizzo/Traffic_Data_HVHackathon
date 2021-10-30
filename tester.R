# Load Libraries
library(tidyverse)
library(sf)
library(leaflet)
library(viridis)

data <- read_csv("NYS_Traffic_Data.csv")


# Get the max salary from data frame.
roads <-list(data$WKT)

for (pt in roads) {
  print(pt)
}



