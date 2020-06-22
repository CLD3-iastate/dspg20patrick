library(tidyverse)
library(viridis)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(ggplot2)
library(tigris)
library(data.table)
library(leaflet)

##################### DHS Leaflet Plots #####################

# veteran shp files -------------------------------------------------
# a vector containing the subset fips codes
fips <- c("51141", "37169", "37171", "51035", "51063", "51067", "51089")

# read in the shp data as an sf files
veterans <- sf::read_sf("./data/original/dhs-veterans/veterans.shp") %>%
  # only keep data in the places we want
  subset(FIPS %in% fips) %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

# leaflet plots ---------------------------------------------

vet_plot <- leaflet(data = veterans) %>% # create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  addMarkers()
vet_plot

