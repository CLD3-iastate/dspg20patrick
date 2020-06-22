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

# shp files -------------------------------------------------
veterans <- st_read("./data/original/dhs-veterans/veterans.shp")
fips <- c("51141", "37169", "37171", "51035", "51063", "51067", "51089")
veterans_fips <- subset(veterans, FIPS %in% fips)
va_counties <- counties("Virginia", cb = T)

points <- veterans_fips %>% 
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), # columns with geometry
               crs = 4326)

# leaflet plots ---------------------------------------------

lplot <- leaflet::leaflet(data = points) %>% # create leaflet object
  leaflet::addTiles() %>% # add basemap
  leaflet::addMarkers()


