##################### Isochrones #####################

# devtools::install_github("tlorusso/traveltime")
# devtools::install_github("rCarto/osrm")
# install.packages("rmapzen")
# install.packages("rgdal")
library(traveltime)
library(tidyverse)
library(sf)
library(osmdata)
library(leaflet)
library(sp)
library(purrr)
library(mapview)
library(osrm)
library(rmapzen)
library(rgdal)
# webshot::install_phantomjs()

fips <- c(
  # patrick county zipcode only
  "51141"
  # , "37169", "37171", "51035", "51063", "51067", "51089"
)

#Defining a function that transforms shp file to sf file
#takes in folder, file as argument to give correct path of file
shp_to_sf <- function(folder, file){
  sf::read_sf((paste("./data/original/dhs-", folder, "/", file , ".shp",
                     sep = ""))) %>%
    # only keep data in the places we want
    subset(FIPS %in% fips) %>%
    sf::st_transform('+proj=longlat +datum=WGS84')
}

emsstations <- shp_to_sf("emsstations","emsstations")

# emsstations_plot <- leaflet(data = emsstations) %>% # create leaflet object
#   addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
#   addMarkers()
# #call plot
# emsstations_plot

# Write out for web
ems <- emsstations %>% st_transform(4269)
ems <- ems %>% select(OBJECTID, ADDRESS, CITY, STATE, ZIP, NAICSDESCR, LONGITUDE, LATITUDE, DIRECTIONS, NAME, geometry)
write_rds(ems, "./data/web/ems.Rds")

# traveltime ----------------------------------------------------------------------
readRenviron("~/.Renviron")
traveltime_api <- Sys.getenv("TRAVELAPI")
traveltime_id <- Sys.getenv("TRAVELID")

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

for(i in 1:nrow(emsstations)){
  i=9
  ems_iso8 <- traveltime_map(appId= traveltime_id,
                             apiKey = traveltime_api,
                             location=c(emsstations$LATITUDE[i],emsstations$LONGITUDE[i]),
                             traveltime=480,
                             type="driving",
                             departure="2020-08-07T08:00:00+01:00")
  saveRDS(ems_iso8, file = paste0('ems_iso_8_',i,'.RDS')) 
  ems_iso10 <- traveltime_map(appId= traveltime_id,
                              apiKey = traveltime_api,
                              location=c(emsstations$LATITUDE[i],emsstations$LONGITUDE[i]),
                              traveltime=600,
                              type="driving",
                              departure="2020-08-07T08:00:00+01:00")
  saveRDS(ems_iso10, file = paste0('ems_iso_10_',i,'.RDS')) 
  ems_iso12 <- traveltime_map(appId= traveltime_id,
                              apiKey = traveltime_api,
                              location=c(emsstations$LATITUDE[i],emsstations$LONGITUDE[i]),
                              traveltime=720,
                              type="driving",
                              departure="2020-08-07T08:00:00+01:00")
  saveRDS(ems_iso12, file = paste0('ems_iso_12_',i,'.RDS')) 
  residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
  m1 = mapview(ems_iso8, layer.name = "8 minute isochrone", col.regions = colors[1])
  m2 = mapview(ems_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
  m3 = mapview(ems_iso12, layer.name = "12 minute isochrone", col.regions = colors[3])
  # add the second layer on top
  m1 = m1 + m2 + m3 + residential
  mapshot(m1, file = paste0("~/git/dspg2020patrick/output/isochrone_maps/emsmap_",i, ".png", sep = ""))
}

# residential coverage -------------------------------------------------------

residential <- read_sf("./data/working/corelogic/residential.csv")
residential_sf <- st_as_sf(residential, coords = c("parcel_level_longitude", "parcel_level_latitude"))
st_crs(residential_sf) <- "+proj=longlat +datum=WGS84"

ems_iso8_1 <- readRDS("~/git/dspg2020patrick/data/working/isochrones/ems/ems_iso_8_1.RDS")
ems_iso10_1 <- readRDS("~/git/dspg2020patrick/data/working/isochrones/ems/ems_iso_10_1.RDS")
ems_iso12_1 <- readRDS("~/git/dspg2020patrick/data/working/isochrones/ems/ems_iso_12_1.RDS")

pp_8 <- st_intersection(residential_sf, ems_iso8_1)
pp_10 <- st_intersection(residential_sf, ems_iso10_1)
pp_12 <- st_intersection(residential_sf, ems_iso12_1)

coverage_8_1 <- nrow(pp_8)/nrow(residential_sf)
coverage_10_1 <- nrow(pp_10)/nrow(residential_sf)
coverage_12_1 <- nrow(pp_12)/nrow(residential_sf)

# osrm ------------------------------------------------------------------------

# for(i in 1:nrow(emsstations)){
# iso <- osrmIsochrone(loc = c(emsstations$LONGITUDE[i], emsstations$LATITUDE[i]), breaks = seq(from = 10,to = 20, by = 5))
# 
# iso@data$drive_times <- factor(paste(iso@data$min, "to", iso@data$max, "min"))
# # color palette for each area
# factpal <- colorFactor(rev(heat.colors(3)), iso@data$drive_times)
# 
# # draw map
# m2 <- leaflet() %>% 
#   setView(emsstations$LONGITUDE[i], emsstations$LATITUDE[i], zoom = 11) %>%
#   addProviderTiles("CartoDB.Positron", group="Greyscale") %>% 
#   addMarkers(lng = emsstations$LONGITUDE[i], emsstations$LATITUDE[i], popup = "EMS Station Isochrone") %>% 
#   addPolygons(fill=TRUE, stroke=TRUE, color = "black",
#               fillColor = ~factpal(iso@data$drive_times),
#               weight=0.5, fillOpacity=0.2,
#               data = iso, popup = iso@data$drive_times,
#               group = "Drive Time") %>% 
#   # Legend
#   addLegend("bottomright", pal = factpal, values = iso@data$drive_time,   title = "Drive Time")
# mapshot(m2, file = paste0("~/git/dspg2020patrick/output/isochrone_maps/emsmap_osrm_",i, ".png", sep = ""))
# }

