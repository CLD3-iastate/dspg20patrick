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

# traveltime ----------------------------------------------------------------------
readRenviron("~/.Renviron")
traveltime_api <- Sys.getenv("TRAVELAPI")
traveltime_id <- Sys.getenv("TRAVELID")


for(i in 1:nrow(emsstations)){
  traveltime10 <- traveltime_map(appId= traveltime_id,
                                apiKey = traveltime_api,
                                location=c(emsstations$LATITUDE[i],emsstations$LONGITUDE[i]),
                                traveltime=600,
                                type="driving",
                                departure="2020-08-07T08:00:00+01:00")
  traveltime15 <- traveltime_map(appId= traveltime_id,
                                 apiKey = traveltime_api,
                                 location=c(emsstations$LATITUDE[i],emsstations$LONGITUDE[i]),
                                 traveltime=900,
                                 type="driving",
                                 departure="2020-08-07T08:00:00+01:00")
  traveltime20 <- traveltime_map(appId= traveltime_id,
                                 apiKey = traveltime_api,
                                 location=c(emsstations$LATITUDE[i],emsstations$LONGITUDE[i]),
                                 traveltime=1200,
                                 type="driving",
                                 departure="2020-08-07T08:00:00+01:00")
  m1 = mapview(traveltime10,  col.regions = c("grey"))
  # add the second layer on top
  m1 = m1 + traveltime15 + traveltime20
  mapshot(m1, file = paste0("~/git/dspg2020patrick/output/isochrone_maps/emsmap_",i, ".png", sep = ""))
}

# osrm ------------------------------------------------------------------------

for(i in 1:nrow(emsstations)){
iso <- osrmIsochrone(loc = c(emsstations$LONGITUDE[i], emsstations$LATITUDE[i]), breaks = seq(from = 10,to = 20, by = 5))

iso@data$drive_times <- factor(paste(iso@data$min, "to", iso@data$max, "min"))
# color palette for each area
factpal <- colorFactor(rev(heat.colors(3)), iso@data$drive_times)

# draw map
m2 <- leaflet() %>% 
  setView(emsstations$LONGITUDE[i], emsstations$LATITUDE[i], zoom = 11) %>%
  addProviderTiles("CartoDB.Positron", group="Greyscale") %>% 
  addMarkers(lng = emsstations$LONGITUDE[i], emsstations$LATITUDE[i], popup = "EMS Station Isochrone") %>% 
  addPolygons(fill=TRUE, stroke=TRUE, color = "black",
              fillColor = ~factpal(iso@data$drive_times),
              weight=0.5, fillOpacity=0.2,
              data = iso, popup = iso@data$drive_times,
              group = "Drive Time") %>% 
  # Legend
  addLegend("bottomright", pal = factpal, values = iso@data$drive_time,   title = "Drive Time")
mapshot(m2, file = paste0("~/git/dspg2020patrick/output/isochrone_maps/emsmap_osrm_",i, ".png", sep = ""))
}
# mapzen --------------------------------------------------------------------

# it may cost money so i'm going to wait on this
# readRenviron("~/.Renviron")
# mapzen_api <- Sys.getenv("MAPZENAPI")

# residential coverage -------------------------------------------------------

