##################### Isochrones #####################

# devtools::install_github("tlorusso/traveltime")
library(traveltime)
library(tidyverse)
library(sf)
library(osmdata)
library(leaflet)
library(sp)
library(purrr)
library(mapview)

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

#emsstations leaflet plot ---------------------------------------------------------
emsstations_plot <- leaflet(data = emsstations) %>% # create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  addMarkers()
#call plot
emsstations_plot

# traveltime ----------------------------------------------------------------------
readRenviron("~/.Renviron")
traveltime_api <- Sys.getenv("TRAVELAPI")
traveltime_id <- Sys.getenv("TRAVELID")

traveltime5 <- traveltime_map(appId= traveltime_id,
                               apiKey = traveltime_api,
                               location=c(emsstations$LATITUDE[1],emsstations$LONGITUDE[1]),
                               traveltime=300,
                               type="driving",
                               departure="2020-08-07T08:00:00+01:00")
traveltime10 <- traveltime_map(appId= traveltime_id,
                               apiKey = traveltime_api,
                               location=c(emsstations$LATITUDE[1],emsstations$LONGITUDE[1]),
                               traveltime=600,
                               type="driving",
                               departure="2020-08-07T08:00:00+01:00")
traveltime15 <- traveltime_map(appId= traveltime_id,
                               apiKey = traveltime_api,
                               location=c(emsstations$LATITUDE[1],emsstations$LONGITUDE[1]),
                               traveltime=900,
                               type="driving",
                               departure="2020-08-07T08:00:00+01:00")

m1 = mapview(traveltime5,  col.regions = c("grey"))
# add the second layer on top
m1 + traveltime10 + traveltime15
