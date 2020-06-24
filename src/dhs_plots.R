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
#FIPS CODE: a vector containing the subsect fips code
fips <- c("51141", "37169", "37171", "51035", "51063", "51067", "51089")

#ZIP CODES: a vector containg the subset zipcodes
#some files do not have FIPS code, looked up the counties on ZILLOW, verify?
zips <- c(24185, 24082, 24133, 24053, 24120, 24171, 24177, 24076, 27042,
          27046, 27053, 27016, 27021, 27022, 27031, 27030, 27041, 27043,
          27047,27049, 27007, 28676, 28683, 27017, 28621, 27024,24325,
          24328,24333,24330,24343,24351,24350,24352,24105,24312,24381,
          24317,24079,24091,24380,24072, 24184,24088,24092,24055,24102,24101,
          24151,24112,24067,24176,24078,24089,24148,24165,24168 )

#Defining a function that transforms shp file to sf file
#takes in folder, file as argument to give correct path of file
shp_to_sf <- function(folder, file){
  sf::read_sf((paste("./data/original/dhs-", folder, "/", file , ".shp",
                     sep = ""))) %>%
    # only keep data in the places we want
    subset(FIPS %in% fips) %>%
    sf::st_transform('+proj=longlat +datum=WGS84')
}
# veteran shp files -------------------------------------------------
# a vector containing the subset fips codes
fips <- c("51141", "37169", "37171", "51035", "51063", "51067", "51089")

# read in the shp data as an sf files
veterans <- sf::read_sf("./data/original/dhs-veterans/veterans.shp") %>%
  # only keep data in the places we want
  subset(FIPS %in% fips) %>%
  sf::st_transform('+proj=longlat +datum=WGS84')


# leaflet plots ----------------------------------------------------------------

vet_plot <- leaflet(data = veterans) %>% # create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  addMarkers()
vet_plot

#dialysis cvs file --------------------------------------------------------------
dialysis <- #read in the csv data
  read_csv("./data/original/dhs-dialysis/kidney-dialysis.csv") %>%
  #separate comma separted coordinates from one column to two columns
  separate(CalcLocation, c("x","y"), sep = ",") %>%
  #transform data frame into a sf file, did c("y", "x") because c("x","y") = Antartica
  st_as_sf(coords = c("y", "x")) %>%
  #only keep data in desired location
    subset(Zip %in% zips)

# dialysis leaflet plots ---------------------------------------------------------
dialysis_plot <- leaflet(data = dialysis) %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  addMarkers()

#  sf::st_transform('+proj=longlat +datum=WGS84')

#emsstations sf file---------------------------------------------------------------
emsstations <- shp_to_sf("emsstations","emsstations")

#emsstations leaflet plot ---------------------------------------------------------
emsstations_plot <- leaflet(data = emsstations) %>% # create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  addMarkers()
#call plot
emsstations_plot

#hopsital--------------------------------------------------------------------
#read shp hospital file
hospitals <- sf::read_sf("./data/original/dhs-hospitals/Hospitals.shp") %>%
 # only keep data in the places we want, used zip because FIPS n.a
  subset(ZIP %in% zips) %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

#hospital leaflet plot-----------------------------------------------------------------
hospitals_plot <- leaflet(data = hospitals) %>% # create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  addMarkers()
#call plot
hospitals_plot

#local emergency----------------------------------------------------------------
#localemergency read to sf
local_emergency <- shp_to_sf("localemergency","Local_EOC")

#local emergency leaflet plot  ----------------------------------------------------
local_emergency_plot <- leaflet(data = local_emergency) %>% # create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  addMarkers()
#call plot
local_emergency_plot
