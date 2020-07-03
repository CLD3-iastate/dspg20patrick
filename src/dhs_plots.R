library(tidyverse)
library(sf)
library(osmdata)
library(leaflet)
library(sp)


###### DHS Leaflet Plots #####################
#FIPS CODE: a vector containing the subsect fips code
fips <- c("51141", "37169", "37171", "51035", "51063", "51067", "51089")

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
  separate(CalcLocation, c("lat","long"), sep = ",") %>%
  filter(State == "VA") %>%
  #transform data frame into a sf file, did c("y", "x") because c("x","y") = Antartica
  st_as_sf(coords = c("long","lat")) %>%
  #labeling the projection to be same as the others
  st_set_crs("+proj=longlat +datum=WGS84")



# dialysis leaflet plots ---------------------------------------------------------
dialysis_plot <- leaflet(data = dialysis) %>% #create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  #bbox() %>%
  addMarkers()

#use bbox on leaflet to create boundaries

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
  filter(STATE == "VA") %>%
 # only keep data in the places we want, used zip because FIPS n.a%
  sf::st_transform('+proj=longlat +datum=WGS84')

#hospital leaflet plot-----------------------------------------------------------------
#create boundary box for locations of interest
# search for your place of interest
# coordinates of patrick county 36.6886° N, 80.3213° W
bb <- getbb('patrick county, virginia')

hospitals_plot <- leaflet(data = hospitals) %>% # create leaflet object
  addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
  addMarkers() %>%
  fitBounds(bb[1,1], bb[2,1], bb[1,2], bb[2,2]) %>%
  addMeasure()

#add bounding box for location
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


# nursinghomes shp files -------------------------------------------------
# read shp nursinghomes fi
nursing_homes <- sf::read_sf("./data/original/dhs-nursinghomes/NursingHomes.shp") %>%
# only keep data in the places we want
subset(ZIP %in% zips) %>%
sf::st_transform('+proj=longlat +datum=WGS84')


 # leaflet plots ----------------------------------------------------------------

nursing_homes_plot <- leaflet(data = nursing_homes) %>%
# create leaflet object
addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
   addMarkers()
nursing_homes_plot

# pharmacies csv file -------------------------------------------------
#read in the csv data
pharmacies <- read_csv("./data/original/dhs-pharmacies/pharmacies.csv") %>%
#separate comma separted coordinates from one column to two columns
separate(CalcLocation, c("x","y"), sep = ",") %>%
#transform data frame into a sf file, did c("y", "x") because c("x","y") = Antartica
st_as_sf(coords = c("y", "x")) %>%
#only keep data in desired location
subset(ZIP %in% zips)

# pharmacies leaflet plots ---------------------------------------------------------
pharmacies_plot <- leaflet(data = pharmacies) %>% #create leaflet object
   addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
   addMarkers()
 pharmacies_plot

# urgentcare shp files -------------------------------------------------
# read shp nursinghomes file
urgent_care <- sf::read_sf("./data/original/dhs-urgentcare/UrgentCareFacs.shp") %>%
# only keep data in the places we want
subset(FIPS %in% fips) %>%
sf::st_transform('+proj=longlat +datum=WGS84')


# leaflet plots ----------------------------------------------------------------

urgent_care_plot <- leaflet(data = urgent_care) %>% # create leaflet object
   addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
   addMarkers()
urgent_care_plot
