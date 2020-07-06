library(tidygeocoder)
library(dplyr)
library(readxl)
library(sf)
library(leaflet)
library(disco)
library(RColorBrewer)


#
# Read in --------------------------------------------------
#

groceries <- read_xlsx("./data/original/patrick_groceries.xlsx", col_names = TRUE, trim_ws = TRUE, progress = readxl_progress())
wifi <- read_xlsx("./data/original/patrick_wifi.xlsx", col_names = TRUE, trim_ws = TRUE, progress = readxl_progress())


#
# Geocode --------------------------------------------------
#

groceries <- groceries %>% geocode(fulladdress, lat = latitude, long = longitude, method = "cascade")
wifi <- wifi %>% geocode(fulladdress, lat = latitude, long = longitude, method = "cascade")

# Patrick County High School, lat = 36.624179, long = -80.269961
wifi$latitude[5] <- 36.624179
wifi$longitude[5] <- -80.269961
wifi$geo_method[5] <- "manual"

# Meadows of Dan Food Market, lat = 36.735641, long = -80.407796
groceries$latitude[12] <- 36.735641
groceries$longitude[12] <- -80.407796
groceries$geo_method[12] <- "manual"

# Country Convenience Market, lat = 36.741524, long = -80.208900
groceries$latitude[16] <- 36.741524
groceries$longitude[16] <- -80.208900
groceries$geo_method[16] <- "manual"


#
# Plot --------------------------------------------------
#

# Groceries
pal <- colorFactor(palette = disco(palette = "vibrant", n = 3), domain = groceries$type)
leaflet(groceries) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(stroke = FALSE, fillOpacity = 1, color = ~pal(type), radius = 4) %>%
  addLegend("bottomleft", 
            pal = pal, 
            values =  ~type,
            title = "Type", 
            opacity = 1)

# Wifi
leaflet(wifi) %>%
  addTiles() %>%
  addCircleMarkers(stroke = FALSE, fillOpacity = 1, radius = 6)


#
# Write out --------------------------------------------------
#

write.csv(groceries, "./data/working/geocode/patrick_groceries.csv", col.names = TRUE)
write.csv(wifi, "./data/working/geocode/patrick_wifi.csv", col.names = TRUE)