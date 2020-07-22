library(tidygeocoder)
library(dplyr)
library(readxl)
library(sf)
library(leaflet)
library(disco)
library(RColorBrewer)
library(readr)


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

# Patrick Henry Community College Stuart Campus, lat = 36.647241, long = -80.270562
wifi$latitude[10] <- 36.647241
wifi$longitude[10] <- -80.270562
wifi$geo_method[10] <- "manual"

# Meadows of Dan Food Market, lat = 36.735641, long = -80.407796
groceries$latitude[6] <- 36.735641
groceries$longitude[6] <- -80.407796
groceries$geo_method[6] <- "manual"

# Country Convenience Market, lat = 36.741524, long = -80.208900
groceries$latitude[10] <- 36.741524
groceries$longitude[10] <- -80.208900
groceries$geo_method[10] <- "manual"


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
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(stroke = FALSE, fillOpacity = 1, radius = 6)


#
# Write out --------------------------------------------------
#

write.csv(groceries, "./data/working/geocode/patrick_groceries.csv")
write.csv(wifi, "./data/working/geocode/patrick_wifi.csv")

groceries <- groceries %>% select(-notes)

write_rds(groceries, "./data/web/groceries.Rds")
write_rds(wifi, "./data/web/wifi.Rds")
