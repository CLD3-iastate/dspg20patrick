# install.packages("tidyverse")
# install.packages("viridis")
# install.packages("sf")
# install.packages("ggthemes")
# install.packages("RColorBrewer")
# install.packages("ggplot2")
library(tidyverse)
library(viridis)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(ggplot2)
library(tigris)
library(data.table)


######## USDA Food Desert Atlas Data Geographies - 2017 #################


#
# Tigris Work ------------------------------------------------------------------------
#

setwd("/Users/morgan/Documents/dspg/patrick county project")

usda_data <- fread("atlas_patrick_county.csv")

usda_data <- usda_data %>%
  rename(GEOID = CensusTract)

usda_data$GEOID <- as.character(usda_data$GEOID)
usda_data <- as.data.frame(usda_data)

# load in tigris county tract shape files
tract_data <- tracts(51, 141, cb = FALSE, year = 2017)

# change from sp to sf data type
tract_data <- st_as_sf(tract_data)

# I used geo_join from tigris to merge the usda_data and tract_data
# It is still in list form
usda_geo_data <- usda_data %>%
  left_join(tract_data)

usda_geo_data$geometry <- tract_data$geometry
usda_geo_data$geometry <- st_as_sf(usda_geo_data$geometry)

# USDA plots -----------------------------------------------------------------------
# LAhalfand10, lahunv10share, lakids10share, lalowi10share, lapop10share, laseniors10share, LILATracts_1And10
# var needs to be replaced with the column we care about
# Mo will fill this in with a finished example soon

plot(st_geometry(usda_geo_data))

# LAhalfand10
min_LAhalfand10 <- floor(min(usda_geo_data$LAhalfand10))
max_LAhalfand10 <- ceiling(max(usda_geo_data$LAhalfand10))
ggplot() +
  geom_sf(data = usda_geo_data, size = 0.2, aes(fill = LAhalfand10)) +
  labs(title = "Title",
       caption = "Caption") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_LAhalfand10, max_LAhalfand10), 
                        breaks = seq(min_LAhalfand10, max_LAhalfand10, length.out = 5))
# ggsave(path = "./output/usda/", device = "png", filename = "plot_LAhalfand10.png", plot = last_plot())
