# install.packages("tidycensus")
# install.packages("tidyverse")
# install.packages("viridis")
# install.packages("sf")
# install.packages("ggthemes")
# install.packages("RColorBrewer")
# install.packages("ggplot2")
library(tidycensus)
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

setwd("~/git/dspg2020patrick")
usda_data <- fread("atlas_patrick_county.csv")

usda_data <- usda_data %>%
  rename(GEOID = CensusTract)

# load in tigris county tract shape files
tract_data <- tracts(51, 141, cb = FALSE, year = 2017)

# I used geo_join from tigris to merge the usda_data and tract_data
# It is still in list form
geo_data <- geo_join(tract_data, usda_data, by = "GEOID")
