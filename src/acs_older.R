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

######## Pull ACS 2014/18 data for older adult variables #################

# API key ------------------------------------------------------------------------
#

# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

# Select variables ------------------------------------------------------------------------
#

# Load all variable names
# load_variables(2018, "acs5", cache = TRUE)

#HEALTH INSURANCE
# % males 65+ without health insurance of all males 65+
# B27001_026 (M 65-74 no HI) + B27001_029 (M 75+ no HI ) / (B27001_024 (total M 65-74) + B27001_027 (total M 75+))

#% females 65+ without health insurance of all females 65+
#B27001_054 (F 65-74 no HI) + B27001_057 (F 75+ no HI) / B27001_052 (total F 65-74) + B27001_055 (total F 75+)

# % individuals 65+ without health insurance of all individuals 65+
# B27001_026 (M 65-74 no HI) + B27001_029 (M 75+ no HI ) + B27001_054 (F 65-74 no HI) + B27001_057 (F 75+ no HI) /  B27001_052 (total F 65-74) + B27001_055 (total F 75+) + (B27001_024 (total M 65-74) + B27001_027 (total M 75+))

#HARDSHIP
#% households with at least one 60+ household member receiving SNAP of all households with at least one 60+ member
#B22001_003 (At least one 60+, SNAP) / B22001_006 + B22001_003 (Total household with at least one 60+)

#% males 65+ with income below poverty (BP) level of all males 65+
# B17001_015 (M 65-74 BP) + B17001_016 (M 75+ BP) / (B17001_015 + B17001_016 + B17001_044 +B17001_045) (Total M 65+)

#% females 65+ with income below poverty level of all females 65+
#B17001_029 (F 65-74 BP) + B17001_030 (F 75+ BP) / (B17001_029 + B17001_030 + B17001_058 + B17001_059) (Total F 65+)

#% individuals 65+ with income below poverty level of all individuals 65+
#B17001_029 (F 65-74 BP) + B17001_030 (F 75+ BP) + B17001_015 (M 65-74 BP) + B17001_016 (M 75+ BP) / (B17001_015 + B17001_016 + B17001_029 + B17001_030 + B17001_044 +B17001_045 + B17001_058 + B17001_059) (Total 65+)

#HOUSEHOLDS
# sixty: % households with one or more 60+ member of all households
#B11006_002  (Households with one or more 60+) / B11006_001 (Total)

#% married couple families of all households with one or more 60+ member
#B11006_005(Married couple with one or more 60+)/ B11006_003 (Total family with 60+)

#% male householder with no wife of all households with one or more 60+ member
#B11006_006(M HH no wife 60+ present) /  B11006_003 (Total HH with 60+)

#% female householder with no husband of all households with one or more 60+ member
#B11006_007(F HH 60+ present) / B11006_003 (Total HH with 60+)

#% single (male OR female) householder with no wife/husband of all households with one or more 60+ member
#B11006_007(F HH 60+ present) + B11006_006(M HH no wife 60+ present) / B11006_003 (Total HH with 60+)

#% non family households of all households with one or more 60+ member
#B11006_008 (non-family HH 60+ present) / B11006_003 (Total HH with 60+)

#EMPLOYMENT
#% males 65+ in labor force (LF) of all males 65+
# B23001_074(M 65-69 LF) + B23001_079(M 70-74 LF) + B23001_084(M 75+ LF) / B23001_073(65-69 M) + B23001_078(70-74 M) + B23001_083(75+ M) (Total Males 65+)

# % females 65+ in labor force of all females 65+
#B23001_160(F 65-69 LF) + B23001_165(F 70-74 LF) + B23001_170(F 75+ LF) / B23001_159(F 65-69) + B23001_164(F 70 -74) + B23001_169(F 75+)

# % individuals 65+ in labor force of all individuals 65+
#B23001_160(F 65-69 LF) + B23001_165(F 70-74 LF) + B23001_170(F 75+ LF) + B23001_074(M 65-69 LF) + B23001_079(M 70-74 LF) + B23001_084(M 75+ LF) /  B23001_159(F 65-69) + B23001_164(F 70 -74) + B23001_169(F 75+) + B23001_073(65-69 M) + B23001_078(70-74 M) + B23001_083(75+ M) (All Individuals)

 # Select variables
acs_older_vars <- c(
  # HI males age 65 +
  "B27001_024","B27001_026","B27001_027", "B27001_029",
  # HI females 65+
  "B27001_052","B27001_054","B27001_055" ,"B27001_057",
  #SNAP Hardship
  "B22001_003","B22001_006",
  #BELOW POVERTY
  "B17001_015","B17001_016", "B17001_029", "B17001_030", "B17001_044", "B17001_045", "B17001_058", "B17001_059",
  #HOUSEHOLDS
  "B11006_001","B11006_002", "B11006_003","B11006_005","B11006_006", "B11006_007", "B11006_008",
  #EMPLOYMENT
  "B23001_073", "B23001_074", "B23001_078", "B23001_079", "B23001_083", "B23001_084", "B23001_159", "B23001_160", "B23001_164", "B23001_165", "B23001_169", "B23001_170"

)
#
# Get data ------------------------------------------------------------------------
#
# Get data from 2014/18 5-year estimates for Patrick County (51141) at tract level
older_data_tract <- get_acs(geography = "tract", state = 51, county = 141,
                      variables = acs_older_vars,
                      year = 2018, survey = "acs5",
                      cache_table = TRUE, output = "wide", geometry = TRUE,
                      keep_geo_vars = TRUE)

# Get data from 2014/18 5-year estimates for Patrick County (51141) at block group level
older_data_bgrp <- get_acs(geography = "block group", state = 51, county = 141,
                     variables = acs_older_vars,
                     year = 2018, survey = "acs5",
                     cache_table = TRUE, output = "wide", geometry = TRUE,
                     keep_geo_vars = TRUE)

#
# Calculate ------------------------------------------------------------------------
#
# Tract level
acs_older_tract <- older_data_tract %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.x,
  ALAND = ALAND,
  AWATER = AWATER,
  geometry = geometry,
  #HEALTH INSURANCE
  malesnohealthins = (B27001_026E + B27001_029E) / (B27001_024E + B27001_027E) * 100,
  femalesnohealthins = (B27001_054E + B27001_057E) / (B27001_052E + B27001_055E) * 100,
  allnohealthins = (B27001_026E + B27001_029E + B27001_054E + B27001_057E) / (B27001_052E + B27001_055E + B27001_024E + B27001_027E ) * 100,
  #HARDSHIPS
  snap = B22001_003E / (B22001_006E + B22001_003E) * 100,
  malesbp = (B17001_015E + B17001_016E) / (B17001_015E + B17001_016E + B17001_044E +B17001_045E) * 100,
  femalebp = (B17001_029E + B17001_030E) / (B17001_029E + B17001_030E + B17001_058E + B17001_059E) * 100,
  allbp = (B17001_029E + B17001_030E + B17001_015E + B17001_016E) / (B17001_015E + B17001_016E + B17001_029E + B17001_030E + B17001_044E +B17001_045E + B17001_058E + B17001_059E) * 100,
  #HOUSEHOLDS
  sixty = B11006_002E / B11006_001E * 100,
  married_sixty = B11006_005E / B11006_003E * 100,
  malehh_sixty = B11006_006E /B11006_003E * 100,
  femalehh_sixty = B11006_007E / B11006_003E * 100,
  allhh_sixty = B11006_007E + B11006_006E/ B11006_003E * 100,
  nofamily_sixty = B11006_008E / B11006_003E * 100,
  #EMPLOYMENT
  malelf = (B23001_074E + B23001_079E + B23001_084E) / (B23001_073E + B23001_078E + B23001_083E) * 100,
  femalelf = (B23001_160E + B23001_165E+ B23001_170E) / (B23001_159E + B23001_164E + B23001_169E) * 100,
  alllf = (B23001_160E + B23001_165E + B23001_170E + B23001_074E + B23001_079E + B23001_084E) / (B23001_159E + B23001_164E + B23001_169E + B23001_073E + B23001_078E + B23001_083E) * 100
  )

#Block group (note: variables with estimate = 0 will have NAs in the final calculation. Disregard these
# for now and use tract-level values for plotting.)
 acs_older_bgrp <- older_data_bgrp %>% transmute(
   STATEFP = STATEFP,
   COUNTYFP = COUNTYFP,
   TRACTCE = TRACTCE,
   GEOID = GEOID,
   NAME.x = NAME.x,
   NAME.y = NAME.x,
   ALAND = ALAND,
   AWATER = AWATER,
   geometry = geometry,
   #HEALTH INSURANCE
   malenohealthins = (B27001_026E + B27001_029E) / (B27001_024E + B27001_027E) * 100,
   femalenohealthins = (B27001_054E + B27001_057E) / (B27001_052E + B27001_055E) * 100,
   allnohealthins = (B27001_026E + B27001_029E + B27001_054E + B27001_057E) / (B27001_052E + B27001_055E + B27001_024E + B27001_027E ) * 100,
   #HARDSHIPS
   snap = B22001_003E / (B22001_006E + B22001_003E) * 100,
   malesbp = (B17001_015E + B17001_016E) / (B17001_015E + B17001_016E + B17001_044E +B17001_045E) * 100,
   femalebp = (B17001_029E + B17001_030E) / (B17001_029E + B17001_030E + B17001_058E + B17001_059E) * 100,
   allbp = (B17001_029E + B17001_030E + B17001_015E + B17001_016E) / (B17001_015E + B17001_016E + B17001_029E + B17001_030E + B17001_044E +B17001_045E + B17001_058E + B17001_059E) * 100,
   #HOUSEHOLDS
   sixty = B11006_002E / B11006_001E * 100,
   married_sixty = B11006_005E / B11006_003E * 100,
   malehh_sixty = B11006_006E /B11006_003E * 100,
   femalehh_sixty = B11006_007E / B11006_003E * 100,
   allhh_sixty = B11006_007E + B11006_006E/ B11006_003E * 100,
   nofamily_sixty = B11006_008E / B11006_003E * 100,
   #EMPLOYMENT
   malelf = (B23001_074E + B23001_079E + B23001_084E) / (B23001_073E + B23001_078E + B23001_083E) * 100,
   femalelf = (B23001_160E + B23001_165E+ B23001_170E) / (B23001_159E + B23001_164E + B23001_169E) * 100,
   alllf = (B23001_160E + B23001_165E + B23001_170E + B23001_074E + B23001_079E + B23001_084E) / (B23001_159E + B23001_164E + B23001_169E + B23001_073E + B23001_078E + B23001_083E) * 100
 )
 #
 # Plots ------------------------------------------------------------------------
 #


 acs_plot <- function(acs_variables, ...){
   ggplot() +
     geom_sf(data = acs_older_tract, size = 0.2, aes(fill = acs_variables)) +
     labs(title = "Percent population age 65 and over with no health insurance\nby Census tract group, 2014/18",
          caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
     theme_map() +
     theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
           legend.title = element_text(size = 11, face = "bold"),
           legend.text = element_text(size = 11),
           legend.position = "right") +
     scale_fill_continuous(name = "Percent", low = "#fff7ec", high = "#7F0000",
                           limits = c(floor(min(acs_variables)), ceiling(max(acs_variables))),
                           breaks = seq(floor(min(acs_variables)), ceiling(max(acs_variables)), length.out = 5))
   ggsave(path = "./output/acs/", device = "png", filename = paste("plot_age65_", acs_variables, ".png"), plot = last_plot())

 }



 # Age 65 and over
 min_healthins <- floor(min(acs_older_tract$allnohealthins))
 max_healthins <- ceiling(max(acs_older_tract$allnohealthins))
 ggplot() +
   geom_sf(data = acs_older_tract, size = 0.2, aes(fill = allnohealthins)) +
   labs(title = "Percent population age 65 and over with no health insurance\nby Census tract group, 2014/18",
        caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
   theme_map() +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         legend.title = element_text(size = 11, face = "bold"),
         legend.text = element_text(size = 11),
         legend.position = "right") +
   scale_fill_continuous(name = "Percent", low = "#fff7ec", high = "#7F0000",
                         limits = c(min_healthins, max_healthins),
                         breaks = seq(min_healthins, max_healthins, length.out = 5))
 ggsave(path = "./output/acs/", device = "png", filename = "plot_age65_nohi.png", plot = last_plot())
