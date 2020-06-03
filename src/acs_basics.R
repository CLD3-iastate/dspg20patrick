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


######## Pull ACS 2014/18 data for basic Patrick County sociodemographics #################


#
# API key ------------------------------------------------------------------------
#

# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")


#
# Select variables ------------------------------------------------------------------------
#

# Load all variable names
# load_variables(2018, "acs5", cache = TRUE)

# % population age 65+
# B01001_020:25 (male), B01001_044:49 (female) / B01001_001    
# % population age <=18
# B01001_003:006 (male), B01001_027:30 (female) / B01001_001
# % population Hispanic or Latino
# B03001_003 / B02001_001
# % population Black
# B02001_003 / B02001_001
# % population over age 25 without a BA degree
# B15003_002:021 / B15003_001
# % population in labor force that is unemployed
# B23025_005 / B23025_002
# % population living under 100% poverty line
# B17001_002 / B17001_001
# % population without health insurance
# (005 + 008 + 011 + 014 + 017 + 020 + 023 + 026 + 029 + 033 + 036 + 039 + 042 + 045 + 048 + 051 + 054 + 057) /  B27001_001

# Select variables
acsvars <- c(
  # age 65 +
  "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
  "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049",
  "B01001_001",
  # age <18
  "B01001_003", "B01001_004", "B01001_005", "B01001_006",
  "B01001_027", "B01001_028", "B01001_029", "B01001_030",
  # Hispanic
  "B03001_003", "B03001_001",
  # Black
  "B02001_003", "B02001_001",
  # Without BA
  "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007",
  "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013",
  "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018", "B15003_019",
  "B15003_020", "B15003_021", "B15003_001",
  # Unemployed
  "B23025_005", "B23025_002",
  # In poverty
  "B17001_002", "B17001_001",
  # Without health insurance
  "B27001_005", "B27001_008", "B27001_011", "B27001_014", "B27001_017", "B27001_020", "B27001_023",
  "B27001_026", "B27001_029", "B27001_033", "B27001_036", "B27001_039", "B27001_042", "B27001_045",
  "B27001_048", "B27001_051", "B27001_054", "B27001_057", "B27001_001"
 )


#
# Get data ------------------------------------------------------------------------
#

# Get data from 2014/18 5-year estimates for Patrick County (51141) at tract level 
data_tract <- get_acs(geography = "tract", state = 51, county = 141,
                variables = acsvars,
                year = 2018, survey = "acs5",
                cache_table = TRUE, output = "wide", geometry = TRUE,
                keep_geo_vars = TRUE)

# Get data from 2014/18 5-year estimates for Patrick County (51141) at block group level
data_bgrp <- get_acs(geography = "block group", state = 51, county = 141,
                     variables = acsvars,
                     year = 2018, survey = "acs5",
                     cache_table = TRUE, output = "wide", geometry = TRUE,
                     keep_geo_vars = TRUE)

#
# Calculate ------------------------------------------------------------------------
#

# Tract level 
acs_tract <- data_tract %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.x,
  ALAND = ALAND,
  AWATER = AWATER,
  geometry = geometry,
  age65 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
           B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E * 100,
  under18 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E +
             B01001_027E + B01001_028E + B01001_029E + B01001_030E) / B01001_001E * 100,
  hispanic = B03001_003E / B02001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
  noba = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E +
          B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
          B15003_016E + B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E) / B15003_001E * 100,
  unempl = B23025_005E / B23025_002E * 100,
  inpov = B17001_002E / B17001_001E * 100,
  nohealthins = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + B27001_023E + 
                 B27001_026E + B27001_029E + B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E +
                 B27001_048E + B27001_051E + B27001_054E + B27001_057E) / B27001_001E * 100
  )

# Block group (note: variables with estimate = 0 will have NAs in the final calculation. Disregard these
# for now and use tract-level values for plotting.)
acs_bgrp <- data_bgrp %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  BLKGRPCE = BLKGRPCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.x,
  ALAND = ALAND,
  AWATER = AWATER,
  geometry = geometry,
  age65 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
             B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E * 100,
  under18 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E +
               B01001_027E + B01001_028E + B01001_029E + B01001_030E) / B01001_001E * 100,
  hispanic = B03001_003E / B02001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
  noba = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E +
            B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
            B15003_016E + B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E) / B15003_001E * 100,
  unempl = B23025_005E / B23025_002E * 100,
  inpov = B17001_002E / B17001_001E * 100,
  nohealthins = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + B27001_023E + 
                   B27001_026E + B27001_029E + B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E +
                   B27001_048E + B27001_051E + B27001_054E + B27001_057E) / B27001_001E * 100
)


#
# Plots ------------------------------------------------------------------------
#

# Age 65 and over
min_age65 <- floor(min(acs_bgrp$age65))
max_age65 <- ceiling(max(acs_bgrp$age65))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = age65)) +
  labs(title = "Percent population age 65 and over\nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fff7ec", high = "#7F0000",
                    limits = c(min_age65, max_age65), 
                    breaks = seq(min_age65, max_age65, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_age65.png", plot = last_plot())

# Age 18 and under
min_under18 <- floor(min(acs_bgrp$under18))
max_under18 <- ceiling(max(acs_bgrp$under18))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = under18)) +
  labs(title = "Percent population age 18 and under\nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_under18, max_under18), 
                        breaks = seq(min_under18, max_under18, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_under18.png", plot = last_plot())

# Hispanic
# only at tract level
min_hispanic <- floor(min(acs_tract$hispanic))
max_hispanic <- ceiling(max(acs_tract$hispanic))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = hispanic)) +
  labs(title = "Percent population hispanic \nby Census tract, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_hispanic, max_hispanic), 
                        breaks = seq(min_hispanic, max_hispanic, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_hispanic.png", plot = last_plot())

# Black
min_black <- floor(min(acs_bgrp$black))
max_black <- ceiling(max(acs_bgrp$black))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = black)) +
  labs(title = "Percent population black \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_black, max_black), 
                        breaks = seq(min_black, max_black, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_black.png", plot = last_plot())

# Age 15 and older and without a BA
min_noba <- floor(min(acs_bgrp$noba))
max_noba <- ceiling(max(acs_bgrp$noba))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = noba)) +
  labs(title = "Percent population 25 and over without a Bachelor's \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_noba, max_noba), 
                        breaks = seq(min_noba, max_noba, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_noba.png", plot = last_plot())

# Unemployed in LF
min_unempl <- floor(min(acs_bgrp$unempl))
max_unempl <- ceiling(max(acs_bgrp$unempl))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = unempl)) +
  labs(title = "Percent population unemployed in the labor force \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_unempl, max_unempl), 
                        breaks = seq(min_unempl, max_unempl, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_unempl.png", plot = last_plot())

# At or Below 100 percent poverty level
# only at tract level
min_inpov <- floor(min(acs_tract$inpov))
max_inpov <- ceiling(max(acs_tract$inpov))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = inpov)) +
  labs(title = "Percent population under 100 percent poverty Level \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_inpov, max_inpov), 
                        breaks = seq(min_inpov, max_inpov, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_inpov.png", plot = last_plot())

# No health insurance
# only at tract level
min_nohealthins <- floor(min(acs_tract$nohealthins))
max_nohealthins <- ceiling(max(acs_tract$nohealthins))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = nohealthins)) +
  labs(title = "Percent population without health insurance \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_nohealthins, max_nohealthins), 
                        breaks = seq(min_nohealthins, max_nohealthins, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_nohealthins.png", plot = last_plot())

