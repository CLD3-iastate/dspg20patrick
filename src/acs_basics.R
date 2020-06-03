# install.packages("tidycensus")
# install.packages("tidyverse")
# install.packages("viridis")
# install.packages("sf")
library(tidycensus)
library(tidyverse)
library(viridis)
library(sf)



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
# B01001_003:006 (male), B01001_027:30 (female) / B01001_001    -----> (UNDER 18)
# % population Hispanic or Latino
# B03001_003 / B02001_001
# % population Black
# B02001_003 / B02001_001
# % population over age 25 without a BA degree
# B15003_002:021 / B15003_001            -----> (you want the general-most table)
# % population in labor force that is unemployed
# B23025_005 / B23025_002                -----> (you want the general-most table)
# % population living under 100% poverty line
# B17001_002 / B17001_001                -----> (you want the table with the general-most universe)
# % population without health insurance  -----> (you want the table with the general-most universe)
# (005 + 008 + 011 + 014 + 017 + 020 + 023 + 026 + 029 + 033 + 036 + 039 + 042 + 045 + 048 + 051 + 054 + 057) /  B27001_001


#
# Get data ------------------------------------------------------------------------
#

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


# Get data from 2014/18 5-year estimates for Patrick County (51141)
data <- get_acs(geography = "tract", state = 51, county = 141,
                variables = acsvars,
                year = 2018, survey = "acs5",
                cache_table = TRUE, output = "wide", geometry = TRUE,
                keep_geo_vars = TRUE)


#
# Calculate ------------------------------------------------------------------------
#

acsdata <- data %>% transmute(
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

# Data at the Block Level ---------------------------------------------------------
# I keep getting errors that data is not avaiable at the block level. It says I can pull using
# get_decennial but that didn't work either. I tried using "acs" and also "150" instead of blocks.
# Perhaps there is something I am missing.
data_block <- get_acs(geography = "block", state = 51, county = 141,
                variables = acsvars,
                year = 2018, survey = "acs5",
                cache_table = TRUE, output = "wide", geometry = TRUE,
                keep_geo_vars = TRUE)

# Calculate at Block Level ---------------------------------------------------------
acsdata_block <- data_block %>% transmute(
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
# Plots at Tract Level -------------------------------------------------------------
# I know there's probably an easy loop to write to create and save, so please let me know!
# I did not include legend titles because I was having trouble changing the text
# These also don't include the outlines of the individual tracts or mention that 
# they're at  the tract level

age65plot <- ggplot(acsdata, aes(fill = age65, color = age65)) +
  geom_sf()
print(age65plot +
  ggtitle("Percent of Population Aged 65 or Older") +
  theme(legend.title = element_blank()))
ggsave("age65plot.jpg", plot = last_plot(), path = "/sfs/qumulo/qhome/mes5bu/git/dspg20patrick/output")

under18plot <- ggplot(acsdata, aes(fill = under18, color = under18)) +
  geom_sf()
print(under18plot +
        ggtitle("Percent of Population 18 or Older") +
        theme(legend.title = element_blank()))
ggsave("under18plot.jpg", plot = last_plot(), path = "/sfs/qumulo/qhome/mes5bu/git/dspg20patrick/output")

hispanicplot <- ggplot(acsdata, aes(fill = hispanic, color = hispanic)) +
  geom_sf()
print(hispanicplot +
        ggtitle("Percent of Population Hispanic") +
        theme(legend.title = element_blank()))
ggsave("hispanicplot.jpg", plot = last_plot(), path = "/sfs/qumulo/qhome/mes5bu/git/dspg20patrick/output")

blackplot <- ggplot(acsdata, aes(fill = black, color = black)) +
  geom_sf()
print(blackplot + 
        ggtitle("Percent of Population Black") +
        theme(legend.title = element_blank()))
ggsave("blackplot.jpg", plot = last_plot(), path = "/sfs/qumulo/qhome/mes5bu/git/dspg20patrick/output")

nobaplot <- ggplot(acsdata, aes(fill = noba, color = noba)) +
  geom_sf()
print(nobaplot +
  ggtitle("Percent of Population Over 25 Without a Bachelor's Degree") +
  theme(legend.title = element_blank()))
ggsave("nobaplot.jpg", plot = last_plot(), path = "/sfs/qumulo/qhome/mes5bu/git/dspg20patrick/output")

unemplplot <- ggplot(acsdata, aes(fill = unempl, color = unempl)) +
  geom_sf()
print(unemplplot +
  ggtitle("Percent of Population Unemployed in the Labor Force") +
  theme(legend.title = element_blank()))
ggsave("unemplplot.jpg", plot = last_plot(), path = "/sfs/qumulo/qhome/mes5bu/git/dspg20patrick/output")

inpovplot <- ggplot(acsdata, aes(fill = inpov, color = inpov)) +
  geom_sf()
print(inpovplot +
  ggtitle("Percent of Population Under 100 Percent of the Poverty Level") +
  theme(legend.title = element_blank()))
ggsave("inpovplot.jpg", plot = last_plot(), path = "/sfs/qumulo/qhome/mes5bu/git/dspg20patrick/output")

nohealthinsplot <- ggplot(acsdata, aes(fill = nohealthins, color = nohealthins)) +
  geom_sf()
print(nohealthinsplot +
  ggtitle("Percent of Population Without Health Insurance") +
  theme(legend.title = element_blank()))
ggsave("nohealthinsplot.jpg", plot = last_plot(), path = "/sfs/qumulo/qhome/mes5bu/git/dspg20patrick/output")

# Plots at Block Level -------------------------------------------------------------
# Here lies the empty grave of what soon will be plots at the block level

# Write out ------------------------------------------------------------------------
#

# When we have a data folder.
