################ ACS API ################
# use control-shift-c to uncomment lines
# install.packages("tidycensus")
library(tidycensus)
# install.packages("tidyverse")
library(tidyverse)
# install.packages("viridis")
library(viridis)

# get your key on the acs api website, activate it, and then copy it
# to find this, do a simple google search for "acs api" and there will be a button
# that says request a key, send it to your email, etc.

# installing it means you never have to retype this, I believe
census_api_key("ac2c12d089ea55018c59548c149fbe915463bebe", install = TRUE)

# readRenviron("~/.Renviron")
# Sys.getenv("CENSUS_API_KEY")

# use the get_acs fcn to receive data tables
# if you want to explore, use ?get_acs or ?tidycensus

# all variables names
vars <- load_variables(2018, "acs5", cache = TRUE)
View(vars)

# data includes all five year estimates from Patrick County's 4 Census Tracts

data <- get_acs(geography = "tract", state = 51, county = 141,
                  variables = c("B03001_003","B01001_001", "B02001_001", "B02001_003",
                                "B29001_005", "B01001_003","B01001_004", "B01001_005",
                                "B01001_006", "B01001_007", "B01001_020","B01001_021",
                                "B01001_022","B01001_023","B01001_024","B01001_025",
                                "B01001_027","B01001_028","B01001_029","B01001_030",
                                "B01001_031","B01001_044","B01001_045","B01001_046",
                                "B01001_047","B01001_048","B01001_049", "B06009_001",
                                "B06009_002", "B06009_003", "B06009_004", "B06012_002",
                                "B27011_007", "B27011_012", "B27011_017", "B06012_001", 
                                "B27003_001", "B12006_001", "B12006_006", "B12006_011",
                                "B12006_017", "B12006_022", "B12006_028", "B12006_033",
                                "B12006_039", "B12006_044", "B12006_050", "B12006_055"),
                year = 2018, survey = "acs5",
                  cache_table = TRUE, output = "wide", geometry = TRUE,
                  keep_geo_vars = TRUE)
# View(data)


# % population age 65+ - B01001_020:25 (male), B01001_044:49 (female) / B01001_001
# % population age <=18 - B01001_003:007 (male), B01001_027:31 (female) / B01001_001
# % population Hispanic or Latino - B03001_003 / B02001_001
# % population Black - B02001_003 / B02001_001
# % population over age 25 without a BA degree - 2:4 (less than BA) / B06009_001
# % population in labor force that is unemployed - B12006_006,11,17,22,28,33,39,44,50,55 / B12006_001
# % population living under 100% poverty line - B06012_002 / B06012_001
# % population without health insurance - B27011_007, B27011_012, B27011_017 (sum) / B27003_001

# rename columns that I can for the moment, they are intuitive
data <- rename(data, age_divisor = B01001_001E)
data <- rename(data, hispanic_or_latino = B03001_003E)
data <- rename(data, black = B02001_003E)
data <- rename(data, unemployed_in_lf = )
data <- rename(data, under_100_poverty = B06012_002E)
data <- rename(data, over_25 = B06009_001E)
data <- rename(data, race_divisor = B02001_001E)
data <- rename(data, poverty_divisor = B06012_001E)
data <- rename(data, health_ins_divisor = B27003_001E)
data <- rename(data, unemp_divisor = B12006_001E)

# create new variables
# no_health_ins includes employed, unemployed, and not in LF
data$no_health_ins <- data$B27011_007E + data$B27011_012E + data$B27011_017E

# all population equal to or over 65 that are male
data$over_65_male <- data$B01001_020E + data$B01001_021E + data$B01001_022E + data$B01001_023E + data$B01001_024E + data$B01001_025E

# all population equal to or over 65 that are female
data$over_65_female <- data$B01001_044E + data$B01001_045E + data$B01001_046E + data$B01001_047E + data$B01001_048E + data$B01001_049E

# all population equal to or over 65
data$over_65 <- data$over_65_male + data$over_65_female

# all population equal to or under 19 that are male
data$under_19_male <- data$B01001_003E + data$B01001_004E + data$B01001_005E + data$B01001_006E + data$B01001_007E

# all population equal to or under 19 that are female
data$under_19_female <- data$B01001_027E + data$B01001_028E + data$B01001_029E + data$B01001_030E + data$B01001_031E

# all population equal to or under 19
data$under_19 <- data$under_19_male + data$under_19_female

# population without a BA
data$no_ba <- data$B27011_007E + data$B27011_012E + data$B27011_017E

# unemployed but in LF
data$unemployed_in_lf <- data$B12006_006E + data$B12006_011E + data$B12006_017E + data$B12006_022E + data$B12006_028E + data$B12006_033E + data$B12006_039E + data$B12006_044E + data$B12006_050E + data$B12006_055E

data$no_health_ins_pc <- (data$no_health_ins)/data$health_ins_divisor
data$no_ba_pc <- data$no_ba/data$over_25
data$over_65_pc <- data$over_65/data$age_divisor
data$under_19_pc <- data$under_19/data$age_divisor
data$hispanic_or_latino_pc <- data$hispanic_or_latino/data$race_divisor
data$black_pc <- data$black/data$race_divisor
data$unemployed_in_lf_pc <- data$unemployed_in_lf/data$unemp_divisor
data$under_100_poverty_pc <- data$under_100_poverty/data$poverty_divisor
data$no_health_ins_pc <- data$no_health_ins/data$health_ins_divisor

# generate a new df with the percentages
percentage_df <- c()
percentage_df <- data[117:124]
percentage_df$census_num <- data$TRACTCE
percentage_df$county_fips <- data$COUNTYFP
percentage_df$state_fips <- data$STATEFP

View(percentage_df)