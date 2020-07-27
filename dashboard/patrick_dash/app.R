library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(mapview)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(shinycssloaders)
library(readxl)
library(tidygeocoder)
library(disco)
library(readr)
library(stringr)
library(tigris)

# readRenviron("~/.Renviron")
# shinyname <- Sys.getenv("SHINYNAME")
# shinytoken <- Sys.getenv("SHINYTOKEN")
# shinysecret <- Sys.getenv("SHINYSECRET")
# 
# rsconnect::setAccountInfo(name=shinyname,
#                           token=shinytoken,
#                           secret=shinysecret)

prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff',spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

# data -----------------------------------------------------------
socdem_block <- readRDS("data/socdem_block.Rds")
socdem_block <- st_transform(socdem_block, '+proj=longlat +datum=WGS84')

socdem_tract <- readRDS("data/socdem_tract.Rds")
socdem_tract <- st_transform(socdem_tract, '+proj=longlat +datum=WGS84')

connectivity <- readRDS("data/connectivity.Rds")
connectivity <- st_transform(connectivity, '+proj=longlat +datum=WGS84')

ems <- readRDS("data/ems.Rds")
ems <- st_transform(ems, '+proj=longlat +datum=WGS84')

groceries <- readRDS("data/groceries.Rds")
groceries <- st_as_sf(groceries, coords = c("longitude", "latitude"))
st_crs(groceries) <- "+proj=longlat +datum=WGS84"
groceries <- st_transform(groceries, '+proj=longlat +datum=WGS84')
groceries <- subset(groceries, type == "farmers market" | type == "supermarket")

otherfood <- readRDS("data/otherfood.Rds")
otherfood <- st_as_sf(otherfood, coords = c("longitude", "latitude"))
st_crs(otherfood) <- "+proj=longlat +datum=WGS84"
otherfood <- st_transform(otherfood, '+proj=longlat +datum=WGS84')

usda <- readRDS("data/usda.Rds")
usda <- st_transform(usda, '+proj=longlat +datum=WGS84')

wifi <- readRDS("data/wifi.Rds")
wifi <- st_as_sf(wifi, coords = c("longitude", "latitude"))
st_crs(wifi) <- "+proj=longlat +datum=WGS84"
wifi <- st_transform(wifi, '+proj=longlat +datum=WGS84')

olderadults <- readRDS("data/olderadults.Rds")
olderadults <- st_transform(olderadults, '+proj=longlat +datum=WGS84')

residential <- readRDS("data/residential.Rds")
residential <- st_as_sf(residential, coords = c("longitude", "latitude"))
st_crs(residential) <- "+proj=longlat +datum=WGS84"
residential <- st_transform(residential, '+proj=longlat +datum=WGS84')

measures_table <- read_excel("data/Measures.xlsx")


# user -------------------------------------------------------------
ui <- navbarPage("Patrick County Dashboard",
                          theme = shinytheme("cosmo"),
                          tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
  # main -----------------------------------------------------------
                          tabPanel("Main", value = "main",
                                   mainPanel(
                                     div(
                                     h1("Addressing Barriers to Health Care Access in Patrick County, Virginia"),
                                     br(),
                                     h2("Barriers to Health Care", style=paste0("color:",navBarBlue)),
                                     p("Rural counties face challenges in providing health care access to its residents given few health facilities available, lack of broadband infrastructure that limits providing telemedicine access or communicating health information, and individual-level inequalities that pose barriers to health care access and use. Further, identifying areas of high need or potential solutions may be difficult for rural areas without adequate resources to acquire, analyze, and interpret relevant data. This University of Virginia Biocomplexity Institute Data Science for Public Good (DSPG) project aimed to build local capacity, leverage social and data science to address current and future resident well-being, and enhance data-driven decision making about rural health in Patrick County, Virginia."),
                                     div(),
                                     h2("Community Shortages", style=paste0("color:",navBarBlue,";text-align:left;")),
                                     p("Patrick County is a rural area in Virginia’s Central Piedmont area, bordering North Carolina, with a declining population of approximately 17,600 people. Like many other rural areas in the United States, Patrick County is facing challenges in addressing its residents’ health and quality of life needs. The county’s doctor to patient ratios of 3,530 to 1 for primary care providers, 8,840 to 1 for dentists, and 2,520 to 1 for mental health providers are 3- to 8-times higher than statewide, and the county’s only hospital closed in 2017. At the same time, the median income for Patrick County residents is $42,900, 46% of children living in the county are eligible for free or reduced-price school lunch, and 12% of residents are food insecure. "),
                                    div(),
                                    h2("Data Science for the Public Good", style=paste0("color:",navBarBlue,";text-align:left;")),
                                    div(),
                                    column(4,
                                      div(
                                      img(src='https://www.dropbox.com/s/e4ubv2drscr91ji/DSPG3StateLogo.jpg?raw=1',style='max-height:40vh;width:100%'),
                                      style='text-align:left')),
                                    column(8,
                                    div(
                                    p("Our DSPG research team worked closely with Patrick County Extension Office, Virginia Department of Health, and Healthy Patrick County coalition stakeholders to identify the county’s priority challenges in the area of health. The research team reviewed a prior community health assessment, situation analysis, relevant funding applications, and held a listening meeting with extension professionals and stakeholders to identify these challenges. During a listening session, lack of data on health care access, food access as related to diabetes and heart disease prevalence, older adult health, and digital connectivity that would facilitate access to telemedicine emerged as key problems where providing actionable insights could address barriers to Patrick County residents’ health"),
                                    div(),
                                    p("In the past three months, our team implemented the data science framework—identified, acquired, profiled, and used publicly available data to provide actionable information in each of the four priority areas. We constructed isochrones (areas of equal travel time) of residents’ grocery store and farmers’ market access, identifying food deserts in the county that could benefit from programs facilitating access to produce. We produced census block group-level maps of computing device and internet availability, and provided maps free wi-fi hotspot access isochrones; this equips extension agents with knowledge on how best to reach their constituents, as well as identifies internet gaps that suggest where new wi-fi hotspots could be optimally placed to provide internet access to more residents. Further, we used emergency medical service (EMS) station locations to examine county coverage and map residential properties that are difficult to reach in standard EMS travel time thresholds. We assembled data on older adults in the county to supplement EMS access information, identifying areas where providing telehealth or travelling preventive care services may be particularly important. Finally, we compiled our findings on a dashboard that allows extension professionals, stakeholders, and other users to explore all features interactively. "),
                                    style='text-align:left')),
                                    div(),
                                    h2("Dashboard Aims", style=paste0("color:",navBarBlue,";text-align:left;")),
                                    
                                    p("Our dashboard is aimed at Patrick County extension professionals and the communities they serve. Information available through the interface helps extension agents identify areas where residents may not have access to internet, or areas with a high smartphone ownership share, suggesting what channels agents may want to use to disseminate health-related information most effectively. Information on older adult populations and grocery store access can help extension agents better understand where underserved populations live and how to advocate on their behalf. Our second audience group are local health departments seeking data insights to inform their decision-making. For local health department stakeholders, identifying broadband access gaps that limit access to telemedicine, grocery store access gaps, and areas with high proportions of older adults with independent living difficulty can suggest optimal locations for placing free wi-fi hotspots, providing grocery delivery services, devising mobile health unit routes, or can inform other solutions that would benefit a broad population base. Finally, our project may be of interest to state government representatives in the Virginia Department of Health and the State Office of Rural Health who may need small or rural area-specific insights that Centers for Disease Control and other county-level datasets cannot provide. ")
                                     ),style="text-align:left;width:100%")
                                   
                          ),
                          
  # socio -----------------------------------------------------------
                          tabPanel("Sociodemographics", value = "socio",
                                   mainPanel(
                                     h1("Sociodemographics of Patrick County"),
                                     br(),
                                     p("Patrick County is a rural area in Virginia’s Central Piedmont area, bordering North Carolina, with a population of approximately 17,600 people. Like many other rural areas in the United States, Patrick County is facing challenges in addressing its residents’ health and quality of life needs. Our research team worked closely with Patrick County Extension Office, Virginia Department of Health, and Healthy Patrick County coalition stakeholders to identify the county’s priority challenges. Lack of data on health care access, food access as related to diabetes and heart disease prevalence, older adult health, and digital connectivity that would facilitate access to telemedicine emerged as key issues. We addressed each topic, providing Patrick County with actionable insights that can inform decision-making to improve their residents’ quality of life. "),
                                     div(),
                                     p("Before delving into each issue, we examined Patrick County population sociodemographic and socioeconomic characteristics to better understand the residents that the county serves. We retrieved American Community Survey (ACS) data to calculate this information at census block group and census tract levels. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets. We used the most recently available 5-year estimates from 2014/18 to compute percent Patrick County residents in a given block group or tract by age, race, ethnicity, employment, health insurance coverage, and other relevant characteristics. "),
                                     br(),
                                     selectInput("sociodrop", "Variables", choices = c(
                                       "65 and Older" = "age65",
                                       "18 and Younger" = "under18",
                                       "Black" = "black",
                                       "Hispanic" = "hispanic",
                                       "No Bachelor's Deegree" = "noba",
                                       "Unemployed" = "unempl",
                                       "Without Health Insurance" = "nohealthins2",
                                       "Private Insurance" = "privateins",
                                       "Public Insurance" = "publicins",
                                       "Under 100 percent of the Poverty Line" = "inpov",
                                       "Receiving Snap Benefits or Public Assistance" = "snap")
                                     ),
                                     withSpinner(leafletOutput("socioplot"))
                                   )
                          ),
                          
  # older -----------------------------------------------------------
                          tabPanel("Older Adult Well-Being", value = "older",
                                   mainPanel(
                                     h1("Older Adult Well-Being"),
                                     h3("Individual Level Information"),
                                     br(),
                                     p("The US population is aging, and in Patrick County, over 30% of residents are older adults aged 65 years and over. This represents more than 5,000 individuals with varying health conditions that may benefit from locally accessible health care and social services resources. However, access to health care resources is limited in rural areas, particularly for older adults in need of assistance with activities of daily life.  "),
                                     div(),
                                     p("To help Patrick County better understand their older adult population, we used American Community Survey (ACS) data and obtained census tract level information for the age group. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets with population sociodemographic and socioeconomic characteristics. We used the most recently available 5-year estimates from 2014/18 to calculate the percentage of the Patrick County older adults with different types of disability, as well as provided information on their living arrangements and socioeconomic condition. We provided all information at census tract level and by gender.  "),
                                     div(),
                                     p("These insights on the health and socioeconomic status of older adults in Patrick County can assist the county in identifying areas of high need for health care resources that will reach older adults. "),
                                     br(),
                                     
                                     selectInput("olddrop", "Individual Variables", choices = c(
                                       "Vision Difficulty" = "visdiff",
                                       "Ambulatory Difficulty" = "ambdiff",
                                       "Self-Care Difficulty" = "carediff",
                                       "Cognitive Difficulty" = "cogdiff",
                                       "Independent Living Difficulty" = "ildiff",
                                       "Any Disability" = "disab",
                                       "Below 100 percent of the Poverty Line" = "inpov",
                                       "Labor Force" = "labfor")
                                     ),
                                     selectInput("oldspecdrop", "Specifications for Individual Variables", choices = c(
                                       "Total",
                                       "Female" = "_f",
                                       "Male" = "_m")
                                     ),
                                     withSpinner(leafletOutput("oldplot")),
                                     h3("Household Level Information"),
                                     br(),
                                     selectInput("hhdrop", "Household Variables", choices = c(
                                       "Married Couple Households with one or more 60+ member" = "hhsixty_married",
                                       "Households with one or more 60+ members" = "hhsixty_total",
                                       "Single (no partner present) households with one or more 60+ member" = "hhsixty_nonfam",
                                       "Households with one or more 60+ members that are Male" = "hhsixty_mhh",
                                       "Households with one or more 60+ members that are Female" = "hhsixty_fhh" # ,
                                       #"Households with one or more 60+ household members receiving SNAP" = "snap"
                                       )
                                     ),
                                     withSpinner(leafletOutput("householdplot"))
                                   )
                          ),
                          
  # wifi-----------------------------------------------------------
                          tabPanel("Connectivity", value =  "connectivity",
                                   mainPanel(
                                     h1("Connectivity"),
                                     h3("Device and Internet Access"),
                                     br(),
                                     p("Internet connection and computing devices are crucial for access to health information, resources, and participation in online health-related services like telemedicine. Rural areas frequently lack broadband access, experience low internet speeds, pay higher subscription prices, have fewer internet providers available than urban areas. It is crucial to consider digital connectivity in improving health care access. We examined digital connectivity in Patrick County in two ways to provide the county with insights on where increasing connectivity would facilitate communicating health information and improve online health service access. "),
                                     div(),
                                     p("We first examined access to computing devices and internet connection types in Patrick County. We used American Community Survey (ACS) data to obtain this information at census block group level. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets containing information on population sociodemographic and socioeconomic characteristics. We used the most recently available 5-year estimates from 2014/18 to calculate the percentage of the Patrick County residents with access to various technologies like smartphones, broadband internet, and computers by block group. "),
                                     br(),
                                     selectInput("devicedrop", "Connectivity Variables", choices = c(
                                       "No Computer" = "nocomputer",
                                       "Laptop Ownership" = "laptop",
                                       "Smartphone Ownership" = "smartphone",
                                       "Tablet Ownership" = "tablet",
                                       "Without Internet" = "nointernet",
                                       "Satellite Internet" = "satellite",
                                       "Cellular Internet" = "cellular",
                                       "Broadband Internet" = "broadband")
                                     ),
                                     withSpinner(leafletOutput("deviceplot")),
                                     h3("Wi-Fi Hotspot Access"),
                                     br(),
                                     p("To understand internet access at a more granular level, we then examined access to free wi-fi hotspots in the county. We obtained wi-fi hotspot locations using the publicly available Virginia Tech and CommonwealthConnect hotspot map. CommonwealthConnect aims to highlight areas where people can connect to the internet for free, decreasing constraints placed on families that do not have internet access at home. We retrieved free internet locations in Patrick County from the data. We extracted locations of Patrick County residential properties from 2019 CoreLogic, a proprietary dataset for US real estate that includes information on building characteristics. Finally, we used the TravelTime Application Programming Interface (API) to calculate 10- and 15-minute car travel time isochrones—areas of equal travel time given a departure time and mode of transportation—from wi-fi hotspots. TravelTime API aggregates data from Open Street Maps, transport timetables and speed profiles to generate isochrones. Isochrones allowed us to identify hotspot gaps, or clusters of residential properties that cannot reach a free hotspot location within a selected travel time range.  "),
                                     div(),
                                     p("This information equips extension agents with knowledge on how best to reach their constituents, as well as identifies internet gaps that suggest where new wi-fi hotspots could be optimally placed to provide internet access to more residents. "),
                                     br(),
                                     selectInput("wifidrop", "Free Wifi Locations", choices = c(
                                       "Meadows of Dan Elementary School",
                                       "Woolwine Elementary School",
                                       "Patrick Springs Primary School",
                                       "Blue Ridge Elementary School",
                                       "Patrick County High School",
                                       "Stuart Elementary School",
                                       "Patrick County Branch Library",
                                       "Hardin Reynolds Memorial School",
                                       "Stuart Baptist Church",                        
                                       "Patrick Henry Community College Stuart Campus")),
                                     withSpinner(leafletOutput("wifiplot")),
                                     h3("TravelTime Coverage"),
                                     br(),
                                     withSpinner(DTOutput("wifitable"))
                                   )
                          ),
  # ems -----------------------------------------------------------
                          tabPanel("Health Care Access", value ="ems",
                                   #sidebarLayout(
                                   #sidebarPanel(
                                   #),
                                   mainPanel(
                                     h1("Health Care Access"),
                                     h3("Emergency Medical Service Locations"),
                                     br(),
                                     p("Access to health care resources in rural areas is limited by a lack of transportation and a shortage of healthcare professionals. Compared to their urban counterparts, rural residents must travel farther to obtain both preventive and specialty care. Patrick County’s general practitioner, dentist, and mental health provider to patient ratios fall below state averages, and the county recently experienced the closure of its only hospital. Its residents often rely on emergency medical services (EMS) stations to obtain care and transportation to other health care facilities.  "),
                                     div(),
                                     p("In order to better understand health service access limitations in the county, we examined residents’ access to EMS stations. We obtained EMS locations using Homeland Infrastructure Foundation-Level Data (HIFLD) collected by the Department of Homeland Security. HIFLD is a public source dataset with information on a range of facilities; we used the data to retrieve locations of EMS stations at latitude and longitude level. We extracted locations of Patrick County residential properties from 2019 CoreLogic, a proprietary dataset for US real estate that includes information on building characteristics. Finally, we used the TravelTime Application Programming Interface (API) to calculate 8-, 10-, and 12- minute car travel time isochrones—areas of equal travel time given a departure time and mode of transportation—from EMS stations. TravelTime API aggregates data from Open Street Maps, transport timetables and speed profiles to generate isochrones. Isochrones allowed us to identify EMS coverage gaps, or clusters of residential properties that cannot be reached from an EMS location within a selected travel time range. We selected 8-, 10-, and 12-minute thresholds as EMS are expected to reach distressed individuals within 8 minutes. However, this threshold is frequently exceeded by 20% to 40% in rural areas. "),
                                     br(),
                                     selectInput("emsdrop", "EMS Locations", choices = c(
                                       "Stuart Volunteer Fire Department" = "STUART VOLUNTEER FIRE DEPARTMENT",
                                      "Moorefield Store Volunteer Fire Department" = "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT",                                                         
                                       "Blue Ridge Volunteer Rescue Squad" = "BLUE RIDGE VOLUNTEER RESCUE SQUAD",                                                                   
                                       "Vesta Rescue Squad" = "VESTA RESCUE SQUAD",                                                                                           
                                      "Ararat Rescue Squad"  ="ARARAT RESCUE SQUAD",                                                                                          
                                      "Five Forks Volunteer Fire and Rescue Station 1" = "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS",
                                      "Five Forks Volunteer Fire and Rescue Station 2"= "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2",
                                      "Jeb Stuart Rescue Squad" = "JEB STUART RESCUE SQUAD",                                                                                      
                                      "Smith River Rescue Squad" = "SMITH RIVER RESCUE SQUAD"                                                                                     
                                     )),
                                     withSpinner(leafletOutput("emsplot")),
                                     h3("TravelTime Coverage"),
                                     br(),
                                     withSpinner(DTOutput("emstable"))
                                   )
                                   #)
                          ),
                          
  # food -----------------------------------------------------------
                          
                          tabPanel("Food Access", value =  "food",
                                   mainPanel(
                                     h1("Food Access"),
                                     h3("USDA Data Explorer"),
                                     br(),
                                     p("Social determinants of health shape food access, a key factor in negative health outcomes. Rural area residents frequently face difficulties in accessing healthy and nutritious food, and experience high rates of chronic illnesses like heart disease and diabetes, resulting in higher mortality rates and lower life expectancy compared to urban areas. Facilitating  access to nutritional and high-quality foods can lead to decreases in chronic disease prevalence. Many Patrick County residents suffer from conditions like diabetes and obesity, and providing healthy food may support disease prevention. We used two approaches to give Patrick County actionable information on their residents’ food access that can inform county efforts to provide equitable food access for all. "),
                                     div(),
                                     p("First, we examined food access at multiple distance thresholds and by both age and socioeconomic status. We used the 2017 United State Department of Agriculture (USDA) Food Access Research Atlas, a central database created by the Economic Research Service that provides information on access indicators at census tract level. The data allows individuals to understand food access in communities based on sociodemographic and socioeconomic factors. We developed tract-level maps that identify Patrick County tracts where residents may have difficulty accessing nutritious foods, and highlight areas where this is the case for particularly vulnerable groups like low-income individuals and older adults. "),
                                     br(),
                                     selectInput("usdadrop", "USDA Variables", choices = c(
                                       "Low Vehicle Access at 1 Mile" = "lahunv1share",
                                       # "Low Vehicle Access at 10 Miles" = "lahunv10share",
                                       "Low Food Access for Children at 1 Mile" = "lakids1share",
                                       "Low Food Access for Children at 10 Miles" = "lakids10share",
                                       "Low Food Access for Low Income Population at 1 Mile" = "lalowi1share",
                                       "Low Food Access for Low Income Population at 10 Miles" = "lalowi10share",
                                       "Low Food Access Population at 1 Mile" = "lapop1share",  
                                       "Low Food Access Population at 10 Miles" = "lapop10share",
                                       "Low Food Access Seniors at 1 Mile" = "laseniors1share",
                                       "Low Food Access Seniors at 10 Miles" = "laseniors10share")
                                     ),
                                     withSpinner(leafletOutput("usdaplot")),
                                     h3("Grocery and Farmers Market Access"),
                                     br(),
                                     p("Second, to better understand how residents must travel to obtain food, we constructed isochrones—shapes covering places within reach in the same timeframe given a start location and a mode of transportation—from Patrick County residential properties to locations of grocery stores, convenience stores, and farmers’ markets. We used Google Maps, a comprehensive web mapping service, to identify these locations at latitude and longitude level. We extracted locations of Patrick County residential properties from 2019 CoreLogic, a proprietary dataset for US real estate that includes information on building characteristics. Finally, we used the TravelTime Application Programming Interface (API) to calculate 10- and 15-minute car travel times from grocery locations. TravelTime API aggregates data from Open Street Maps, transport timetables and speed profiles to generate isochrones. This allowed us to identify food deserts, or clusters of properties that cannot reach a location with healthy food within a selected travel time range.  "),
                                     div(),
                                     p("These areas in the county could benefit from programs facilitating access to produce. "),
                                     br(),
                                     selectInput("grocdrop", "Grocery Locations", choices = c(
                                       "Mountain Meadow Farm and Craft Market",
                                       "Lowes Foods of Stuart",
                                       "Patrick County Local Farmers Market",
                                       "Stuart Farmers Market",                
                                       "W & W Produce",
                                       "Walmart Supercenter",
                                       "Poor Farmers Farm")),
                                     withSpinner(leafletOutput("grocplot")),
                                     h3("TravelTime Coverage"),
                                     br(),
                                     withSpinner(DTOutput("groctable")),
                                     h3("Other Food Access"),
                                     br(),
                                     withSpinner(leafletOutput("othermap"))
                                   )
                         ),
  # data -----------------------------------------------------------
          tabPanel("Data and Measures", value = "data",
                   mainPanel(
                     h1("Data and Measures"),
                     br(),
                     p("paragraph about data."),
                     br(),
                     selectInput("topic", "Data Topics", choices = c(
                       "All",
                       "Connectivity",
                       "Demographics",
                       "Food Access",
                       "Health",
                       "Older Adults")
                   ),
                   withSpinner(DTOutput("datatable"))
          )
        ),

  # contact -----------------------------------------------------------
          tabPanel("Contact", value = "contact",
                   mainPanel(
                     h1("Contact"),
                     br(),
                     p("This is a paragraph about contacting us")
                   )
          ),
  inverse = T
    )



# server -----------------------------------------------------------
server <- function(input, output, session) {

  # socio plots: done -----------------------------------------------------
  
  var <- reactive({
    input$sociodrop
  })
  #age65
  output$socioplot <- renderLeaflet({
    if(var() == "age65") {
      
    pal <- colorQuantile("Blues", domain = socdem_block$age65, probs = seq(0, 1, length = 6), right = TRUE)

    labels <- lapply(
      paste("<strong>Area: </strong>",
            socdem_block$NAME.y,
            "<br />",
            "<strong>% population with public assistance or SNAP benefits</strong>",
            round(socdem_block$age65, 2)),
      htmltools::HTML
    )
    
    leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(socdem_block$age65), 
                  fillOpacity = 0.6, 
                  stroke = FALSE,
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      # addMarkers(data = residential) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~(socdem_block$age65),
                title = "Percent by<br>Quintile Group",
                opacity = 0.6,
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
    #under18
    }else if(var() == "under18"){
      pal <- colorQuantile("Blues", domain = socdem_block$under18, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% population with public assistance or SNAP benefits</strong>",
              round(socdem_block$under18, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(socdem_block$under18), 
                    fillOpacity = 0.6, 
                    stroke = FALSE,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        # addMarkers(data = residential) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$under18),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "black"){
      pal <- colorQuantile("Blues", domain = socdem_block$black, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% population with public assistance or SNAP benefits</strong>",
              round(socdem_block$black, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(socdem_block$black), 
                    fillOpacity = 0.6, 
                    stroke = FALSE,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        # addMarkers(data = residential) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$black),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "noba"){
      pal <- colorQuantile("Blues", domain = socdem_block$noba, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% population with public assistance or SNAP benefits</strong>",
              round(socdem_block$noba, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(socdem_block$noba), 
                    fillOpacity = 0.6, 
                    stroke = FALSE,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        # addMarkers(data = residential) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$noba),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "unempl"){
      pal <- colorQuantile("Blues", domain = socdem_block$unempl, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% population with public assistance or SNAP benefits</strong>",
              round(socdem_block$unempl, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(socdem_block$unempl), 
                    fillOpacity = 0.6, 
                    stroke = FALSE,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        # addMarkers(data = residential) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$unempl),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "nohealthins2"){
      pal <- colorQuantile("Blues", domain = socdem_block$nohealthins2, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% population with public assistance or SNAP benefits</strong>",
              round(socdem_block$nohealthins2, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(socdem_block$nohealthins2), 
                    fillOpacity = 0.6, 
                    stroke = FALSE,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        # addMarkers(data = residential) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$nohealthins2),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "snap"){
      pal <- colorQuantile("Blues", domain = socdem_block$snap, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% population with public assistance or SNAP benefits</strong>",
              round(socdem_block$snap, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(socdem_block$snap), 
                    fillOpacity = 0.6, 
                    stroke = FALSE,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        # addMarkers(data = residential) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$snap),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "inpov"){
      pal <- colorQuantile("Blues", domain = socdem_tract$inpov, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% population with public assistance or SNAP benefits</strong>",
              round(socdem_tract$inpov, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(socdem_tract$inpov), 
                    fillOpacity = 0.6, 
                    stroke = FALSE,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        # addMarkers(data = residential) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$inpov),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "hispanic"){
      pal <- colorQuantile("Blues", domain = socdem_tract$hispanic, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% population with public assistance or SNAP benefits</strong>",
              round(socdem_tract$hispanic, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(socdem_tract$hispanic), 
                    fillOpacity = 0.6, 
                    stroke = FALSE,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        # addMarkers(data = residential) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$hispanic),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "privateins"){
      pal <- colorQuantile("Blues", domain = socdem_tract$privateins, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% population with public assistance or SNAP benefits</strong>",
              round(socdem_tract$privateins, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(socdem_tract$privateins), 
                    fillOpacity = 0.6, 
                    stroke = FALSE,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        # addMarkers(data = residential) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$privateins),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else{
      pal <- colorQuantile("Blues", domain = socdem_tract$publicins, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% population with public assistance or SNAP benefits</strong>",
              round(socdem_tract$publicins, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(socdem_tract$publicins), 
                    fillOpacity = 0.6, 
                    stroke = FALSE,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        # addMarkers(data = residential) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$publicins),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  
  # old plots - snap -----------------------------------------------
  var_old <- reactive({
    input$olddrop
  })
  var_hh <- reactive({
    input$hhdrop
  })
  output$oldplot <- renderLeaflet({
    # healthins wasn't coded properly so it's just all zeroes
if(var_old() == "visdiff") {
          data <- switch(input$oldspecdrop,
                         "Total" = olderadults$visdiff,
                         "_f" = olderadults$visdiff_f,
                         "_m" = olderadults$visdiff_m)
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
        
        labels <- lapply(
          paste("<strong>Area: </strong>",
                olderadults$NAME.y,
                "<br />",
                "<strong>% older adults with vision difficulties</strong>",
                round(data, 2)),
          htmltools::HTML
        )
        
        leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
          addTiles() %>%
          addPolygons(fillColor = ~pal(data), 
                      fillOpacity = 0.6, 
                      stroke = FALSE,
                      label = labels,
                      labelOptions = labelOptions(direction = "bottom",
                                                  style = list(
                                                    "font-size" = "12px",
                                                    "border-color" = "rgba(0,0,0,0.5)",
                                                    direction = "auto"
                                                  ))) %>%
          # addMarkers(data = residential) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values =  ~(data),
                    title = "Percent by<br>Quartile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
      }else if(var_old() == "ambdiff") {
        data <- switch(input$oldspecdrop,
                       "Total" = olderadults$ambdiff,
                       "_f" = olderadults$ambdiff_f,
                       "_m" = olderadults$ambdiff_m)
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
        
        labels <- lapply(
          paste("<strong>Area: </strong>",
                olderadults$NAME.y,
                "<br />",
                "<strong>% older adults with ambulatory difficulties</strong>",
                round(data, 2)),
          htmltools::HTML
        )
        
        leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
          addTiles() %>%
          addPolygons(fillColor = ~pal(data), 
                      fillOpacity = 0.6, 
                      stroke = FALSE,
                      label = labels,
                      labelOptions = labelOptions(direction = "bottom",
                                                  style = list(
                                                    "font-size" = "12px",
                                                    "border-color" = "rgba(0,0,0,0.5)",
                                                    direction = "auto"
                                                  ))) %>%
          # addMarkers(data = residential) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values =  ~(data),
                    title = "Percent by<br>Quartile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
      }else if(var_old() == "cogdiff") {
        data <- switch(input$oldspecdrop,
                       "Total" = olderadults$cogdiff,
                       "_f" = olderadults$cogdiff_f,
                       "_m" = olderadults$cogdiff_m)
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
        
        labels <- lapply(
          paste("<strong>Area: </strong>",
                olderadults$NAME.y,
                "<br />",
                "<strong>% older adults with cognitive difficulties</strong>",
                round(data, 2)),
          htmltools::HTML
        )
        
        leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
          addTiles() %>%
          addPolygons(fillColor = ~pal(data), 
                      fillOpacity = 0.6, 
                      stroke = FALSE,
                      label = labels,
                      labelOptions = labelOptions(direction = "bottom",
                                                  style = list(
                                                    "font-size" = "12px",
                                                    "border-color" = "rgba(0,0,0,0.5)",
                                                    direction = "auto"
                                                  ))) %>%
          # addMarkers(data = residential) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values =  ~(data),
                    title = "Percent by<br>Quartile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
      }else if(var_old() == "carediff") {
        data <- switch(input$oldspecdrop,
                       "Total" = olderadults$carediff,
                       "_f" = olderadults$carediff_f,
                       "_m" = olderadults$carediff_m)
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
        
        labels <- lapply(
          paste("<strong>Area: </strong>",
                olderadults$NAME.y,
                "<br />",
                "<strong>% older adults with self-care difficulties</strong>",
                round(data, 2)),
          htmltools::HTML
        )
        
        leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
          addTiles() %>%
          addPolygons(fillColor = ~pal(data), 
                      fillOpacity = 0.6, 
                      stroke = FALSE,
                      label = labels,
                      labelOptions = labelOptions(direction = "bottom",
                                                  style = list(
                                                    "font-size" = "12px",
                                                    "border-color" = "rgba(0,0,0,0.5)",
                                                    direction = "auto"
                                                  ))) %>%
          # addMarkers(data = residential) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values =  ~(data),
                    title = "Percent by<br>Quartile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
      }else if(var_old() == "ildiff") {
        data <- switch(input$oldspecdrop,
                       "Total" = olderadults$ildiff,
                       "_f" = olderadults$ildiff_f,
                       "_m" = olderadults$ildiff_m)
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
        
        labels <- lapply(
          paste("<strong>Area: </strong>",
                olderadults$NAME.y,
                "<br />",
                "<strong>% older adults with independent living difficulties</strong>",
                round(data, 2)),
          htmltools::HTML
        )
        
        leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
          addTiles() %>%
          addPolygons(fillColor = ~pal(data), 
                      fillOpacity = 0.6, 
                      stroke = FALSE,
                      label = labels,
                      labelOptions = labelOptions(direction = "bottom",
                                                  style = list(
                                                    "font-size" = "12px",
                                                    "border-color" = "rgba(0,0,0,0.5)",
                                                    direction = "auto"
                                                  ))) %>%
          # addMarkers(data = residential) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values =  ~(data),
                    title = "Percent by<br>Quartile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
      }else if(var_old() == "disab") {
        data <- switch(input$oldspecdrop,
                       "Total" = olderadults$disab,
                       "_f" = olderadults$disab_f,
                       "_m" = olderadults$disab_m)
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
        
        labels <- lapply(
          paste("<strong>Area: </strong>",
                olderadults$NAME.y,
                "<br />",
                "<strong>% older adults with any disability</strong>",
                round(data, 2)),
          htmltools::HTML
        )
        
        leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
          addTiles() %>%
          addPolygons(fillColor = ~pal(data), 
                      fillOpacity = 0.6, 
                      stroke = FALSE,
                      label = labels,
                      labelOptions = labelOptions(direction = "bottom",
                                                  style = list(
                                                    "font-size" = "12px",
                                                    "border-color" = "rgba(0,0,0,0.5)",
                                                    direction = "auto"
                                                  ))) %>%
          # addMarkers(data = residential) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values =  ~(data),
                    title = "Percent by<br>Quartile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
      }else if(var_old() == "inpov") {
        data <- switch(input$oldspecdrop,
                       "Total" = olderadults$inpov,
                       "_f" = olderadults$inpov_f,
                       "_m" = olderadults$inpov_m)
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
        
        labels <- lapply(
          paste("<strong>Area: </strong>",
                olderadults$NAME.y,
                "<br />",
                "<strong>% older adults with income below poverty line</strong>",
                round(data, 2)),
          htmltools::HTML
        )
        
        leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
          addTiles() %>%
          addPolygons(fillColor = ~pal(data), 
                      fillOpacity = 0.6, 
                      stroke = FALSE,
                      label = labels,
                      labelOptions = labelOptions(direction = "bottom",
                                                  style = list(
                                                    "font-size" = "12px",
                                                    "border-color" = "rgba(0,0,0,0.5)",
                                                    direction = "auto"
                                                  ))) %>%
          # addMarkers(data = residential) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values =  ~(data),
                    title = "Percent by<br>Quartile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
      }else 
        # if(var_old() == "labfor")
          {
        data <- switch(input$oldspecdrop,
                       "Total" = olderadults$labfor,
                       "_f" = olderadults$labfor_f,
                       "_m" = olderadults$labfor_m)
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
        
        labels <- lapply(
          paste("<strong>Area: </strong>",
                olderadults$NAME.y,
                "<br />",
                "<strong>% older adults in the labor force</strong>",
                round(data, 2)),
          htmltools::HTML
        )
        
        leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
          addTiles() %>%
          addPolygons(fillColor = ~pal(data), 
                      fillOpacity = 0.6, 
                      stroke = FALSE,
                      label = labels,
                      labelOptions = labelOptions(direction = "bottom",
                                                  style = list(
                                                    "font-size" = "12px",
                                                    "border-color" = "rgba(0,0,0,0.5)",
                                                    direction = "auto"
                                                  ))) %>%
          # addMarkers(data = residential) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values =  ~(data),
                    title = "Percent by<br>Quartile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
      }
  })
    output$householdplot <- renderLeaflet({
    #if(var_hh() == "snap"){
      
        # data <- switch(input$hhdrop,
        #                "hhsixty_married" = olderadults$hhsixty_marr,
        #                "hhsixty_total" = olderadults$hhsixty_total,
        #                "hhsixty_nonfam" = olderadults$hhsixty_nonfam,
        #                "hhsixty_mhh" = olderadults$hhsixty_mhh,
        #                "hhsixty_fhh" = olderadults$hhsixty_fhh,
        #                "snap" = olderadults$snap)
        # spec <- switch(input$hhdrop,
        #                "hhsixty_married" = "Married",
        #                "hhsixty_total" = "Total",
        #                "hhsixty_nonfam" = "Single",
        #                "hhsixty_mhh" = "Male",
        #                "hhsixty_fhh" = "Female",
        #                "snap" = "SNAP Benefit")

      #   pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      # 
      #   labels <- lapply(
      #     paste("<strong>Area: </strong>",
      #           olderadults$NAME.y,
      #           "<br />",
      #           "<strong>% </strong>",
      #           spec,
      #           "<strong>Households with a 60+ member</strong>",
      #           round(data, 2)),
      #     htmltools::HTML
      #   )
      # 
      #   leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
      #     addTiles() %>%
      #     addPolygons(fillColor = ~pal(data),
      #                 fillOpacity = 0.6,
      #                 stroke = FALSE,
      #                 label = labels,
      #                 labelOptions = labelOptions(direction = "bottom",
      #                                             style = list(
      #                                               "font-size" = "12px",
      #                                               "border-color" = "rgba(0,0,0,0.5)",
      #                                               direction = "auto"
      #                                             ))) %>%
      #     # addMarkers(data = residential) %>%
      #     addLegend("bottomleft",
      #               pal = pal,
      #               values =  ~(data),
      #               title = "Percent by<br>Quartile Group",
      #               opacity = 0.6,
      #               labFormat = function(type, cuts, p) {
      #                 n = length(cuts)
      #                 paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
      #               })
      #   }
      # else 
        if(var_hh() == "hhsixty_total") {
        data <- switch(input$oldspecdrop,
                       "Total" = olderadults$hhsixty_total,
                       "_f" = olderadults$hhsixty_total,
                       "_m" = olderadults$hhsixty_total)

        pal <- colorQuantile("Blues", domain = olderadults$hhsixty_total, probs = seq(0, 1, length = 5), right = TRUE)

        labels <- lapply(
          paste("<strong>Area: </strong>",
                olderadults$NAME.y,
                "<br />",
                "<strong>% Housholds with a 60+ member</strong>",
                round(olderadults$hhsixty_total, 2)),
          htmltools::HTML
        )

        leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
          addTiles() %>%
          addPolygons(fillColor = ~pal(olderadults$hhsixty_total),
                      fillOpacity = 0.6,
                      stroke = FALSE,
                      label = labels,
                      labelOptions = labelOptions(direction = "bottom",
                                                  style = list(
                                                    "font-size" = "12px",
                                                    "border-color" = "rgba(0,0,0,0.5)",
                                                    direction = "auto"
                                                  ))) %>%
          # addMarkers(data = residential) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values =  ~(olderadults$hhsixty_total),
                    title = "Percent by<br>Quartile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
      }else if(var_hh() == "hhsixty_fhh") {
        data <- switch(input$oldspecdrop,
                       "Total" = olderadults$hhsixty_fhh,
                       "_f" = olderadults$hhsixty_fhh,
                       "_m" = olderadults$hhsixty_fhh)

        pal <- colorQuantile("Blues", domain = olderadults$hhsixty_fhh, probs = seq(0, 1, length = 5), right = TRUE)

        labels <- lapply(
          paste("<strong>Area: </strong>",
                olderadults$NAME.y,
                "<br />",
                "<strong>% Housholds with a Female 60+ member</strong>",
                round(olderadults$hhsixty_fhh, 2)),
          htmltools::HTML
        )

        leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
          addTiles() %>%
          addPolygons(fillColor = ~pal(olderadults$hhsixty_fhh),
                      fillOpacity = 0.6,
                      stroke = FALSE,
                      label = labels,
                      labelOptions = labelOptions(direction = "bottom",
                                                  style = list(
                                                    "font-size" = "12px",
                                                    "border-color" = "rgba(0,0,0,0.5)",
                                                    direction = "auto"
                                                  ))) %>%
          # addMarkers(data = residential) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values =  ~(olderadults$hhsixty_fhh),
                    title = "Percent by<br>Quartile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
      }else if(var_hh() == "hhsixty_mhh") {
        data <- switch(input$oldspecdrop,
                       "Total" = olderadults$hhsixty_mhh,
                       "_f" = olderadults$hhsixty_mhh,
                       "_m" = olderadults$hhsixty_mhh)

        pal <- colorQuantile("Blues", domain = olderadults$hhsixty_mhh, probs = seq(0, 1, length = 5), right = TRUE)

        labels <- lapply(
          paste("<strong>Area: </strong>",
                olderadults$NAME.y,
                "<br />",
                "<strong>% Housholds with a Male 60+ member</strong>",
                round(olderadults$hhsixty_mhh, 2)),
          htmltools::HTML
        )

        leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
          addTiles() %>%
          addPolygons(fillColor = ~pal(olderadults$hhsixty_mhh),
                      fillOpacity = 0.6,
                      stroke = FALSE,
                      label = labels,
                      labelOptions = labelOptions(direction = "bottom",
                                                  style = list(
                                                    "font-size" = "12px",
                                                    "border-color" = "rgba(0,0,0,0.5)",
                                                    direction = "auto"
                                                  ))) %>%
          # addMarkers(data = residential) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values =  ~(olderadults$hhsixty_mhh),
                    title = "Percent by<br>Quartile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
      }else if(var_hh() == "hhsixty_nonfam") {
        data <- switch(input$oldspecdrop,
                       "Total" = olderadults$hhsixty_nonfam,
                       "_f" = olderadults$hhsixty_nonfam,
                       "_m" = olderadults$hhsixty_nonfam)

        pal <- colorQuantile("Blues", domain = olderadults$hhsixty_nonfam, probs = seq(0, 1, length = 5), right = TRUE)

        labels <- lapply(
          paste("<strong>Area: </strong>",
                olderadults$NAME.y,
                "<br />",
                "<strong>% Single Housholds with a 60+ member</strong>",
                round(olderadults$hhsixty_nonfam, 2)),
          htmltools::HTML
        )

        leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
          addTiles() %>%
          addPolygons(fillColor = ~pal(olderadults$hhsixty_nonfam),
                      fillOpacity = 0.6,
                      stroke = FALSE,
                      label = labels,
                      labelOptions = labelOptions(direction = "bottom",
                                                  style = list(
                                                    "font-size" = "12px",
                                                    "border-color" = "rgba(0,0,0,0.5)",
                                                    direction = "auto"
                                                  ))) %>%
          # addMarkers(data = residential) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values =  ~(olderadults$hhsixty_nonfam),
                    title = "Percent by<br>Quartile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
      }else{
        data <- switch(input$oldspecdrop,
                       "Total" = olderadults$hhsixty_marr,
                       "_f" = olderadults$hhsixty_marr,
                       "_m" = olderadults$hhsixty_marr)

        pal <- colorQuantile("Blues", domain = olderadults$hhsixty_marr, probs = seq(0, 1, length = 5), right = TRUE)

        labels <- lapply(
          paste("<strong>Area: </strong>",
                olderadults$NAME.y,
                "<br />",
                "<strong>% Married Housholds with a 60+ member</strong>",
                round(olderadults$hhsixty_marr, 2)),
          htmltools::HTML
        )

        leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
          addTiles() %>%
          addPolygons(fillColor = ~pal(olderadults$hhsixty_marr),
                      fillOpacity = 0.6,
                      stroke = FALSE,
                      label = labels,
                      labelOptions = labelOptions(direction = "bottom",
                                                  style = list(
                                                    "font-size" = "12px",
                                                    "border-color" = "rgba(0,0,0,0.5)",
                                                    direction = "auto"
                                                  ))) %>%
          # addMarkers(data = residential) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values =  ~(olderadults$hhsixty_marr),
                    title = "Percent by<br>Quartile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
      }
})
  
  
  # data and measures table: done ----------------------------------------
    var_topic <- reactive({
      input$topic
    })
  output$datatable <- renderDataTable({
    if(var_topic() == "All"){
  table <- as.data.frame(measures_table)
  table
    }
    else{
      data <- switch(input$topic,
                     "Connectivity" = "connectivity",
                     "Demographics" = "demographics",
                     "Food Access" = "food access",
                     "Health" = "health",
                     "Older Adults" = "older adults")
      table <- subset(measures_table, Topic == data)
      table <- as.data.frame(table)
      table
    }
  })
  
  
  # device: done ---------------------------------------------------------
  var_device <- reactive({
    input$devicedrop
  })
  output$deviceplot <- renderLeaflet({
  if(var_device() != "nocomputer"){
  data <- switch(input$devicedrop,
                 "laptop" = connectivity$laptop,
                 "smartphone" = connectivity$smartphone,
                 "tablet" = connectivity$tablet, 
                 "nointernet" = connectivity$nointernet,
                 "satellite" = connectivity$satellite,
                 "cellular" = connectivity$cellular,
                 "broadband" = connectivity$broadband)
  
  device_spec <- switch(input$devicedrop,
                 "laptop" = "laptop",
                 "smartphone" = "smartphone",
                 "tablet" = "tablet", 
                 "nointernet" = "no internet access",
                 "satellite" = "satellite internet",
                 "cellular" = "cellular internet",
                 "broadband" = "broadband internet")
  
  pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)
  
  labels <- lapply(
    paste("<strong>Area: </strong>",
          connectivity$NAME.y,
          "<br />",
          "<strong>% Population with",
          device_spec,
          "access</strong>",
          round(data, 2)),
    htmltools::HTML
  )
  
  leaflet(data = connectivity, options = leafletOptions(minZoom = 10))%>%
    addTiles() %>%
    addPolygons(fillColor = ~pal(data), 
                fillOpacity = 0.6, 
                stroke = FALSE,
                label = labels,
                labelOptions = labelOptions(direction = "bottom",
                                            style = list(
                                              "font-size" = "12px",
                                              "border-color" = "rgba(0,0,0,0.5)",
                                              direction = "auto"
                                            ))) %>%
    # addMarkers(data = residential) %>%
    addLegend("bottomleft",
              pal = pal,
              values =  ~(data),
              title = "Percent by<br>Quintile Group",
              opacity = 0.6,
              labFormat = function(type, cuts, p) {
                n = length(cuts)
                paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
              })
  }else{
    pal <- colorQuantile("Blues", domain = connectivity$nocomputer, probs = seq(0, 1, length = 6), right = TRUE)
    
    labels <- lapply(
      paste("<strong>Area: </strong>",
            connectivity$NAME.y,
            "<br />",
            "<strong>% Population without computer access</strong>",
            round(connectivity$nocomputer, 2)),
      htmltools::HTML
    )
    
    leaflet(data = connectivity, options = leafletOptions(minZoom = 10))%>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(connectivity$nocomputer), 
                  fillOpacity = 0.6, 
                  stroke = FALSE,
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      # addMarkers(data = residential) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~(connectivity$nocomputer),
                title = "Percent by<br>Quintile Group",
                opacity = 0.6,
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
    }
  })
  
  
  # wifi: done -----------------------------------------------------------
  var_wifi <- reactive({
    input$wifidrop
  })
  output$wifiplot <- renderLeaflet({
    if(var_wifi() != "Meadows of Dan Elementary School"){
      colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
      
      data <- switch(input$wifidrop,
                     "Woolwine Elementary School" = 2,
                     "Patrick Springs Primary School" = 3,
                     "Blue Ridge Elementary School" = 4,
                     "Patrick County High School" = 5,
                     "Stuart Elementary School" = 6,
                     "Patrick County Branch Library" = 7,
                     "Hardin Reynolds Memorial School" = 8,
                     "Stuart Baptist Church" = 9,                       
                     "Patrick Henry Community College Stuart Campus" = 10
      )
      
      wifi_iso10 <- readRDS(paste0("data/isochrones/wifi/wifi_iso_10_",data,".RDS"))
      wifi_iso15 <- readRDS(paste0("data/isochrones/wifi/wifi_iso_15_",data,".RDS"))
      
      
      residential_map = mapview(residential, cex =.5, layer.name = "residential areas", color = colors[5], legend = F)
      m1 = mapview(wifi_iso10, layer.name = "10 minute isochrone", col.regions = colors[1])
      m2 = mapview(wifi_iso15, layer.name = "15 minute isochrone", col.regions = colors[2])
      m1 = m1 + m2 + residential_map
      
      m1@map
    }else{
      wifi_iso10 <- readRDS(paste0("data/isochrones/wifi/wifi_iso_10_",1,".RDS"))
      wifi_iso15 <- readRDS(paste0("data/isochrones/wifi/wifi_iso_15_",1,".RDS"))
      
      
      residential_map = mapview(residential, cex =.5, layer.name = "residential areas", color = colors[5], legend = F)
      m1 = mapview(wifi_iso10, layer.name = "10 minute isochrone", col.regions = colors[1])
      m2 = mapview(wifi_iso15, layer.name = "15 minute isochrone", col.regions = colors[2])
      m1 = m1 + m2 + residential_map
      
      m1@map
    } 
  })
  
  output$wifitable <- renderDataTable({
    data <- switch(input$wifidrop,
                   "Meadows of Dan Elementary School" = 1,
                   "Woolwine Elementary School" = 2,
                   "Patrick Springs Primary School" = 3,
                   "Blue Ridge Elementary School" = 4,
                   "Patrick County High School" = 5,
                   "Stuart Elementary School" = 6,
                   "Patrick County Branch Library" = 7,
                   "Hardin Reynolds Memorial School" = 8,
                   "Stuart Baptist Church" = 9,                       
                   "Patrick Henry Community College Stuart Campus" = 10 
    )
    
    wifi_iso10 <- readRDS(paste0("data/isochrones/wifi/wifi_iso_10_",data,".RDS"))
    wifi_iso15 <- readRDS(paste0("data/isochrones/wifi/wifi_iso_15_",data,".RDS"))
    
    pp_10 <- st_intersection(residential, wifi_iso10)
    pp_15 <- st_intersection(residential, wifi_iso15)
    
    coverage_10 <- (nrow(pp_10)/nrow(residential)*100)
    coverage_15 <- (nrow(pp_15)/nrow(residential)*100)
    
    table <- as.data.frame(c("10 Minutes", "15 Minutes"))
    table$Coverage <- c(coverage_10, coverage_15)
    colnames(table) <- c("Time", "Coverage")
    table
  })
  
  # ems: done ------------------------------------------------------------
  
  var_ems <- reactive({
    input$emsdrop
  })
  output$emsplot <- renderLeaflet({
    if(var_ems() != "STUART VOLUNTEER FIRE DEPARTMENT"){
      
    colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
    
    data <- switch(input$emsdrop,
    "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = 2,                                                         
    "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = 3,                                                                   
    "VESTA RESCUE SQUAD" = 4,                                                                                           
    "ARARAT RESCUE SQUAD" = 5,                                                                                          
    "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = 6,
    "JEB STUART RESCUE SQUAD" = 7,                                                                                      
    "SMITH RIVER RESCUE SQUAD" = 8,                                                                                     
    "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = 9
    )
    
    ems_iso8 <- readRDS(paste0("data/isochrones/ems/ems_iso_8_",data,".RDS"))
    ems_iso10 <- readRDS(paste0("data/isochrones/ems/ems_iso_10_",data,".RDS"))
    ems_iso12 <- readRDS(paste0("data/isochrones/ems/ems_iso_12_",data,".RDS"))
    
    
    residential_map = mapview(residential, cex =.5, layer.name = "residential areas", color = colors[5], legend = F)
    m1 = mapview(ems_iso8, layer.name = "8 minute isochrone", col.regions = colors[1])
    m2 = mapview(ems_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
    m3 = mapview(ems_iso12, layer.name = "12 minute isochrone", col.regions = colors[3])
    m1 = m1 + m2 + m3 + residential_map
    
    m1@map
  }else{
    ems_iso8 <- readRDS(paste0("data/isochrones/ems/ems_iso_8_",1,".RDS"))
    ems_iso10 <- readRDS(paste0("data/isochrones/ems/ems_iso_10_",1,".RDS"))
    ems_iso12 <- readRDS(paste0("data/isochrones/ems/ems_iso_12_",1,".RDS"))
    
    
    residential_map = mapview(residential, cex =.5, layer.name = "residential areas", color = colors[5], legend = F)
    m1 = mapview(ems_iso8, layer.name = "8 minute isochrone", col.regions = colors[1])
    m2 = mapview(ems_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
    m3 = mapview(ems_iso12, layer.name = "12 minute isochrone", col.regions = colors[3])
    m1 = m1 + m2 + m3 + residential_map
    
    m1@map
  } 
  })
  
  output$emstable <- renderDataTable({
   data <- switch(input$emsdrop,
                    "STUART VOLUNTEER FIRE DEPARTMENT" = 1,
                     "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = 2,
                     "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = 3,
                     "VESTA RESCUE SQUAD" = 4,
                     "ARARAT RESCUE SQUAD" = 5,
                     "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = 6,
                     "JEB STUART RESCUE SQUAD" = 7,
                     "SMITH RIVER RESCUE SQUAD" = 8,
                     "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = 9)


    ems_iso8 <- readRDS(paste0("data/isochrones/ems/ems_iso_8_",data,".RDS"))
    ems_iso10 <- readRDS(paste0("data/isochrones/ems/ems_iso_10_",data,".RDS"))
    ems_iso12 <- readRDS(paste0("data/isochrones/ems/ems_iso_12_",data,".RDS"))

    pp_8 <- st_intersection(residential, ems_iso8)
    pp_10 <- st_intersection(residential, ems_iso10)
    pp_12 <- st_intersection(residential, ems_iso12)

    coverage_8 <- (nrow(pp_8)/nrow(residential)*100)
    coverage_10 <- (nrow(pp_10)/nrow(residential)*100)
    coverage_12 <- (nrow(pp_12)/nrow(residential)*100)

    table <- as.data.frame(c("8 Minutes", "10 Minutes", "12 Minutes"))
    table$Coverage <- c(coverage_8, coverage_10, coverage_12)
    colnames(table) <- c("Time", "Coverage")
    table
    })
  
  # usda - lahunv10share  -----------------------------------------------------------
  var_usda <- reactive({
    input$usdadrop
  })
  output$usdaplot <- renderLeaflet({
    if(var_usda() != "lahunv1share"){
      data <- switch(input$usdadrop,
                     # "lahunv10share" = usda$lahunv10share,
                     "lakids1share" = usda$lakids1share,
                     "lakids10share" = usda$lakids10share,
                     "lalowi1share" = usda$lalowi1share,
                     "lalowi10share" = usda$lalowi10share,
                     "lapop1share" = usda$lapop1share,  
                     "lapop10share" = usda$lapop10share,
                     "laseniors1share" = usda$laseniors1share,
                     "laseniors10share" = usda$laseniors10share)
      
      usda_spec <- switch(input$usdadrop,
                          # "lahunv10share" = "low vehicle access at 10 miles",
                          "lakids1share" = "low food access for children at 1 mile",
                          "lakids10share" = "low food access for children at 10 miles",
                          "lalowi1share" = "low food access for low income population at 1 mile",
                          "lalowi10share" = "low food access for low income population at 10 miles",
                          "lapop1share" = "low food access at 1 mile",  
                          "lapop10share" = "low food access at 10 miles",
                          "laseniors1share" = "low food access for seniors at 1 mile",
                          "laseniors10share" = "low food access for seniors at 10 miles")
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              usda$NAME.y,
              "<br />",
              "<strong>% Population with",
              usda_spec,
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = usda, options = leafletOptions(minZoom = 10))%>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.6, 
                    stroke = FALSE,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        # addMarkers(data = residential) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else{
      pal <- colorQuantile("Blues", domain = usda$lahunv1share, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              usda$NAME.y,
              "<br />",
              "<strong>% Population with low vehicle access at 1 mile</strong>",
              round(usda$lahunv1share, 2)),
        htmltools::HTML
      )
      
      leaflet(data = usda, options = leafletOptions(minZoom = 10))%>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(usda$lahunv1share), 
                    fillOpacity = 0.6, 
                    stroke = FALSE,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        # addMarkers(data = residential) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(usda$lahunv1share),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  # grocery --------------------------------------------------------
  
  var_groc <- reactive({
    input$grocdrop
  })
  output$grocplot <- renderLeaflet({
    if(var_groc() != "Mountain Meadow Farm and Craft Market"){
      colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
      
      data <- switch(input$grocdrop,
                     "Lowes Foods of Stuart" = 4,
                     "Patrick County Local Farmers Market" = 5,
                     "Stuart Farmers Market" = 6,                
                     "W & W Produce" = 7,
                     "Walmart Supercenter" = 8,
                     "Poor Farmers Farm" = 9
      )
      
      groc_iso10 <- readRDS(paste0("data/isochrones/grocery/grc_iso_10_",data,".RDS"))
      groc_iso15 <- readRDS(paste0("data/isochrones/grocery/grc_iso_15_",data,".RDS"))
      
      
      residential_map = mapview(residential, cex =.5, layer.name = "residential areas", color = colors[5], legend = F)
      m1 = mapview(groc_iso10, layer.name = "10 minute isochrone", col.regions = colors[1])
      m2 = mapview(groc_iso15, layer.name = "15 minute isochrone", col.regions = colors[2])
      m1 = m1 + m2 + residential_map
      
      m1@map
    }else{
      groc_iso10 <- readRDS(paste0("data/isochrones/grocery/grc_iso_10_",3,".RDS"))
      groc_iso15 <- readRDS(paste0("data/isochrones/grocery/grc_iso_15_",3,".RDS"))
      
      
      residential_map = mapview(residential, cex =.5, layer.name = "residential areas", color = colors[5], legend = F)
      m1 = mapview(groc_iso10, layer.name = "10 minute isochrone", col.regions = colors[1])
      m2 = mapview(groc_iso15, layer.name = "15 minute isochrone", col.regions = colors[2])
      m1 = m1 + m2 + residential_map
      
      m1@map
    } 
  })
  
  output$groctable <- renderDataTable({
    data <- switch(input$grocdrop,
                   "Mountain Meadow Farm and Craft Market" = 3,
                   "Lowes Foods of Stuart" = 4,
                   "Patrick County Local Farmers Market" = 5,
                   "Stuart Farmers Market" = 6,                
                   "W & W Produce" = 7,
                   "Walmart Supercenter" = 8,
                   "Poor Farmers Farm" = 9
    )
    
    groc_iso10 <- readRDS(paste0("data/isochrones/grocery/grc_iso_10_",data,".RDS"))
    groc_iso15 <- readRDS(paste0("data/isochrones/grocery/grc_iso_15_",data,".RDS"))
    
    pp_10 <- st_intersection(residential, groc_iso10)
    pp_15 <- st_intersection(residential, groc_iso15)
    
    coverage_10 <- (nrow(pp_10)/nrow(residential)*100)
    coverage_15 <- (nrow(pp_15)/nrow(residential)*100)
    
    table <- as.data.frame(c("10 Minutes", "15 Minutes"))
    table$Coverage <- c(coverage_10, coverage_15)
    colnames(table) <- c("Time", "Coverage")
    table
  })
  
  output$othermap <- renderLeaflet({
    
    patrickcty <- counties(state = "51", year = 2018)
    patrickcty <- st_as_sf(patrickcty)
    patrickcty <- patrickcty %>% filter(COUNTYFP == 141)


    # Other food
     otherfood <- otherfood %>% 
      geocode(fulladdress, lat = latitude, long = longitude, method = "cascade")
    
    otherfood$latitude[7] <- 36.735423
    otherfood$longitude[7] <- -80.403206
    otherfood$geo_method[7] <- "manual"
    
    otherfood$latitude <- jitter(otherfood$latitude, factor = 1)
    otherfood$longitude <- jitter(otherfood$longitude, factor = 1)

    pal <- colorFactor(c("#0E879C", "#D9E12B", "#E6A01D"), domain = otherfood$type)

    labels <- lapply(
      paste("<strong>Name: </strong>",
            otherfood$name,
            "<br />",
            "<strong>Address:</strong>",
            otherfood$fulladdress,
            "<br />",
            "<strong>Type:</strong>",
            otherfood$type,
            "<br />",
            "<strong>Open to:</strong>",
            otherfood$audience,
            "<br />",
            "<strong>Notes:</strong>",
            otherfood$notes),
      htmltools::HTML
    )

    leaflet(data = otherfood,
            options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = patrickcty, stroke = T, weight = 2, color = "black", fillOpacity = 0) %>%
      addCircleMarkers(stroke = FALSE,
                       fillOpacity = 0.7,
                       color = ~pal(type),
                       radius = 7,
                       opacity = 0.7,
                       label = labels,
                       labelOptions = labelOptions(direction = "bottom",
                                                   style = list(
                                                     "font-size" = "12px",
                                                     "border-color" = "rgba(0,0,0,0.5)",
                                                     direction = "auto"))) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~type,
                title = "Type",
                opacity = 0.7)
  })
}

shinyApp(ui = ui, server = server)

