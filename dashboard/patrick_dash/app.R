library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(tidycensus)
library(viridis)
library(ggthemes)
library(RColorBrewer)

# data -----------------------------------------------------------
socdem_block <- readRDS("~/git/dspg2020patrick/data/web/socdem_block.Rds")
socdem_block <- st_transform(socdem_block, '+proj=longlat +datum=WGS84')
socdem_tract <- readRDS("~/git/dspg2020patrick/data/web/socdem_tract.Rds")
socdem_tract <- st_transform(socdem_tract, '+proj=longlat +datum=WGS84')
connectivity <- readRDS("~/git/dspg2020patrick/data/web/connectivity.Rds")
ems <- readRDS("~/git/dspg2020patrick/data/web/ems.Rds")
groceries <- readRDS("~/git/dspg2020patrick/data/web/groceries.Rds")
usda <- readRDS("~/git/dspg2020patrick/data/web/usda.Rds")
wifi <- readRDS("~/git/dspg2020patrick/data/web/wifi.Rds")
olderadults <- readRDS("~/git/dspg2020patrick/data/web/olderadults.Rds")
residential <- readRDS("~/git/dspg2020patrick/data/web/residential.Rds")


# user -----------------------------------------------------------
ui <-
  navbarPage("Patrick County Dashboard",
# main -----------------------------------------------------------
# TBD: words by isabel and tas
             tabPanel("Main", value = "main",
                      mainPanel(
                      h2("Patrick County"),
                      br(),
                      p("Patrick county is fun and I love it, but it's rural so sometimes 
                        we have issues with health care access..."),
                      div(),
                      p("our team is made up of...")
                      )
             ),
# socio -----------------------------------------------------------
             tabPanel("Sociodemographics", value = "socio",
                      mainPanel(
                        h2("Sociodemographics of Patrick County"),
                        br(),
                        p(""),
                        div(),
                        p("Currenty this is important for these reasons..."),
                        selectInput("sociodrop", "Select Variable", names(socdem_block)),
                        leafletOutput("socioplot") # ,
                        #probably drop down menu with two menus for data sets?
                      )
             ),
# older -----------------------------------------------------------
             tabPanel("Older Adult Well-Being", value = "older",
                        mainPanel(
                          h2("Older Adult Well-Being"),
                          br(),
                          p("Older adults have the hardest time getting healthcare..."),
                          div(),
                          p("Currenty this is important for these reasons...") # ,
                          #output(older people leaflets),
                          #probably drop down menu with two menus for data sets?
                          #select based on gender
                      )
             ),
# wifi-----------------------------------------------------------
             navbarMenu("Connectivity",
                        tabPanel("Device and Internet Access", value =  "device",
                                   mainPanel(
                                     h2("Device and Internet Access"),
                                     br(),
                                     p("This is a paragraph about connectivity status"),
                                     div(),
                                     p("This is a second paragraph about why it's important") # ,
                                     #output(leaflets),
                                     #probably drop down variable selector
                                   )
                        ),
                        # wifi maps -----------------------------
                        tabPanel("Wi-Fi Hotspot Access", value = "wifi",
                                   mainPanel(
                                     h2("Wi-Fi Hotspot Access"),
                                     br(),
                                     p("This is a paragraph about coverage maps because we love coverage maps"),
                                     div(),
                                     p("This is a second paragraph") # ,
                                     #output(wifi leaflets),
                                     #probably drop down for isochrone range, wifi hotpsot, coverage
                                     #table
                                   )
                        )
                      ),
# ems -----------------------------------------------------------
            tabPanel("Health Care Access", value ="ems",
                      #sidebarLayout(
                      #sidebarPanel(
                      #),
                    mainPanel(
                      h2("Health Care Access"),
                      br(),
                      p("This is a paragraph about coverage maps because we love coverage maps"),
                      div(),
                      p("This is a second paragraph") # ,
                         #output(leaflets),
                          #probably drop down for isochrone range, hotpsot, and coverage
                        #table
                    )
                      #)
),
# food -----------------------------------------------------------
            navbarMenu("Food Access",
                        tabPanel("USDA Data Explorer", value =  "usda",
                              mainPanel(
                                   h2("USDA Data Explorer"),
                                   br(),
                                   p("This is a paragraph about food access"),
                                   div(),
                                   p("This is a second paragraph about current importance") # ,
                                   #output(leaflet)
                                   #dropdown menu w variable options
                                  )
                              ),
                                  # food maps ---------------------
                         tabPanel("Grocery and Farmers Market Access", value = "grocery",
                                mainPanel(
                                    h2("Grocery and Farmers Market Access"),
                                    br(),
                                    p("This is a paragraph about food access"),
                                    div(),
                                    p("This is a second paragraph about current importance") # ,
                                    #output(leaflet)
                                    #isochrone range, locarion, coverage table
                                   )
                        )
          ),
# data -----------------------------------------------------------
          tabPanel("Data and Measures", value = "data",
                   mainPanel(
                     h2("Data and Measures"),
                     br(),
                     p("paragraph about data.") # ,
                     # table that displays measures based on topic selection
                   )
          ),
# contact -----------------------------------------------------------
          tabPanel("Contact", value = "contact",
                   mainPanel(
                     h2("Contact"),
                     br(),
                     p("This is a paragraph about contacting us")
                   )
          ),
  inverse = T
  )

# server -----------------------------------------------------------
server <- function(input, output) {

# server socio -----------------------------------------------------

  output$socioplot <- renderLeaflet({
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
      addPolygons(fillColor = ~pal(snap), 
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
                values =  ~snap,
                title = "Percent by<br>Quintile Group", 
                opacity = 0.6,
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
  })
}

shinyApp(ui = ui, server = server)

