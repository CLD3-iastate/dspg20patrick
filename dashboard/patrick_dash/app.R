library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(tidycensus)
library(viridis)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)

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
olderadults <- st_transform(olderadults, '+proj=longlat +datum=WGS84')
residential <- readRDS("~/git/dspg2020patrick/data/web/residential.Rds")


# user -------------------------------------------------------------
ui <-fluidPage(theme = shinytheme("cosmo"),
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
                        leafletOutput("socioplot")# ,
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
                          p("Currenty this is important for these reasons..."),
                          selectInput("olddrop", "Individual Variables", choices = c(
                            "Without Health Insurance" = "nohealthins",
                            "Vision Difficulty" = "visdiff",
                            "Ambulatory Difficulty" = "ambdiff",
                            "Self-Care Difficulty" = "carediff",
                            "Cognitive Difficulty" = "cogdiff",
                            "Independent Living Difficulty" = "ildiff",
                            "Any Disability" = "disab",
                            "Below 100 percent of the Poverty Line" = "inpov",
                            "Labor Force" = "labfor")
                          ),
                          selectInput("hhdrop", "Household Variables", choices = c(
                            "Married Couple Households with one or more 60+ member" = "hhsixty_married",
                            "Households with one or more 60+ members" = "hhsixty_total",
                            "Single (no partner present) households with one or more 60+ member" = "hhsixty_nonfam",
                            "Households with one or more 60+ members that are Male" = "hhsixty_mhh",
                            "Households with one or more 60+ members that are Female" = "hhsixty_fhh",
                            "Households with one or more 60+ household members receiving SNAP" = "snap")
                          ),
                          selectInput("oldspecdrop", "Specifications for Individual Variables", choices = c(
                            "Total",
                            "Female" = "_f",
                            "Male" = "_m")
                          ),
                          leafletOutput("oldplot")
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
                     # table(datatable)
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
)


# server -----------------------------------------------------------
server <- function(input, output, session) {

  # socio plots -----------------------------------------------------
  
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
      pal <- colorQuantile("Blues", domain = socdem_tract$inpov, probs = seq(0, 1, length = 6), right = TRUE)
      
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
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "hispanic"){
      pal <- colorQuantile("Blues", domain = socdem_tract$hispanic, probs = seq(0, 1, length = 6), right = TRUE)
      
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
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "privateins"){
      pal <- colorQuantile("Blues", domain = socdem_tract$privateins, probs = seq(0, 1, length = 6), right = TRUE)
      
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
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else{
      pal <- colorQuantile("Blues", domain = socdem_tract$publicins, probs = seq(0, 1, length = 6), right = TRUE)
      
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
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  # old plots -----------------------------------------------
  var_old <- reactive({
    input$olddrop
  })
  var_hh <- reactive({
    input$hhdrop
  })
  output$oldplot <- renderLeaflet({
    # healthins wasn't coded properly so it's just all zeroes

    if(var_old() == "nohealthins"){

    data <- switch(input$oldspecdrop,
                    "Total" = olderadults$nohealthins,
                    "_f" = olderadults$nohealthins_f,
                   "_m" = olderadults$nohealthins_m)
    
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = min(4,length(unique(data)))), right = TRUE)
    
        labels <- lapply(
          paste("<strong>Area: </strong>",
                olderadults$NAME.y,
                "<br />",
                "<strong>% older adults without health insurance</strong>",
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
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.6,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
      
      }else 
        if(var_old() == "visdiff") {
          data <- switch(input$oldspecdrop,
                         "Total" = olderadults$visdiff,
                         "_f" = olderadults$visdiff_f,
                         "_m" = olderadults$visdiff_m)
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)
        
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
                    title = "Percent by<br>Quintile Group",
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
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)
        
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
                    title = "Percent by<br>Quintile Group",
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
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)
        
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
                    title = "Percent by<br>Quintile Group",
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
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)
        
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
                    title = "Percent by<br>Quintile Group",
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
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)
        
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
                    title = "Percent by<br>Quintile Group",
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
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)
        
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
                    title = "Percent by<br>Quintile Group",
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
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)
        
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
                    title = "Percent by<br>Quintile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
      }else if(var_old() == "labfor") {
        data <- switch(input$oldspecdrop,
                       "Total" = olderadults$labfor,
                       "_f" = olderadults$labfor_f,
                       "_m" = olderadults$labfor_m)
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)
        
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
                    title = "Percent by<br>Quintile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
      }else 
        # if(var_hh() == "snap")
        {
        
        data <- switch(input$hhdrop,
                       "hhsixty_married" = olderadults$hhsixty_marr,
                       "hhsixty_total" = olderadults$hhsixty_total,
                       "hhsixty_nonfam" = olderadults$hhsixty_nonfam,
                       "hhsixty_mhh" = olderadults$hhsixty_mhh,
                       "hhsixty_fhh" = olderadults$hhsixty_fhh,
                       "snap" = olderadults$snap)
        spec <- switch(input$hhdrop,
                       "hhsixty_married" = "Married",
                       "hhsixty_total" = "Total",
                       "hhsixty_nonfam" = "Single",
                       "hhsixty_mhh" = "Male",
                       "hhsixty_fhh" = "Female",
                       "snap" = "SNAP Benefit")
        
        pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)
        
        labels <- lapply(
          paste("<strong>Area: </strong>",
                olderadults$NAME.y,
                "<br />",
                "<strong>% </strong>", 
                spec,
                "<strong>Households with a 60+ member</strong>",
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
                    title = "Percent by<br>Quintile Group",
                    opacity = 0.6,
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                    })
        }
      # else if(var_hh() == "hhsixty_total") {
      #   
      #   
      #   
      #   pal <- colorQuantile("Blues", domain = olderadults$hhsixty_total, probs = seq(0, 1, length = 6), right = TRUE)
      #   
      #   labels <- lapply(
      #     paste("<strong>Area: </strong>",
      #           olderadults$NAME.y,
      #           "<br />",
      #           "<strong>% Housholds with a 60+ member</strong>",
      #           round(olderadults$hhsixty_total, 2)),
      #     htmltools::HTML
      #   )
      #   
      #   leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
      #     addTiles() %>%
      #     addPolygons(fillColor = ~pal(olderadults$hhsixty_total), 
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
      #               values =  ~(olderadults$hhsixty_total),
      #               title = "Percent by<br>Quintile Group",
      #               opacity = 0.6,
      #               labFormat = function(type, cuts, p) {
      #                 n = length(cuts)
      #                 paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
      #               })
      # }else if(var_hh() == "hhsixty_fhh") {
      #   
      #   pal <- colorQuantile("Blues", domain = olderadults$hhsixty_fhh, probs = seq(0, 1, length = 6), right = TRUE)
      #   
      #   labels <- lapply(
      #     paste("<strong>Area: </strong>",
      #           olderadults$NAME.y,
      #           "<br />",
      #           "<strong>% Housholds with a Female 60+ member</strong>",
      #           round(olderadults$hhsixty_fhh, 2)),
      #     htmltools::HTML
      #   )
      #   
      #   leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
      #     addTiles() %>%
      #     addPolygons(fillColor = ~pal(olderadults$hhsixty_fhh), 
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
      #               values =  ~(olderadults$hhsixty_fhh),
      #               title = "Percent by<br>Quintile Group",
      #               opacity = 0.6,
      #               labFormat = function(type, cuts, p) {
      #                 n = length(cuts)
      #                 paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
      #               })
      # }else if(var_hh() == "hhsixty_mhh") {
      #   
      #   pal <- colorQuantile("Blues", domain = olderadults$hhsixty_mhh, probs = seq(0, 1, length = 6), right = TRUE)
      #   
      #   labels <- lapply(
      #     paste("<strong>Area: </strong>",
      #           olderadults$NAME.y,
      #           "<br />",
      #           "<strong>% Housholds with a Male 60+ member</strong>",
      #           round(olderadults$hhsixty_mhh, 2)),
      #     htmltools::HTML
      #   )
      #   
      #   leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
      #     addTiles() %>%
      #     addPolygons(fillColor = ~pal(olderadults$hhsixty_mhh), 
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
      #               values =  ~(olderadults$hhsixty_mhh),
      #               title = "Percent by<br>Quintile Group",
      #               opacity = 0.6,
      #               labFormat = function(type, cuts, p) {
      #                 n = length(cuts)
      #                 paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
      #               })
      # }else if(var_hh() == "hhsixty_nonfam") {
      #   
      #   pal <- colorQuantile("Blues", domain = olderadults$hhsixty_nonfam, probs = seq(0, 1, length = 6), right = TRUE)
      #   
      #   labels <- lapply(
      #     paste("<strong>Area: </strong>",
      #           olderadults$NAME.y,
      #           "<br />",
      #           "<strong>% Single Housholds with a 60+ member</strong>",
      #           round(olderadults$hhsixty_nonfam, 2)),
      #     htmltools::HTML
      #   )
      #   
      #   leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
      #     addTiles() %>%
      #     addPolygons(fillColor = ~pal(olderadults$hhsixty_nonfam), 
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
      #               values =  ~(olderadults$hhsixty_nonfam),
      #               title = "Percent by<br>Quintile Group",
      #               opacity = 0.6,
      #               labFormat = function(type, cuts, p) {
      #                 n = length(cuts)
      #                 paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
      #               })
      # }else{
      #   
      #   pal <- colorQuantile("Blues", domain = olderadults$hhsixty_marr, probs = seq(0, 1, length = 6), right = TRUE)
      #   
      #   labels <- lapply(
      #     paste("<strong>Area: </strong>",
      #           olderadults$NAME.y,
      #           "<br />",
      #           "<strong>% Married Housholds with a 60+ member</strong>",
      #           round(olderadults$hhsixty_marr, 2)),
      #     htmltools::HTML
      #   )
      #   
      #   leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
      #     addTiles() %>%
      #     addPolygons(fillColor = ~pal(olderadults$hhsixty_marr), 
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
      #               values =  ~(olderadults$hhsixty_marr),
      #               title = "Percent by<br>Quintile Group",
      #               opacity = 0.6,
      #               labFormat = function(type, cuts, p) {
      #                 n = length(cuts)
      #                 paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
      #               })
      #}
  })
  # data and measures table ----------------------------------------
  # output$datatable <- renderTable({
  #   data <- switch(input$topic,
  #                "" = olderadults$visdiff,
  #                "_f" = olderadults$visdiff_f,
  #                "_m" = olderadults$visdiff_m)
  # table(data)
  # })
  # device ---------------------------------------------------------
  # wifi -----------------------------------------------------------
  # ems ------------------------------------------------------------
  # usda -----------------------------------------------------------
  # grocery --------------------------------------------------------
}
shinyApp(ui = ui, server = server)

