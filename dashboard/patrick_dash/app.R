library(shiny)

ui <-
  navbarPage("Patrick County Dashboard",
             tabPanel("Main", value = "main",
                      mainPanel(
                      )
             ),
             tabPanel("Sociodemographics", value = "socio",
                      mainPanel(
                        h2("Sociodemographics of Patrick County")
                      )
             ),
             tabPanel("Older Adult Well-Being", value = "older",
                        mainPanel(
                          h2("Older Adults")
                      )
             ),
             tabPanel("Health Care Access", value ="ems",
                      #sidebarLayout(
                        #sidebarPanel(
                        #),
                        mainPanel(
                          h2("Health Care Access")
                        )
                      #)
              ),
             navbarMenu("Connectivity",
                        tabPanel("Device and Internet Access", value =  "device",
                                   mainPanel(
                                     h2("Device and Internet Access")
                                   )
                        ),
                        tabPanel("Wi-Fi Hotspot Access", value = "wifi",
                                   mainPanel(
                                     h2("Wi-Fi Hotspot Access"),
                                     p("This is a paragraph about coverage maps because we love coverage maps"),
                                     div(),
                                     p("This is a second paragraph")
                                   )
                        )
                      ),
                        navbarMenu("Food Access",
                                   tabPanel("USDA Data Explorer", value =  "usda",
                                              mainPanel(
                                                h2("USDA Data Explorer")
                                              )
                                  ),
                                   tabPanel("Grocery and Farmers Market Access", value = "grocery",
                                              mainPanel(
                                                h2("Grocery and Farmers Market Access")
                                            )
                        )
          ),
          tabPanel("Data and Measures", value = "data",
                   mainPanel(
                     h2("Data and Measures")
                   )
          ),
          tabPanel("Contact", value = "contact",
                   mainPanel(
                     h2("Contact")
                   )
          ),
  inverse = T
  )

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)

