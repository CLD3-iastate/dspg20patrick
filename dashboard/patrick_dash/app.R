library(shiny)

ui <-
  navbarPage("Patrick County Dashboard",
             tabPanel("Older Adults",
                      sidebarLayout(
                        sidebarPanel(
                        ),
                        mainPanel(
                          h2("Older Adults")
                        )
                      )
             ),
             tabPanel("Connectivity",
                      sidebarLayout(
                        sidebarPanel(
                        ),
                        mainPanel(
                          h2("Connectvity")
                        )
                      )
              ),
             navbarMenu("Health Services",
                        tabPanel("DHS Data",
                                 sidebarLayout(
                                   sidebarPanel(
                                   ),
                                   mainPanel(
                                     h2("DHS Data")
                                   )
                                 )
                        ),
                        tabPanel("Coverage Maps",
                                 sidebarLayout(
                                   sidebarPanel(
                                   ),
                                   mainPanel(
                                     h2("EMS Isochrone Coverage Maps")
                                   )
                                )
                        )
                      ),
                        navbarMenu("Food Access",
                                   tabPanel("USDA Data",
                                            sidebarLayout(
                                              sidebarPanel(
                                              ),
                                              mainPanel(
                                                h2("USDA Food Access Data")
                                              )
                                            )
                                  ),
                                   tabPanel("Coverage Maps",
                                            sidebarLayout(
                                              sidebarPanel(
                                              ),
                                              mainPanel(
                                                h2("Food Acess Isochrone Coverage Maps")
                                              )
                                            )
                        )
          ),
  inverse = T
  )

server <- function(input, output) {
   
}

shinyApp(ui = ui, server = server)

