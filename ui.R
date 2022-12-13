library(shiny)
library(leaflet)
library(shinythemes)
library(plotly)

choices_list_year <- list("1914" = 1914, 
                          "1915" = 1915, 
                          "1916" = 1916,
                          "1917" = 1917,
                          "1918" = 1918)

navbarPage(theme = shinytheme("sandstone"),
           "WWI VieweR", id="main",
           tabPanel("European theatre of World War I", leafletOutput("European_map", height=3000),
                    tags$head(includeCSS("style.css")),
                    absolutePanel(id = "controls", class = "panel panel-default", 
                                  top = 75, left = 55, width = 350, fixed=TRUE,
                                  draggable = TRUE, height = "auto",
                                  
                                  span(tags$i(h6()), style="color:#045a8d"),
                                  #column(3,
                                  sliderInput("sliderInputdate",
                                                "Dates:",
                                                min = as.Date("1914-01-01","%Y-%m-%d"),
                                                max = as.Date("1918-12-01","%Y-%m-%d"),
                                                value=c(as.Date("1914-01-01","%Y-%m-%d"),
                                                        as.Date("1918-12-01","%Y-%m-%d")))
                                  # h4(textOutput("reactive_death_count"), align = "right"),
                                  # h6(textOutput("clean_date_reactive"), align = "right"),
                                  # h6(textOutput("reactive_country_count"), align = "right"),
                                  # plotlyOutput("prediction_plot", height="180px", width="100%"),
                                  # plotlyOutput("production_plot", height="180px", width="100%"),
                                  # plotlyOutput("pilot_plot", height="180px", width="100%")
                                  
                    ),
           ),
           #tabPanel("Data", DT::dataTableOutput("data")),
           tabPanel("Read Me",includeMarkdown("readme.md")))



