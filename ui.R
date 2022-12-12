library(shiny)
library(leaflet)
library(shinythemes)
library(plotly)

navbarPage(theme = shinytheme("sandstone"),
           "WWI VieweR", id="main",
           tabPanel("European theatre of World War I", leafletOutput("wwmap", height=3000),
                    tags$head(includeCSS("style.css")),
                    absolutePanel(id = "controls", class = "panel panel-default", 
                                  top = 75, left = 55, width = 350, fixed=TRUE,
                                  draggable = TRUE, height = "auto",
                                  
                                  span(tags$i(h6()), style="color:#045a8d")#,
                                  # h3(textOutput("reactive_case_count"), align = "right"),
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



