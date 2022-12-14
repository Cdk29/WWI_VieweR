library(shiny)
library(leaflet)
library(shinythemes)
library(plotly)

load_campaign <- function(csv_file) {
  
  ww_data <- read.csv(csv_file, stringsAsFactors = FALSE )
  
  campaign <- unlist(str_split(ww_data$campaign, pattern = ","))
  campaign <- str_replace(campaign, pattern = "\\s", "")
  campaign <- unique(campaign)
  
  
  return(campaign)
}

generate_tab_panel <- function() {
  tab <-  tabPanel("European theatre of World War I", leafletOutput("European_map", height=3000),
                             tags$head(includeCSS("style.css")),
                             absolutePanel(id = "controls", class = "panel panel-default", 
                                           top = 75, left = 55, width = 350, fixed=TRUE,
                                           draggable = TRUE, height = "auto",
                                           span(tags$i(h6()), style="color:#045a8d"),
                                           h3("European Theatre"),
                                           htmlOutput('image'),
                                           h3(sliderInput("sliderInputdate",
                                                          "Dates:",
                                                          min = as.Date("1914-01-01","%Y-%m-%d"),
                                                          max = as.Date("1918-12-01","%Y-%m-%d"),
                                                          value=c(as.Date("1914-01-01","%Y-%m-%d"),
                                                                  as.Date("1918-12-01","%Y-%m-%d"))))
                                           ,
                                           h6(selectInput(inputId="CampaignInput",label=h3("Campaigns selected"),
                                                          choices=campaign, multiple = TRUE, selected = campaign)),
                                           h6("Please put the mouse cursor inside the box to remove or add elements.", align = "left")
                                           
                             ),
  )
  return(tab)
}

campaign <- load_campaign("European_theatre_of_World_War_I.csv")

navbarPage(theme = shinytheme("sandstone"),
           "WWI VieweR", id="main",
           generate_tab_panel(),
           # tabPanel("European theatre of World War I", leafletOutput("European_map", height=3000),
           #          tags$head(includeCSS("style.css")),
           #          absolutePanel(id = "controls", class = "panel panel-default", 
           #                        top = 75, left = 55, width = 350, fixed=TRUE,
           #                        draggable = TRUE, height = "auto",
           #                        span(tags$i(h6()), style="color:#045a8d"),
           #                        h3("European Theatre"),
           #                        htmlOutput('image'),
           #                        h3(sliderInput("sliderInputdate",
           #                                      "Dates:",
           #                                      min = as.Date("1914-01-01","%Y-%m-%d"),
           #                                      max = as.Date("1918-12-01","%Y-%m-%d"),
           #                                      value=c(as.Date("1914-01-01","%Y-%m-%d"),
           #                                              as.Date("1918-12-01","%Y-%m-%d"))))
           #                        ,
           #                        h6(selectInput(inputId="CampaignInput",label=h3("Campaigns selected"),
           #                                          choices=campaign, multiple = TRUE, selected = campaign)),
           #                        h6("Please put the mouse cursor inside the box to remove or add elements.", align = "left")
           #                        
           #          ),
           # ),
           #tabPanel("Data", DT::dataTableOutput("data")),
           tabPanel("Read Me",includeMarkdown("readme.md")))



