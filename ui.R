library(shiny)
library(leaflet)
library(shinythemes)
library(plotly)
library(stringr)

load_campaign <- function(csv_file) {
  
  ww_data <- read.csv(csv_file, stringsAsFactors = FALSE )
  
  campaign <- unlist(str_split(ww_data$campaign, pattern = ","))
  campaign <- str_replace(campaign, pattern = "\\s", "")
  campaign <- unique(campaign)
  
  
  return(campaign)
}

generate_tab_panel <- function(campaign_to_load, map_to_load, tab_name, tab_absolutePanel_name, sliderInputdate_name, CampaignInput,
                               image_tab_panel) {
  campaign <- load_campaign(campaign_to_load)
  tab <-  tabPanel(tab_name, leafletOutput(map_to_load, height=3000),
                             tags$head(includeCSS("style.css")),
                             absolutePanel(id = "controls", class = "panel panel-default", 
                                           top = 75, left = 55, width = 350, fixed=TRUE,
                                           draggable = TRUE, height = "auto",
                                           span(tags$i(h6()), style="color:#045a8d"),
                                           h3(tab_absolutePanel_name),
                                           htmlOutput(image_tab_panel),
                                           h3(sliderInput(sliderInputdate_name,
                                                          "Dates:",
                                                          min = as.Date("1914-01-01","%Y-%m-%d"),
                                                          max = as.Date("1918-12-01","%Y-%m-%d"),
                                                          value=c(as.Date("1914-01-01","%Y-%m-%d"),
                                                                  as.Date("1918-12-01","%Y-%m-%d"))))
                                           ,
                                           h6(selectInput(inputId=CampaignInput,label=h3("Campaigns selected"),
                                                          choices=campaign, multiple = TRUE, selected = campaign)),
                                           h6("Please put the mouse cursor inside the box to remove or add elements.", align = "left")
                                           
                             ),
  )
  return(tab)
}


European_theater_tab <- generate_tab_panel("European_theatre_of_World_War_I.csv", "European_map", 
                                           "European theatre of World War I",  "European Theatre", 
                                           "sliderInputdate", "CampaignInput", "image_european_theatre_img")

Middle_eastern_theater_tab <- generate_tab_panel("Middle_Eastern_theatre_of_World_War_I.csv", "Middle_Eastern_map", 
                                           "Middle Eastern theatre of World War I",  "Middle Eastern theatre", 
                                           "sliderInputdate_middle_eastern", "CampaignInput_middle_eastern", "Middle_Eastern_theatre_img")

African_theater_tab <- generate_tab_panel("African_theatre_of_World_War_I.csv", "African_map", 
                                          "African theatre of World War I",  "African theatre", 
                                          "sliderInputdate_African", "CampaignInput_African", "African_theatre_img")

Asian_and_Pacific_theater_tab <- generate_tab_panel("Asian_and_Pacific_theatre_of_World_War_I.csv", "Asian_and_Pacific_map", 
                                                    "Asian and Pacific theatre of World War I",  "Asian and Pacific theatre", 
                                                    "sliderInputdate_Asian_and_Pacific", "CampaignInput_Asian_and_Pacific", "Asian_and_Pacific_theatre_img")

navbarPage(theme = shinytheme("sandstone"),
           "WWI VieweR", id="main",
           European_theater_tab,
           Middle_eastern_theater_tab,
           African_theater_tab,
           Asian_and_Pacific_theater_tab,
           tabPanel("Read Me",includeMarkdown("readme.md")))



