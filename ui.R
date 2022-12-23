library(shiny)
library(leaflet)
library(shinythemes)
library(plotly)
library(stringr)

load_campaign <- function(csv_file) {
  
  ww_data <- read.csv(csv_file, stringsAsFactors = FALSE )
  
  campaign <- unlist(str_split(ww_data$campaign, pattern = ","))
  campaign <- str_replace(campaign, pattern = "\\s", "")
  campaign <- str_replace_all(campaign, "_", " ")
  campaign <- unique(campaign)
  
  
  return(campaign)
}

load_date <- function(csv_file) {

  ww_data <- read.csv(csv_file, stringsAsFactors = FALSE )
  ww_data$year <- str_extract(ww_data$year, "^[0-9][0-9][0-9][0-9]")
  ww_data$month <- str_extract(ww_data$month, "^[0-9]+")
  ww_data$day <- str_extract(ww_data$day, "^[0-9]+")
  ww_data$date <- paste0(ww_data$year, "-", ww_data$month, "-", ww_data$day)
  max_date <- max(as.Date(ww_data$date, "%Y-%m-%d"), na.rm = TRUE)

  min_date <- min(as.Date(ww_data$date, "%Y-%m-%d"), na.rm = TRUE)
  
  date <- c(min_date, max_date)
  return(date)
}

generate_tab_panel <- function(campaign_to_load, map_to_load, tab_name, tab_absolutePanel_name, sliderInputdate_name, CampaignInput,
                               image_tab_panel) {
  campaign <- load_campaign(campaign_to_load)
  temporal_windows <- load_date(campaign_to_load)
  min_date <- temporal_windows[1]
  max_date <- temporal_windows[2]
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
                                                          min = min_date,
                                                          max = max_date,
                                                          value=c(min_date,
                                                                  max_date)))
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

global_theater_tab <- generate_tab_panel("WWI_all_battles.csv", "global_map",
                                         "All the battles of World War I",  "World War I",
                                         "sliderInputdate_global", "CampaignInput_global", "global_theatre_img")


navbarPage(theme = shinytheme("sandstone"),
           "WWI VieweR", id="main",
           European_theater_tab,
           Middle_eastern_theater_tab,
           African_theater_tab,
           Asian_and_Pacific_theater_tab,
           global_theater_tab,
           tabPanel("Read Me",includeMarkdown("readme.md")))



