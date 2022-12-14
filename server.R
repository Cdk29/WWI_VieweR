library(shiny)
library(readr)
library(dplyr)
library(leaflet)
library(sf)
library(DT)
library(rgdal)
library(colorblindr)
library(lubridate)
library(stringr)
# ggthemr('light')
library(utils)
library(digest)
# https://stackoverflow.com/questions/21686645/how-to-create-md5-hash-of-a-column-in-r
# Import Data 

getColor <- function(ww_data) {
  lapply(ww_data$result, function(result) {
    if(grepl(pattern= "British|Italian|French|Belgian|Allied|Canadian", result)) {
      "blue"
    }  else if (grepl(pattern= "German|Entente|Austro|Bulgarian", result)) {
      "black"
    } else {
      "orange"
    } })
}

getColor_icon <- function(ww_data) {
  lapply(ww_data$result, function(result) {
    if(grepl(pattern= "British|Italian|French|Belgian|Allied|Canadian", result)) {
      "black"
    }  else if (grepl(pattern= "German|Entente|Austro|Bulgarian", result)) {
      "white"
    } else {
      "purple"
    } })
}

load_battles <- function(csv_file) {
  
  ww_data <- read.csv(csv_file, stringsAsFactors = FALSE )
  #ww_data$date <- paste0(ww_data$day, "/", ww_data$month, "/", ww_data$year)
  ww_data$date <- paste0(ww_data$year, "-", ww_data$month, "-", ww_data$day)
  # ww_data$campaign <- str_replace_all(ww_data$campaign)
  ww_data <- mutate(ww_data, on_click=paste0(paste0('<img src=',
                                                    depiction,
                                                    # 'alt="no longer stolen from wikipedia"',
                                                    ' width="300"', #space !
                                                    '>'),
                                            #'<br><strong>depiction: </strong> ', URLencode(depiction),
                                            '<br><strong>Name: </strong> ', name,
                                            '<br><strong>Results: </strong> ', result,
                                            '<br><strong>Date: </strong> ', date,
                                            '<br><strong> Description: </strong>', desc,
                                            '<br><strong>causalties :</strong> ', causalties))

  return(ww_data)
}

pal <- colorFactor(pal = c("blue", "black", "orange"), domain = c("Allied Victory", "Uknown", "Entente Victory"))

shinyServer(function(input, output) {

  European_theatre  <- load_battles("European_theatre_of_World_War_I.csv")
  
  
  output$European_map <- renderLeaflet({

    src = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/55/Canadian_tank_and_soldiers_Vimy_1917.jpg/300px-Canadian_tank_and_soldiers_Vimy_1917.jpg"
    output$image<-renderText({c('<img src="',src,'">')})
    
    European_theatre <- European_theatre %>% filter(between(as.Date(date), as.Date(input$sliderInputdate[1]), as.Date(input$sliderInputdate[2]))) #%>% 
    European_theatre <- European_theatre[grepl(x = European_theatre$campaign, pattern = paste(input$CampaignInput, collapse = "|")),]
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = unlist(getColor_icon(European_theatre)),
      library = 'ion',
      markerColor = unlist(getColor(European_theatre))
    )
    European_theatre %>% leaflet() %>%
      addTiles() %>% setView(mean(European_theatre$long), mean(European_theatre$lat), zoom =7) %>% 
      addAwesomeMarkers(lat =  ~lat, lng =~long, popup = ~on_click, 
                        icon = icons, label=~as.character(name)) %>%  
      addLegend(pal=pal, values = c("Allied Victory", "Uknown", "Entente Victory"))
    }) 

})

