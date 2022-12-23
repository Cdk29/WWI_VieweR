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
    }  else if (grepl(pattern= "Ottoman Victory", result, ignore.case = TRUE)) {
      "black"
    }   else if (grepl(pattern= "retreat|defeat .* Ottoman", result, ignore.case = TRUE)) {
      "green"
    } else if (grepl(pattern= "White victory|Soviet retreat|Red retreat", result, ignore.case = TRUE)) {
      "white"
    } else if (grepl(pattern= "Red breakthrough|Red victory", result, ignore.case = TRUE)) {
      "red"
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
  ww_data$year <- str_extract(ww_data$year, "^[0-9][0-9][0-9][0-9]")
  ww_data$month <- str_extract(ww_data$month, "^[0-9]+")
  ww_data$day <- str_extract(ww_data$day, "^[0-9]+")
  ww_data$campaign <- str_replace(ww_data$campaign, pattern = "\\s", "")
  ww_data$campaign <- str_replace_all(ww_data$campaign, "_", " ")
  ww_data$date <- paste0(ww_data$year, "-", ww_data$month, "-", ww_data$day)
  # ww_data$campaign <- str_replace_all(ww_data$campaign)
  ww_data <- mutate(ww_data, on_click=paste0(paste0('<img src=',
                                                    depiction,
                                                    # 'alt="no longer stolen from wikipedia"',
                                                    ' width="300"', #space !
                                                    '>'),
                                            #'<br><strong>depiction: </strong> ', URLencode(depiction),
                                            '<br><strong>Name: </strong> ', name,
                                            # '<br><strong>Campaign: </strong> ', str_replace_all(campaign, "_", " "),
                                            '<br><strong>Campaign: </strong> ', campaign,
                                            '<br><strong>Results: </strong> ', result,
                                            '<br><strong>Date: </strong> ', date,
                                            '<br><strong> Description: </strong>', desc,
                                            '<br><strong>causalties :</strong> ', causalties))

  return(ww_data)
}

#ajouter dans pal, retirer des levels
pal <- colorFactor(pal = c("blue", "black", "orange"), domain = c("Allied Victory", "Uknown", "Entente Victory"))

pal_middle_eastern <- colorFactor(pal = c("blue", "green","black", "orange"),
                                  domain = c("Allied Victory", "Uknown", "Ottoman Victory", "Ottoman Defeat"))

pal_global <- colorFactor(pal = c("blue", "black", "black", "green", "orange", "red", "white"),
              domain = c("Allied Victory", "Ottoman Victory",  "Entente Victory", "Ottoman Defeat", "Uknown", "Red Victory", "White victory"))
  
shinyServer(function(input, output) {

  European_theatre  <- load_battles("European_theatre_of_World_War_I.csv")
  
  src = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/55/Canadian_tank_and_soldiers_Vimy_1917.jpg/300px-Canadian_tank_and_soldiers_Vimy_1917.jpg"
  output$image_european_theatre_img<-renderText({c('<img src="',src,'">')})
  
  Middle_Eastern_theatre_of_World_War_I  <- load_battles("Middle_Eastern_theatre_of_World_War_I.csv")
  
  src_middle = "http://commons.wikimedia.org/wiki/Special:FilePath/G.C._18_March_1915_Gallipoli_Campaign_Article.jpg?width=300"
  output$Middle_Eastern_theatre_img <-renderText({c('<img src="',src_middle, '">')})
  
  African_theatre_of_World_War_I  <- load_battles("African_theatre_of_World_War_I.csv")
  
  src_africa = "http://commons.wikimedia.org/wiki/Special:FilePath/German_trenches_in_Garua.jpg?width=300"
  output$African_theatre_img <- renderText({c('<img src="',src_africa, '">')})
  
  Asian_and_Pacific_theatre_of_World_War_I  <- load_battles("Asian_and_Pacific_theatre_of_World_War_I.csv")
  
  src_asia = "http://commons.wikimedia.org/wiki/Special:FilePath/Bundesarchiv_Bild_134-C1299,_Tsingtau,_Vorderste_deutsche_Frontlinie.jpg?width=300"
  output$Asian_and_Pacific_theatre_img <- renderText({c('<img src="',src_asia, '">')})
  
  global_theatre_of_World_War_I  <- load_battles("WWI_all_battles.csv")

  src_wwi = 'http://commons.wikimedia.org/wiki/Special:FilePath/WWImontage.jpg?width=300'
  output$global_theatre_img <- renderText({c('<img src="', src_wwi, '">')})

  
  output$European_map <- renderLeaflet({
    
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

  output$Middle_Eastern_map <- renderLeaflet({
    
    Middle_Eastern_theatre_of_World_War_I <- Middle_Eastern_theatre_of_World_War_I %>% filter(between(as.Date(date), as.Date(input$sliderInputdate_middle_eastern[1]), as.Date(input$sliderInputdate_middle_eastern[2]))) #%>% 
    Middle_Eastern_theatre_of_World_War_I <- Middle_Eastern_theatre_of_World_War_I[grepl(x = Middle_Eastern_theatre_of_World_War_I$campaign, pattern = paste(input$CampaignInput_middle_eastern, collapse = "|")),]
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = unlist(getColor_icon(Middle_Eastern_theatre_of_World_War_I)),
      library = 'ion',
      markerColor = unlist(getColor(Middle_Eastern_theatre_of_World_War_I))
    )
    Middle_Eastern_theatre_of_World_War_I %>% leaflet() %>%
      addTiles() %>% setView(mean(Middle_Eastern_theatre_of_World_War_I$long), mean(Middle_Eastern_theatre_of_World_War_I$lat), zoom =7) %>% 
      addAwesomeMarkers(lat =  ~lat, lng =~long, popup = ~on_click, 
                        icon = icons, label=~as.character(name)) %>%  
      addLegend(pal=pal_middle_eastern, values = c("Allied Victory", "Uknown", "Ottoman Victory", "Ottoman Defeat"))
  }) 
  
  output$African_map <- renderLeaflet({
    
    African_theatre_of_World_War_I <- African_theatre_of_World_War_I %>% filter(between(as.Date(date), as.Date(input$sliderInputdate_African[1]), as.Date(input$sliderInputdate_African[2]))) #%>% 
    African_theatre_of_World_War_I <- African_theatre_of_World_War_I[grepl(x = African_theatre_of_World_War_I$campaign, pattern = paste(input$CampaignInput_African, collapse = "|")),]
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = unlist(getColor_icon(African_theatre_of_World_War_I)),
      library = 'ion',
      markerColor = unlist(getColor(African_theatre_of_World_War_I))
    )
    African_theatre_of_World_War_I %>% leaflet() %>%
      addTiles() %>% setView(mean(African_theatre_of_World_War_I$long), mean(African_theatre_of_World_War_I$lat), zoom =7) %>% 
      addAwesomeMarkers(lat =  ~lat, lng =~long, popup = ~on_click, 
                        icon = icons, label=~as.character(name)) %>%  
      addLegend(pal=pal, values = c("Allied Victory", "Uknown", "Entente Victory"))
  }) 
  
  output$Asian_and_Pacific_map <- renderLeaflet({

    Asian_and_Pacific_theatre_of_World_War_I <- Asian_and_Pacific_theatre_of_World_War_I %>% filter(between(as.Date(date), as.Date(input$sliderInputdate_Asian_and_Pacific[1]), as.Date(input$sliderInputdate_Asian_and_Pacific[2]))) #%>%
    Asian_and_Pacific_theatre_of_World_War_I <- Asian_and_Pacific_theatre_of_World_War_I[grepl(x = Asian_and_Pacific_theatre_of_World_War_I$campaign, pattern = paste(input$CampaignInput_Asian_and_Pacific, collapse = "|")),]
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = unlist(getColor_icon(Asian_and_Pacific_theatre_of_World_War_I)),
      library = 'ion',
      markerColor = unlist(getColor(Asian_and_Pacific_theatre_of_World_War_I))
    )
    Asian_and_Pacific_theatre_of_World_War_I %>% leaflet() %>%
      addTiles() %>% setView(mean(Asian_and_Pacific_theatre_of_World_War_I$long), mean(Asian_and_Pacific_theatre_of_World_War_I$lat), zoom =7) %>%
      addAwesomeMarkers(lat =  ~lat, lng =~long, popup = ~on_click,
                        icon = icons, label=~as.character(name)) %>%
      addLegend(pal=pal, values = c("Allied Victory", "Uknown", "Entente Victory"))
  })
  
  output$global_map <- renderLeaflet({

    global_theatre_of_World_War_I <- global_theatre_of_World_War_I %>% filter(between(as.Date(date), as.Date(input$sliderInputdate_global[1]), as.Date(input$sliderInputdate_global[2]))) #%>%
    global_theatre_of_World_War_I <- global_theatre_of_World_War_I[grepl(x = global_theatre_of_World_War_I$campaign, pattern = paste(input$CampaignInput_global, collapse = "|")),]
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = unlist(getColor_icon(global_theatre_of_World_War_I)),
      library = 'ion',
      markerColor = unlist(getColor(global_theatre_of_World_War_I))
    )
    global_theatre_of_World_War_I %>% leaflet() %>%
      addTiles() %>% setView(mean(global_theatre_of_World_War_I$long), mean(global_theatre_of_World_War_I$lat), zoom =7) %>%
      addAwesomeMarkers(lat =  ~lat, lng =~long, popup = ~on_click,
                        icon = icons, label=~as.character(name)) %>%
      addLegend(pal=pal_global, 
                values = c("Allied Victory", "Ottoman Victory",  "Entente Victory", "Ottoman Defeat", "Uknown", "Red Victory", "White victory"))
  })

  
  
  
})

