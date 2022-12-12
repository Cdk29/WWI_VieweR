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
  
  ## A refactor en fonction
load_battles <- function(csv_file) {
  
  ww_data <- read.csv(csv_file, stringsAsFactors = FALSE )

  ww_data <- mutate(ww_data, on_click=paste0(paste0('<img src=',
                                                    depiction,
                                                    # 'alt="no longer stolen from wikipedia"',
                                                    ' width="300"', #space !
                                                    '>'),
                                            #'<br><strong>depiction: </strong> ', URLencode(depiction),
                                            '<br><strong>Name: </strong> ', name,
                                            '<br><strong>Date: </strong> ', date,
                                            '<br><strong> Description: </strong>', desc,
                                            '<br><strong>causalties :</strong> ', causalties))

  return(ww_data)
}


shinyServer(function(input, output) {

  # create a color paletter for category type in the data file
  # pal <- palette_OkabeIto[c(5,1)] 
  
  # pal <- c("#6778A5", "#ECBF93")
  # pal <- colorFactor(pal = pal, domain = c("Eolienne", "Panneau solaire"))

  # output$reactive_case_count <- renderText({
  #   paste0("Production éolienne estimé : ", max(production$value), " MW")
  # })
  # 
  # output$reactive_death_count <- renderText({
  #   paste0("Consommation estimé : ", max(electricity$value), " MW")
  # })
  
  European_theatre  <- load_battles("European_theatre_of_World_War_I.csv")
  
 # European_theatre <- reactive(European_theatre %>% filter(date %in% input$year))
  output$European_map <- renderLeaflet({
    European_theatre %>% filter(date %in% input$year) %>% 
    leaflet() %>%
      addTiles() %>% #setView(mean(European_theatre$long), mean(European_theatre$lat), zoom =7) %>% 
      addMarkers(lat =  ~lat, lng =~long, popup = ~on_click)}) 

})
