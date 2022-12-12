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


shinyServer(function(input, output) {
  # Import Data 
  
  ww_data <- read.csv("European_theatre_of_World_War_I.csv", stringsAsFactors = FALSE )

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

  output$wwmap <- renderLeaflet({
      leaflet(ww_data) %>%
      addTiles() %>%
      addMarkers(data = ww_data, lat =  ~lat, lng =~long, popup = ~on_click)}) 

})
