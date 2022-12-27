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
library(utils)
library(digest)
library(forcats)
library(ggthemr)

ggthemr('light')

create_circular_barplot <- function(causalties) {
  
  data <- data.frame(
    individual=causalties$Nation,
    group=str_replace_all(causalties$side, "Allies and co-belligerents", "Allies"),
    value=as.numeric(causalties$percentage)*10
  )
  
  data <- data[-which(is.na(data$value)),]
  
  
  data <- data %>% arrange(group, value)
  
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 3
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$group <- rep(levels(data$group), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(group)
  data$id <- seq(1, nrow(data))
  
  # Get the name and the y position of each label
  label_data <- data
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data <- data %>% 
    group_by(group) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]

  p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) + 
    geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    # 
    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = rep(max(data$id),3), y = c(50, 100, 150), label = c("5 %", "10 %", "15 %") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
    
    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
    ylim(-100,200) +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-3.1, 4), "cm"), #cache le titre
      plot.title = element_text(hjust = 0.2, vjust = -30),
    ) + ggtitle("Deaths as percentage of the countries' population") +
    coord_polar() +
    geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=1, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.8 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,0,0),  vjust=c(-1,0,1),  colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
  
  return(p)
}

vizualising_amounts <- function(causalties) {
  
  p2 <- ggplot(causalties, aes(x = deaths, y = fct_reorder(Nation, deaths), col= side)) +
    geom_point(size = 3) +
    scale_y_discrete(name = NULL, expand = c(0, 0.5)) +
    theme(legend.position = "none",
          axis.title = element_blank()
    ) # + theme_minimal() 
  p <- ggplotly(p2) 
   
  return(p)
}

getColor <- function(ww_data) {
  lapply(ww_data$result, function(result) {
    if(grepl(pattern= "British|Italian|French|Belgian|Entente|Allied|Canadian|Australian", result)) {
      "blue"
    }  else if (grepl(pattern= "German|Austro|Bulgarian", result)) {
      "black"
    }  else if (grepl(pattern= "Ottoman Victory", result, ignore.case = TRUE)) {
      "black"
    }   else if (grepl(pattern= "retreat|defeat .* Ottoman", result, ignore.case = TRUE)) {
      "green"
    } else if (grepl(pattern= "White victory|Soviet retreat|Red retreat|Japan", result, ignore.case = TRUE)) {
      "white"
    } else if (grepl(pattern= "Red breakthrough|Red victory", result, ignore.case = TRUE)) {
      "red"
    } else {
      "orange"
    } })
}

getColor_icon <- function(ww_data) {
  lapply(ww_data$result, function(result) {
    if(grepl(pattern= "British|Italian|French|Belgian|Entente|Allied|Canadian", result)) {
      "black"
    }  else if (grepl(pattern= "German|Austro|Bulgarian", result)) {
      "white"
    }  else if (grepl(pattern= "Japan", result)) {
      "red"
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
pal <- colorFactor(pal = c("blue", "black", "orange"), domain = c("Allied Victory", "German Victory","Uknown"))

pal_middle_eastern <- colorFactor(pal = c("blue", "green","black", "orange"),
                                  domain = c("Allied Victory", "Uknown", "Ottoman Victory", "Ottoman Defeat"))

pal_global <- colorFactor(pal = c("blue", "black","white", "green", "black", "red", "orange","white"),
              domain = c("Allied Victory", "Ottoman Victory",  "Central Powers Victory", "Ottoman Defeat", "Uknown", "Red Victory", "Japan Victory", "White victory"))
  
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
      addLegend(pal=pal, values = c("Allied Victory", "German Victory","Uknown"))
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
      addLegend(pal=pal, values = c("Allied Victory", "Uknown", "Central Powers Victory"))
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
      addLegend(pal=pal, values = c("Allied Victory", "Uknown", "Central Powers Victory"))
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
                values = c("Allied Victory", "Ottoman Victory",  "Central Powers Victory", "Ottoman Defeat", "Uknown", "Red Victory", "Japan Victory", "White victory"))
  })

  
  causalties <- read_csv("wiki_data.csv")
  
  output$circular_bar_plot <- renderPlot(create_circular_barplot(causalties))
  
  output$deaths_plot <- renderPlotly(vizualising_amounts(causalties))
  
  output$Explanation_dot_plots <- renderText("An explanation about the dot plot.")
  
  output$Explanation_circular <- renderText("An explanation about the circular barplot. Cf the work of Christian Ingrao ....")
})

