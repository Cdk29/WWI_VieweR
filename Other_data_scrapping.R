library(rvest)
library(tidyverse)
library(stringr)
library(plotly)
library(ggthemr)

ggthemr('dust')
ggthemr('light')

# Specify the url for desired website to be scraped
url <- 'https://en.wikipedia.org/wiki/World_War_I_casualties'

# Read the HTML code from the website
webpage <- read_html(url)

# Use CSS selectors to scrape the table
table <- html_nodes(webpage,'table.wikitable')

# Converting the table to a data frame
table <- html_table(table, header = TRUE)

causalties <- table %>%
  bind_rows() %>%
  as_tibble()


idx <- which(causalties$`Population (millions)` %in% c("Neutral nations", "Allies and co-belligerents of World War I", "Central Powers"))
causalties$side <- "Allies and co-belligerents"

causalties[idx[2]:idx[3],]$side <- "Central Powers"

causalties[idx[3]:length(causalties$side),]$side <- "Neutral Nation"


idx <- c(idx, which(causalties$Nation %in% c("Grand total", "Neutral nations", "TotalCentral Powers", "TotalAllied Powers")))

causalties <- causalties[-idx,]
dim(causalties)

causalties$`Total deaths`

causalties$deaths <- str_replace_all(causalties$`Total deaths`, ",", "")

list_deaths <- str_extract_all(causalties$deaths, "[0-9]*")

vector_deaths <- c()
for (i in 1:length(list_deaths)) {
  max_value <- max(list_deaths[[i]])
  print(max_value)
  vector_deaths <- c(vector_deaths, max_value)
  # print(max(list_deaths[[i]][list_deaths[[i]]!=max_value]))
}

causalties$deaths <- as.numeric(vector_deaths)

causalties <- causalties[-which(is.na(causalties$deaths)),]
causalties$Nation <- str_replace_all(causalties$Nation, "\\s[a-z]+$", "")
causalties[7,]$Nation <- "United Kingdom"
causalties[8,]$Nation <- "British Empire"


# from : https://github.com/clauswilke/dataviz/blob/master/visualizing_amounts.Rmd
# df_Americas <- gapminder %>% filter(year == 2007, continent == "Americas")
# ggplot(df_Americas, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) +
# ggthemr('dust')

setwd("~/Bureau/WWI_Viewer")
write.csv(causalties,file='wiki_data.csv', row.names=FALSE)


p <- ggplot(causalties, aes(x = deaths, y = fct_reorder(Nation, deaths))) +
  geom_point(color = "#0072B2", size = 3) +
  # geom_point(size = 3) +
  scale_y_discrete(name = NULL, expand = c(0, 0.5)) +
  theme(
    #axis.ticks.length = grid::unit(0, "pt"),
    #axis.title = element_text(size = 12),
    plot.margin = margin(18, 6, 3, 1.5)
   ) + theme_minimal() 
p


p <- ggplot(causalties, aes(x = deaths, y = fct_reorder(Nation, deaths))) +
  # geom_point(color = "#0072B2", size = 3) +
  geom_point(size = 3) +
  scale_y_discrete(name = NULL, expand = c(0, 0.5)) +
  theme(
    #axis.ticks.length = grid::unit(0, "pt"),
    #axis.title = element_text(size = 12),
    plot.margin = margin(18, 6, 3, 1.5)
  ) #+ theme_minimal() 
p


ggplotly(p) 

p2 <- ggplot(causalties, aes(x = deaths, y = fct_reorder(Nation, deaths), col= side)) +
  geom_point(size = 3) +
  scale_y_discrete(name = NULL, expand = c(0, 0.5)) +
  theme(
    #axis.ticks.length = grid::unit(0, "pt"),
    #axis.title = element_text(size = 12),
    plot.margin = margin(18, 6, 3, 1.5)
  ) # + theme_minimal() 
ggplotly(p2) 

###################
# Circular barplot
#### 

percentage_list <- str_extract_all(causalties$`Deaths as % of population`, "[0-9]+\\.[0-9]")
percentage_vector <- c()
for (i in 1:length(percentage_list)) {
  max_value <- max(percentage_list[[i]])
  print(max_value)
  percentage_vector <- c(percentage_vector, max_value)
}


causalties$percentage <- percentage_vector

#### Addition for the other graph

causalties$wounded <- str_replace_all(causalties$`Military wounded`, ",", "")
causalties$wounded <- str_replace_all(causalties$wounded, "\\[..\\]", "")
causalties$wounded <- str_replace_all(causalties$wounded, "s1", "")
causalties$wounded <- str_replace_all(causalties$wounded, ".+\\sto", "")
causalties$wounded <- str_replace_all(causalties$wounded, "\\—", "NA")
causalties$wounded <- as.numeric(causalties$wounded)

causalties$pop <- str_replace_all(causalties$`Population (millions)` , "\\[..\\]", "")
causalties$pop <- as.numeric(causalties$pop)
causalties$pop <- causalties$pop * 10^6

causalties$wounded_percentage <- (causalties$wounded / causalties$pop) * 100

write.csv(causalties,file='wiki_data.csv', row.names=FALSE)

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

print(data)
print(base_data)
print(grid_data)


p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar

  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) + 
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # 
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),3), y = c(50, 100, 150), label = c("5 %", "10 %", "15 %") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-100,300) +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-3.1, 4), "cm"), #cache le titre
    plot.title = element_text(hjust = 0.2, vjust = -24.5),
  ) + ggtitle("Deaths as percentage of the countries' population") +
  coord_polar() +
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=1, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.8 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,0,0),  vjust=c(-1,0,1),  colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p

###############

#Stacked barplot


causalties <- read_csv("wiki_data.csv")



data <- data.frame(
  individual=causalties$Nation,
  group=str_replace_all(causalties$side, "Allies and co-belligerents", "Allies"),
  Death=as.numeric(causalties$percentage)*10,
  Wounded=as.numeric(causalties$wounded_percentage)*10
)

data <- data[-which(is.na(data$Death)),]

# Transform data in a tidy format (long format)
data <- data %>% gather(key = "observation", value="value", -c(1,2)) 


# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)

data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value))
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

# Make the plot
p <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +

  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) + 
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=1 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # 
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(50, 100, 150, 200), label = c("5 %", "10 %", "15 %", "20 %") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-100,300) +
  # theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,0,0),  vjust=c(-1,0,1),  colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p