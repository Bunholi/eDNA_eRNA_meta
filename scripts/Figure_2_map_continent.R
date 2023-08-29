######################################################
#### Figure 2 - Bubble plot per continent and map ####
######################################################

#Bubble plot continents and map per country

##Required packages

library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(packcircles)
library(maps)
library(treemap)
library(patchwork)
library(RColorBrewer)

##General data setup
setwd("User/workingdirectory") #your working directory
data<- read.csv("eDNA_eRNA_metadata.csv") #metadata

#Bubble plot - 2a

#Separating papers with more than 1 type (one each row)
data<- data%>%
  mutate(DNA_RNA = strsplit(as.character(DNA_RNA), ", ")) %>%
  unnest(DNA_RNA)
unique(data$DNA_RNA) 

#Separating papers with more than 1 continent (one each row)
data_bubble<- data%>%
  mutate(continent = strsplit(as.character(continent), ", ")) %>%
  unnest(continent)%>%
  drop_na(continent)
unique(data_bubble$continent)

#Counting papers per continent
map_cont<- data_bubble %>%
  group_by(continent) %>%
  summarise(count = n())

#Adjusting bubbles
packing<- circleProgressiveLayout(map_cont$count, sizetype = "area")
packing$radius<- 0.9*packing$radius

data_all <- cbind(map_cont, packing)

dat.gg <- circleLayoutVertices(packing, npoints=50)

#Making the plot
color<- c("grey", "grey", "grey", "grey", "grey", "grey", "grey")
bubble_plot<-ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), alpha = 0.6) +
  geom_text(data = data_all, aes(x, y, size=count, label = continent)) +
  scale_fill_manual(values=color)+
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()

#Saving plot
ggsave('Fig2a.pdf', bubble_plot,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)
#Small bubbles with eDNA and eRNA information were added manually in the illustrator


#Map - 2b
#Setting common patters
clean<- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.05)
)

#Getting shape file for both maps - maps package
world_map<- map_data("world") #including Antarctica

#Separating papers with more than 1 country (one each row)
data_country<- data%>%
  mutate(country = strsplit(as.character(country), ", ")) %>%
  unnest(country)
unique(data_country$country) 

#Counting papers
map_country<- data_country %>%
  group_by(country) %>%
  summarise(count = n())

#Plotting map by country
map_2b<-ggplot(map_country) +
  geom_map(
    dat = world_map, map = world_map, aes(map_id = region),
    fill = "white", color = "grey", size = 0.25) +
  geom_map(map = world_map, aes(map_id = country, fill = count)) +
  scale_fill_viridis_c("Studies per country", alpha = 0.7, option="mako", direction = -1)+
  expand_limits(x = world_map$long, y = world_map$lat)+
  theme(legend.title = element_text(size=6),
        legend.text = element_text(size=6),
        legend.position=c(.14,0.4),
        legend.key.size=unit(0.3, 'cm'),
        plot.margin = unit(c(0.5, 0, -0.3, -0.2), 'cm'))
  clean

#Saving plot
ggsave('map_2b.pdf', map_2b,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)
