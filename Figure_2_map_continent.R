library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(maps)
library(treemap)
library(patchwork)
library(RColorBrewer)

#### General data setup ####
data<- read.csv("eDNA_RNA_meta_0515.csv")

#Bubble plot - 2a

#Separating DNA and RNA studies
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

#Counting papers
map_cont<- data_bubble %>%
  group_by(continent) %>%
  summarise(count = n())

#Adjusting bubbles
packing<- circleProgressiveLayout(map_cont$count, sizetype = "area")
packing$radius<- 0.9*packing$radius

data_all <- cbind(map_cont, packing)

dat.gg <- circleLayoutVertices(packing, npoints=50)


# Make the plot
fig2a<-ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), alpha = 0.6) +
  geom_text(data = data_all, aes(x, y, size=count, label = continent)) +
  scale_fill_manual(values= mako(nrow(data_all)))+
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()

ggsave('Fig2a.pdf', fig2a,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)


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
unique(data$country)
data_country<- data%>%
  mutate(country = strsplit(as.character(country), ", ")) %>%
  unnest(country)
unique(data_country$country) 

#Counting papers
map_country<- data_country %>%
  group_by(country) %>%
  summarise(count = n())

#Plotting map by country

map_1a<-ggplot(map_country) +
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
        plot.margin = unit(c(0.5, 0, -0.3, -0.2), 'cm'))+
  ggtitle("a)") +
  clean

# save map per country
ggsave('map_1a.pdf', map_1a,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)
