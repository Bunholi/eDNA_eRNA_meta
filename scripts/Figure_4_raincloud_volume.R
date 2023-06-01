###################################
#### Figure 4 - Raincloud plot ####
###################################

##Required packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggdist)
library(gghalves)

##General data setup
setwd("/Users/ingridbunholi/Desktop/eDNA_RNA_meta/metadata") #working directory
data<- read.csv("eDNA_RNA_meta_0515.csv") #metadata

##Rain cloud plot - volume

#separating papers with more than 1 type (one each row)
df<- data%>%
  mutate(DNA_RNA = strsplit(as.character(DNA_RNA), ", ")) %>%
  unnest(DNA_RNA)
unique(df$DNA_RNA) 

#Filtering only the 6 most frequent organisms and counting
data_vol<- df%>%
  select("volume_l", "type_organism", "DNA_RNA")%>%
  filter(type_organism %in% c("Fish", "Non target", "Bacteria","Invertebrate", "Phytoplankton", "Protist"))%>%
  drop_na()%>%
  mutate(volume_l=round(as.numeric(volume_l), digits=2))%>%
  subset(volume_l!=92.50)

#Transforming RNA occurrences into category
data_vol_2<- data_vol %>%
  mutate(type_organism = ifelse(DNA_RNA == "RNA", "RNA", type_organism))

#Setting parameters
data_vol_2$type_organism<-factor(data_vol_2$type_organism, levels=c("RNA", "Non target", "Fish", "Bacteria", "Invertebrate", "Phytoplankton", "Protist"))
pal <- c("#9862A3", "#3F85DF", "#3F85DF", "#3F85DF", "#3F85DF", "#3F85DF", "#3F85DF")

#Generating rain cloud plot
vol.org<-data_vol_2 %>%
  group_by(type_organism) %>%
  ggplot(aes(x=type_organism, y=volume_l, fill=type_organism)) +
  ggdist::stat_halfeye(
    adjust=0.5,
    width=0.6,
    justification=-0.5,
    .width=0,
    point_color=NA) +
  geom_boxplot(width=0.5, outlier.color = NA, alpha=0.5, position = position_dodge2(width = .8))+
  coord_cartesian(xlim=c(1.2, NA))+
  scale_y_continuous(breaks = round(seq(min(data_vol_2$volume_l), 
                                         max(data_vol_2$volume_l), by = 2),1))+
  geom_point(aes(group=DNA_RNA),
    size = 1.6,
    alpha = 0.2,
    position = position_jitter(
      seed = 1, width = .2
    ))+
  # Themes and Labels
  scale_color_manual(values = pal, guide = "none") +
  scale_fill_manual(values = pal, guide = "none") +
  scale_shape_manual(values=c(19,8))+
  theme_minimal() +
  theme(axis.text.x=element_text(size = 10, vjust = -0.5),
        axis.title.x = element_text(size = 12, vjust = -1),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank())+
  labs(x="",
    y = "Volume (Liters)") +
  coord_flip()

    
#saving plot
ggsave('cloud_plot_final.pdf', vol.org,
       width = 28, height = 18, units = c('cm'),
       dpi = 600)



