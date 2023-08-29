################################################################
#### Figure 6 - time series, VennDiagram and Barplot ID type ####
################################################################

##Required packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(viridis)
library(eulerr)

##General data setup
setwd("User/workingdirectory") #your working directory
data<- read.csv("eDNA_eRNA_metadata.csv") #metadata

#Venn Diagram

#Selecting only the columns with binary data
ID_data<- data %>%
  select("ASV", "OTU", "Species")

#Generating euler object
prop_venn<- euler(ID_data)

#Plotting and saving - number of studies were added in the illustrator
pdf(file = "venn_idtype.pdf", width = 13, height = 12)
plot(prop_venn, fill=c("#FBE36B","#65CFAE", "#C9C9C9"))
dev.off()


#Time Series

##Setting time series graphical elements
set<- theme(axis.text.x=element_text(angle=45,vjust = 0.9, hjust=1, size = 11),
            axis.text.y=element_text(size = 11),
            axis.ticks = element_line(linewidth = 0.5),
            axis.title.y = element_text(size = 13, vjust = 2),
            legend.title = element_blank(),
            legend.text = element_text(size=12))

#### Time series - idtype ####
unique(data$year)
data$year<- as.character(data$year)

#Separating papers with more than 1 type (one each row)
data_time_type<- data%>%
  mutate(id_type = strsplit(as.character(id_type), ", ")) %>%
  unnest(id_type)
unique(data_time_type$id_type) 

#Counting studies
data_time_type<- data_time_type %>%
  group_by(year, id_type)%>%
  summarise(count = n())%>%
  drop_na()

#Plot time series per id type

custom_colors <- c("#FBE36B", "#65CFAE","#C9C9C9")

time_idtype<- ggplot(data_time_type, aes(x=year, y=count, group=id_type, color=id_type))+
  geom_line(linewidth=0.7)+
  geom_point()+
  labs(x="", y="Number of studies")+
  theme_classic()+
  scale_x_discrete(breaks=data_time_type$year)+
  scale_y_continuous(breaks =c(0,10,20,30,40,50,60,70,80))+
  scale_color_manual(values = custom_colors)+
  set

#Saving plot
ggsave('timeseries_idtype.pdf', time_idtype,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)

#Barplot

unique(data$id_type)
data_two<- data%>%
  filter(id_type %in% "ASV, OTU")

data_id<- data%>%
  mutate(id_type = strsplit(as.character(id_type), ", ")) %>%
  unnest(id_type)
unique(data_id$id_type)

#Filtering only the 6 most common organisms and counting
data_id_plot<- data_id %>%
  filter(type_organism %in% c("Fish", "Non target", "Bacteria","Invertebrate", "Phytoplankton", "Protist"))%>%
  group_by(id_type, type_organism)%>%
  summarize(count=n())

data_id_plot$id_type[is.na(data_id_plot$id_type)] <- "None"

#Plotting 

bar_id<- data_id_plot %>%
  ggplot(aes(x=type_organism, y=count, fill=id_type))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#FBE36B", "#65CFAE","#C9C9C9", "#483D8B"), name=NULL,
                    labels=c("ASV", "OTU", "Species", "None"))+
  scale_x_discrete(limits = c("Non target", "Fish", "Bacteria","Invertebrate", "Phytoplankton", "Protist"))+
  geom_text(aes(label = count), size=3.2, 
            position = position_dodge(0.9),
            color="black",vjust = -0.1,hjust = 0.5)+
  theme_classic()+
  xlab("")+
  ylab("Number of studies")+
  theme(legend.position = "top")+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_blank())

#Saving plot
ggsave('id_barplot.pdf', bar_id,
       width = 15, height = 10, units = c('cm'),
       dpi = 600)


