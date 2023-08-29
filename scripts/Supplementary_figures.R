###############################
#### Supplementary figures ####
###############################

#Required packages
library(tidyverse)
library(dplyr)
library(ggplot2)

##General data setup
setwd("User/workingdirectory") #your working directory
data<- read.csv("eDNA_eRNA_metadata.csv") #metadata

#Setting common patterns both time series
set<- theme(axis.text.x=element_text(angle=45,vjust = 0.9, hjust=1, size = 11),
            axis.text.y=element_text(size = 11),
            axis.ticks = element_line(linewidth = 0.5),
            axis.title.y = element_text(size = 13, vjust = 2),
            legend.title = element_blank(),
            legend.text = element_text(size=12))

#### Time series - Extraction kit ####
unique(data$year)
data$year<- as.character(data$year)
unique(data$DNA_extr)

#Separating papers with more than 1 type (one each row)
data_time_extr<- data%>%
  mutate(DNA_extr = strsplit(as.character(DNA_extr), ", ")) %>%
  unnest(DNA_extr)
unique(data_time_extr$DNA_extr) 

data_time_extr<- data_time_extr %>% #filtering only variables for this plot and 5 most frequent extr method
  select(year, DNA_extr)%>%
  filter(DNA_extr %in% c("QBT_Kit", "QPW_Kit", "QPS_Kit", "PCI", "CTAB")) #kits-only studies that followed manufacture protocol

#Counting studies
data_time_extr<- data_time_extr %>%
  group_by(year, DNA_extr)%>%
  summarise(count = n())

#Plot time series per extraction method
time_dnaextr<-ggplot(data_time_extr, aes(x=year, y=count, color=DNA_extr, group=DNA_extr))+
  geom_line(linewidth=0.7)+
  geom_point()+
  labs(x="", y="Number of studies")+
  theme_classic()+
  scale_x_discrete(breaks=data_time_extr$year)+
  scale_y_continuous(breaks =c(0,2,4,6,8,10,12,14,16))+
  set   

ggsave('timeseries_DNAextr.pdf', time_dnaextr,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)


#### Time series - organism type ####
unique(data$year)
data$year<- as.character(data$year)
unique(data$type_organism)

#Separating papers with more than 1 type (one each row)
data_time_org<- data%>%
  mutate(type_organism = strsplit(as.character(type_organism), ", ")) %>%
  unnest(type_organism)
unique(data_time_org$type_organism) 

data_time_org<- data_time_org %>% #filtering only variables for this plot and 5 most frequent extr method
  select(year, type_organism)%>%
  filter(type_organism %in% c("Fish", "Not target", "Bacteria","Invertebrate", "Phytoplankton", "Protistan")) #kits-only studies that followed manufacture protocol

#Counting studies
data_time_org<- data_time_org %>%
  group_by(year, type_organism)%>%
  summarise(count = n())

#Plot time series per extraction method
time_org<-ggplot(data_time_org, aes(x=year, y=count, color=type_organism, group=type_organism))+
  geom_line(linewidth=0.7)+
  geom_point()+
  labs(x="", y="Number of studies")+
  theme_classic()+
  scale_x_discrete(breaks=data_time_org$year)+
  #scale_y_continuous(breaks =c(0,10,20,30,50))+
  set   

ggsave('timeseries_org.pdf', time_org,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)


#### Time series - Filter type ####
unique(data$year)
data$year<- as.character(data$year)
unique(data$filter_type)

#Separating papers with more than 1 type (one each row)
data_time_ft<- data%>%
  mutate(filter_type = strsplit(as.character(filter_type), ", ")) %>%
  unnest(filter_type)
unique(data_time_ft$filter_type) 

data_time_ft<- data_time_ft %>% #filtering only variables for this plot and 5 most frequent extr method
  select(year, filter_type)%>%
  filter(filter_type %in% c("Polyethersulfone", "Polycarbonate", "Glass_Fiber", "Cellulose_Esters", "Cellulose_Nitrate")) #kits-only studies that followed manufacture protocol

#Counting studies
data_time_ft<- data_time_ft %>%
  group_by(year, filter_type)%>%
  summarise(count = n())

#Plot time series per extraction method
time_ft<-ggplot(data_time_ft, aes(x=year, y=count, color=filter_type, group=filter_type))+
  geom_line(linewidth=0.7)+
  geom_point()+
  labs(x="", y="Number of studies")+
  theme_classic()+
  scale_x_discrete(breaks=data_time_ft$year)+
  #scale_y_continuous(breaks =c(0,2,4,6,8,10,12,14,16))+
  set   

ggsave('timeseries_filtertype.pdf', time_ft,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)
