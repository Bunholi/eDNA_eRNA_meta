##########################################################
#### Figure 1 - time series eDNA/eRNA and VennDiagram ####
##########################################################

#Time series 

##Required packages
library(tidyverse)
library(dplyr)
library(ggplot2)

##General data setup
setwd("/Users/ingridbunholi/Desktop/eDNA_RNA_meta/metadata") #working directory
data<- read.csv("eDNA_RNA_meta_0515.csv") #metadata

##Setting time series graphical elements
set<- theme(axis.text.x=element_text(angle=45,vjust = 0.9, hjust=1, size = 11),
            axis.text.y=element_text(size = 11),
            axis.ticks = element_line(linewidth = 0.5),
            axis.title.y = element_text(size = 13, vjust = 2),
            legend.title = element_blank(),
            legend.text = element_text(size=12))

#### Time series - eDNA eRNA studies over time ####

#Checking years
unique(data$year)
data$year<- as.character(data$year)

#Checking DNA_RNA studies - separating papers with more than 1 type (one each row)
unique(data$DNA_RNA)
data_time_type<- data%>%
  mutate(DNA_RNA = strsplit(as.character(DNA_RNA), ", ")) %>%
  unnest(DNA_RNA)
unique(data_time_type$DNA_RNA) 

#Counting DNA_RNA studies per year
data_time_type<- data_time_type %>%
  group_by(year, DNA_RNA)%>%
  summarise(count = n())

#Plot time series eDNA/eRNA studies over time
time_eDNA_RNA<- ggplot(data_time_type, aes(x=year, y=count, linetype=DNA_RNA, group=DNA_RNA))+
  geom_line(linewidth=0.7)+
  geom_point()+
  labs(x="", y="Number of studies")+
  theme_classic()+
  scale_x_discrete(breaks=data_time_type$year)+
  scale_y_continuous(breaks =c(0,10,20,30,40,50,60,70,80))+
  set

#Saving plot
ggsave('timeseries_eDNA_RNA.pdf', time_eDNA_RNA,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)

#VennDiagram eDNA/eRNA

##Required packages
library(eulerr)

#Selecting only the columns with binary data
eDNA_RNA_data<- data %>%
  select("DNA", "RNA")

#Generating euler object
prop_venn<- euler(eDNA_RNA_data)

#Plotting and saving - number of studies were added in the illustrator
pdf(file = "venn_dnarna.pdf", width = 13, height = 12)
plot(prop_venn, fill=c("#9DBFEB", "#8856A7"), alpha=0.5)
dev.off()
