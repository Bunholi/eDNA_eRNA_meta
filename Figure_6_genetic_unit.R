library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(viridis)
library(limma)

#### General data setup ####
data<- read.csv("eDNA_RNA_meta_0515.csv")


#Venn Diagram

data_venn_idtype<- data%>%
  select(ASV,	OTU,Species)

venn_idtype_table<- vennCounts(data_venn_idtype)

pdf(file = "venn_idtype.pdf", width = 13, height = 12)

vennDiagram(venn_idtype_table,
            circle.col = c("dodgerblue2","seagreen","blueviolet"))

dev.off()

#Time Series

#Setting common patterns both time series
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
  summarise(count = n())

#Plot time series per id type
time_idtype<- ggplot(data_time_type, aes(x=year, y=count, linetype=id_type, group=id_type))+
  geom_line(linewidth=0.7)+
  geom_point()+
  labs(x="", y="Number of studies")+
  theme_classic()+
  scale_x_discrete(breaks=data_time_type$year)+
  scale_y_continuous(breaks =c(0,10,20,30,40,50,60,70,80))+
  set

# save plot
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

data_id<- data_id%>%
  mutate(id_type = replace(id_type, id_type=="Species ", "Species"))
unique(data_id$id_type)

data_id<- data_id%>%
  mutate(id_type = replace(id_type, id_type=="OTU ", "OTU"))
unique(data_id$id_type)

id_org<- data_id%>%
  group_by(id_type, type_organism)%>%
  summarize(count=n())

data_id_plot<- data %>%
  filter(type_organism %in% c("Fish", "Not target", "Bacteria","Invertebrate", "Phytoplankton", "Protistan"))

data_id_plot$id_type[is.na(data_id_plot$id_type)] <- "None"

bar_id<- data_id_plot %>%
  ggplot(aes(x=type_organism, y=count, fill=id_type))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("steelblue","darkslateblue", "grey","lightpink"))+
  scale_x_discrete(limits = c("Not target", "Fish", "Bacteria","Invertebrate", "Phytoplankton", "Protistan"))+
  geom_text(aes(label = count), size=3.2, 
            position = position_dodge(0.9),
            color="black",vjust = -0.1,hjust = 0.5)+
  theme_classic()+
  xlab("")+
  ylab("Number of studies")+
  theme(legend.position = "top")+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_blank())

ggsave('id_barplot.pdf', bar_id,
       width = 15, height = 10, units = c('cm'),
       dpi = 600)


