##########################################################
####Figure 1 - time series eDNA/RNA and VennDiagram ######
##########################################################

#Time series 

library(tidyverse)
library(dplyr)
library(ggplot2)

#### General data setup ####
data<- read.csv("eDNA_RNA_meta_0515.csv")

#Setting common patterns both time series
set<- theme(axis.text.x=element_text(angle=45,vjust = 0.9, hjust=1, size = 11),
            axis.text.y=element_text(size = 11),
            axis.ticks = element_line(linewidth = 0.5),
            axis.title.y = element_text(size = 13, vjust = 2),
            legend.title = element_blank(),
            legend.text = element_text(size=12))

#### Time series - eDNA eRNA ####
unique(data$year)
data$year<- as.character(data$year)
unique(data$DNA_RNA)

#Separating papers with more than 1 type (one each row)
data_time_type<- data%>%
  mutate(DNA_RNA = strsplit(as.character(DNA_RNA), ", ")) %>%
  unnest(DNA_RNA)
unique(data_time_type$DNA_RNA) 

data_time_type<- data_time_type %>% #filtering only variables for this plot
  select(year, DNA_RNA)

#Counting studies
data_time_type<- data_time_type %>%
  group_by(year, DNA_RNA)%>%
  summarise(count = n())

#Plot time series per eDNA/RNA
time_eDNA_RNA<- ggplot(data_time_type, aes(x=year, y=count, linetype=DNA_RNA, group=DNA_RNA))+
  geom_line(linewidth=0.7)+
  geom_point()+
  labs(x="", y="Number of studies")+
  theme_classic()+
  scale_x_discrete(breaks=data_time_type$year)+
  scale_y_continuous(breaks =c(0,10,20,30,40,50,60,70,80))+
  set

# save plot
ggsave('timeseries_eDNA_RNA.pdf', time_eDNA_RNA,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)

#VennDiagram eDNA/eRNA

data_venn_dnarna<- data%>%
  select("DNA, RNA")

venn_dnarna_table<- vennCounts(data_venn_dnarna)

pdf(file = "venn_dnarna.pdf", width = 13, height = 12)

vennDiagram(venn_dnarna_table,
                          circle.col = c("violet", "blue"))
dev.off()
