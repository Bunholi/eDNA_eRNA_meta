#####################################################################
#### Figure 3 - Halfmoon and barplots filter type and extraction ####
#####################################################################

##Required packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(patchwork)

##General data setup
setwd("/Users/ingridbunholi/Desktop/eDNA_RNA_meta/metadata") #working directory
data<- read.csv("eDNA_RNA_meta_0515.csv") #metadata

#Half moon plots - figure 3a

##Organism
#Separating papers with more than 1 type (one each row)
data_org<- data%>%
  mutate(type_organism = strsplit(as.character(type_organism), ", ")) %>%
  unnest(type_organism)
unique(data_org$type_organism)

#Selecting only the 6 most common groups
data_org<- data_org%>%
  select("type_organism")%>%
  filter(type_organism %in% c("Fish", "Bacteria","Non target","Invertebrate", "Protist", "Phytoplankton"))

#Counting the number of studies per organism type
data_o<- data_org%>%
  group_by(type_organism)%>%
  drop_na()%>%
  summarise(n())

##System
#Separating papers with more than 1 type (one each row)
data_sys<- data%>%
  mutate(system = strsplit(as.character(system), ", ")) %>%
  unnest(system)
unique(data_sys$system)

data_sys<- data_sys%>%
  select("system")%>%
  filter(system %in% c("River", "Coastal Ocean", "Lake", "Bay", "Open Ocean", "Coral reef"))

#Selecting only the 6 most common groups
data_s<- data_sys %>%
  group_by(system)%>%
  drop_na()%>%
  summarise(n())

#Renaming both data_o and data_s columns to match
data_o<- data_o %>%
  rename("all" = "type_organism")%>%
  rename("count"="n()")

data_s<- data_s %>%
  rename("all" = "system")%>%
  rename("count" = "n()")

#Binding both datasets to produce proportional halfmoon plots
data_halfmoon<- rbind(data_s, data_o)

#Adjusting parameters to produce the plot
data_halfmoon$all <- factor(data_halfmoon$all, levels = c("River", "Coastal Ocean", "Lake", "Bay", "Open Ocean","Coral reef", "Non target", "Fish", "Invertebrate", "Protist", "Bacteria", "Phytoplankton"))
posit<- c(1,2,1,2,1,2,1,2,1,2,1,2)
data_hm<- cbind(data_halfmoon, posit)
col<- c("lightcyan4","lightcyan4","lightcyan4","lightcyan4","lightcyan4","lightcyan4","lightcyan4","lightcyan4","lightcyan4","lightcyan4", "lightcyan4","lightcyan4")

#Generating plot
hfmoon<-ggplot(data_hm, aes(x=posit, y=sqrt(count), fill=all)) + 
  geom_col(width =1) +
  scale_x_continuous(expand = c(0, 0), limits = c(0.5, 2.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = col, guide = "none") +
  scale_fill_manual(values = col, guide = "none") +
  coord_polar(theta = "x", direction = 1) +
  facet_wrap(~all) +
  theme_void()

#Saving plot
ggsave('halfmoon_all_v3.pdf', hfmoon,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)
#All other adjustments were made in the illustrator 


#Barplots - figure 3b and 3c

#Setting common patters for both plots
set<- theme(axis.text.x=element_text(size = 10, vjust = -0.5),
            axis.text.y=element_text(size = 10),
            axis.title.y = element_text(size = 12, vjust = 2),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size=12),
            legend.key.size = unit(0.4, units = "cm"))


# Barplot - filter type ~ organism - 3b ####

#Separating papers with more than 1 filter type (one each row)
data_ft<- data%>%
  mutate(filter_type = strsplit(as.character(filter_type), ", ")) %>%
  unnest(filter_type)
unique(data_ft$filter_type)

#Selecting the 6 most frequent organisms
data_ft<- data_ft%>%
  select(filter_type, type_organism)%>%
  filter(type_organism %in% c("Fish", "Non target", "Bacteria","Invertebrate", "Phytoplankton", "Protist" ))

#Counting and filtering the 5 most common filter types
data_ft$filter_type<- as.factor(data_ft$filter_type)
data_ft$type_organism<- as.factor(data_ft$type_organism)

data_ft_summary<- data_ft%>%
  filter(filter_type %in% c("Polyethersulfone", "Polycarbonate", "Glass_Fiber", "Cellulose_Esters", "Cellulose_Nitrate"))%>%
  group_by(type_organism, filter_type) %>%
  dplyr::summarize(count_by_ft = n())%>%
  complete(filter_type, fill=list(count_by_ft=0))

#Side-by-side barplot: type of org ~ filter type

bar_toft<-ggplot(data = data_ft_summary, aes(x = type_organism, y= count_by_ft, fill = filter_type)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=alpha(c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"), 0.8), name=NULL,
                    labels = c("Cellulose Esters", "Cellulose Nitrate","Glass Fiber","Polycarbonate", "Polyethersulfone"))+
  scale_x_discrete(limits = c("Non target", "Fish", "Bacteria", "Invertebrate", "Phytoplankton","Protist"))+
  geom_text(aes(label = count_by_ft), size=3, 
            position = position_dodge(0.9),
            color="black",vjust = 0,hjust = 0.5)+
  theme_classic() +
  xlab("")+
  ylab("Number of studies (n = 177)")+
  theme(legend.position="top")+
  ggtitle("a)") +
  set

#### Barplot - filter type ~ system - 3b ####

#Separating papers with more than 1 system (one each row)
data_sy<- data%>%
  mutate(system = strsplit(as.character(system), ", ")) %>%
  unnest(system)
unique(data_sy$system)

#Selecting the 5 most freq filter type
data_sy<- data_sy%>%
  select(filter_type, system)%>%
  filter(filter_type %in% c("Polyethersulfone", "Polycarbonate", "Glass_Fiber", "Cellulose_Esters", "Cellulose_Nitrate"))

#Counting and filtering the 6 most common systems
data_sy$filter_type<- as.factor(data_sy$filter_type)
data_sy$system<- as.factor(data_sy$system)

data_sy_summary<- data_sy%>%
  filter(system %in% c("River", "Coastal Ocean", "Lake", "Bay", "Coral reef", "Open Ocean"))%>%
  group_by(system, filter_type) %>%
  dplyr::summarize(count_by_ft = n())%>%
  complete(filter_type, fill=list(count_by_ft=0))

#Side-by-side barplot: system ~ filter type

bar_syft<-ggplot(data = data_sy_summary, aes(x = system, y= count_by_ft, fill = filter_type)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=alpha(c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"), 0.8), name=NULL,
                    labels = c("Cellulose Esters", "Cellulose Nitrate","Glass Fiber","Polycarbonate", "Polyethersulfone"))+
  scale_x_discrete(limits = c("Coastal Ocean", "River", "Lake", "Bay", "Open Ocean" , "Coral reef"))+
  geom_text(aes(label = count_by_ft), size=3, 
            position = position_dodge(0.9),
            color="black",vjust = 0,hjust = 0.5)+
  theme_classic() +
  xlab("")+
  ylab("Number of studies (n = 140)")+
  theme(legend.position="none")+
  ggtitle("b)") +
  set


#### Pannel - filter type ~ organism and system ####
barplots<-bar_toft/bar_syft 

#Saving pannel 3b
ggsave('fig_barplots_0102.pdf', barplots,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)


#### Barplot - extraction method ~ organism  - 3c ####

#filtering only 5 most frequent extr method
data_extr_to<- data %>% 
  select(type_organism, DNA_extr)%>%
  filter(DNA_extr %in% c("QBT_Kit", "QPW_Kit", "QPS_Kit", "PCI", "CTAB")) #kits-only studies that followed manufacture protocol

#Counting and filtering the 6 most common organisms
data_extr_to$DNA_extr<- as.factor(data_extr_to$DNA_extr)
data_extr_to$type_organism<- as.factor(data_extr_to$type_organism)

data_ex_to_summary<- data_extr_to%>%
  filter(type_organism %in% c("Fish", "Non target", "Bacteria","Invertebrate", "Phytoplankton", "Protist"))%>%
  group_by(type_organism, DNA_extr) %>%
  dplyr::summarize(count_by_ext = n())%>%
  complete(DNA_extr, fill=list(count_by_ext=0))

#Side-by-side barplot: type organism ~ DNA extraction method

bar_ext1<-ggplot(data = data_ex_to_summary, aes(x = type_organism, y= count_by_ext, fill = DNA_extr)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=alpha(c("lightskyblue2","#b3cde3","#8c96c6","#8856a7","#810f7c"), 0.8), name=NULL,
                    labels=c("CTAB", "PCI", "QBT Kit", "QPS Kit", "QPW Kit"))+
  scale_x_discrete(limits = c("Non target", "Fish", "Bacteria","Invertebrate", "Phytoplankton", "Protist"))+
  geom_text(aes(label = count_by_ext), size=3, 
            position = position_dodge(0.9),
            color="black",vjust = 0,hjust = 0.5)+
  theme_classic() +
  xlab("")+
  ylab("Number of studies (n = 129)")+
  #theme(legend.position=c(0.9, 0.9))+
  theme(legend.position = "top")+
  ggtitle("a)") +
  set

#### Barplot - extraction method ~ system - 3c ####

#Separating papers with more than 1 system (one each row)
data_ext_sy<- data%>%
  mutate(system = strsplit(as.character(system), ", ")) %>%
  unnest(system)
unique(data_ext_sy$system)

#filtering only 5 most frequent extr method
data_ext_sy<- data_ext_sy %>% 
  select(system, DNA_extr)%>%
  filter(DNA_extr %in% c("QBT_Kit", "QPW_Kit", "QPS_Kit", "PCI", "CTAB")) #kits-only studies that followed manufacture protocol

#Counting and filtering the 5 most common systems
data_ext_sy$DNA_extr<- as.factor(data_ext_sy$DNA_extr)
data_ext_sy$system<- as.factor(data_ext_sy$system)

data_ext_sy_summary<- data_ext_sy%>%
  filter(system %in% c("River", "Coastal Ocean", "Lake", "Bay", "Coral reef", "Open Ocean"))%>%
  group_by(system, DNA_extr) %>%
  dplyr::summarize(count_by_ext = n())%>%
  complete(DNA_extr, fill=list(count_by_ext=0))

#Side-by-side barplot: type organism ~ DNA extraction method

bar_ext2<-ggplot(data = data_ext_sy_summary, aes(x = system, y= count_by_ext, fill = DNA_extr)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=alpha(c("lightskyblue2","#b3cde3","#8c96c6","#8856a7","#810f7c"), 0.8), name=NULL,
                    labels=c("CTAB", "PCI", "QBT Kit", "QPS Kit", "QPW Kit"))+
  scale_x_discrete(limits = c("Coastal Ocean", "River", "Lake", "Bay", "Open Ocean", "Coral reef"))+
  geom_text(aes(label = count_by_ext), size=3, 
            position = position_dodge(0.9),
            color="black",vjust = 0,hjust = 0.5)+
  theme_classic() +
  xlab("")+
  ylab("Number of studies (n = 112)")+
  theme(legend.position="none")+
  #theme(legend.position = "top")+
  ggtitle("b)") +
  set


#### Pannel - extraction method ~ organism and system ####
barplots2<-bar_ext1/bar_ext2

#Saving pannel
ggsave('fig_barplots_ext.pdf', barplots2,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)
