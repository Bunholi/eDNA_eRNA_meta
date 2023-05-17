library(ggplot2)
library(tidyverse)
library(dplyr)
library(patchwork)

data<- read.csv("eDNA_RNA_meta_0515.csv")


#Half moon plots - figure 3a

#Organism
data_org<- data%>%
  mutate(type_organism = strsplit(as.character(type_organism), ", ")) %>%
  unnest(type_organism)
unique(data_org$type_organism)

data_org<- data_org%>%
  select("type_organism")%>%
  filter(type_organism %in% c("Fish", "Bacteria","Not target","Invertebrate", "Protistan", "Phytoplankton"))

data_o<- data_org%>%
  group_by(type_organism)%>%
  drop_na()%>%
  summarise(n())

data_o<- data_o %>%
  rename("system" = "type_organism")%>%
  rename("count"="n()")

org<- data_org %>%
  getPercentages(type_organism)
org

#System
data_sys<- data%>%
  mutate(system = strsplit(as.character(system), ", ")) %>%
  unnest(system)
unique(data_sys$system)

data_sys<- data_sys%>%
  select("system")%>%
  filter(system %in% c("River", "Coastal Ocean", "Lake", "Bay", "Open Ocean", "Coral reef"))

data_s<- data_sys %>%
  group_by(system)%>%
  drop_na()%>%
  summarise(n())

sys<- data_sys %>%
  getPercentages(system)
sys

data_s<- data_s %>%
  rename("count" = "n()")

data_halfmoon<- rbind(data_s, data_o)

data_halfmoon$system <- factor(data_halfmoon$system, levels = c("River", "Coastal Ocean", "Lake", "Bay", "Open Ocean","Coral reef", "Not target", "Fish", "Invertebrate", "Protistan", "Bacteria", "Phytoplankton"))

x<- c(1,2,1,2,1,2,1,2,1,2,1,2)
data_hm<- cbind(data_halfmoon, x)

col<- c("lightcyan4","lightcyan4","lightcyan4","lightcyan4","lightcyan4","lightcyan4","lightcyan4","lightcyan4","lightcyan4","lightcyan4", "lightcyan4","lightcyan4")

hfmoon<-ggplot(data_hm, aes(x=x, y=sqrt(count), fill=system)) + 
  geom_col(width =1) +
  scale_x_continuous(expand = c(0, 0), limits = c(0.5, 2.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = col, guide = "none") +
  scale_fill_manual(values = col, guide = "none") +
  coord_polar(theta = "x", direction = 1) +
  facet_wrap(~system) +
  theme_void()

ggsave('halfmoon_all_v3.pdf', hfmoon,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)

#Setting common patters for both plots
set<- theme(axis.text.x=element_text(size = 10, vjust = -0.5),
            axis.text.y=element_text(size = 10),
            axis.title.y = element_text(size = 12, vjust = 2),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size=12),
            legend.key.size = unit(0.4, units = "cm"))


#### Barplot - filter type ~ organism - 3b ####

#Separating papers with more than 1 filter size (one each row)
unique(data$filter_type)

data_ft<- data%>%
  mutate(filter_type = strsplit(as.character(filter_type), ", ")) %>%
  unnest(filter_type)
unique(data_ft$filter_type)

#Selecting the 5 most freq organisms
data_ft<- data_ft%>%
  select(filter_type, type_organism)%>%
  filter(type_organism %in% c("Fish", "Not target", "Bacteria","Invertebrate", "Phytoplankton" ))

#Counting and filtering the 5 most common filter types
count_ft<- data_ft%>%
  group_by(filter_type)%>%
  dplyr::summarize(n())%>%
  drop_na()

data_ft<- data_ft%>%
  filter(filter_type %in% c("Polyethersulfone", "Polycarbonate", "Glass_Fiber", "Cellulose_Esters", "Cellulose_Nitrate"))

data_ft$filter_type<- as.factor(data_ft$filter_type)
data_ft$type_organism<- as.factor(data_ft$type_organism)


#Counting papers
data_ft_summary<- data_ft %>%
  group_by(type_organism, filter_type) %>%
  dplyr::summarize(count_by_ft = n())%>%
  complete(filter_type, fill=list(count_by_ft=0))

#Side-by-side barplot: type of org ~ filter type

bar_toft<-ggplot(data = data_ft_summary, aes(x = type_organism, y= count_by_ft, fill = filter_type)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=alpha(c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"), 0.8), name=NULL,
                    labels = c("Cellulose Esters", "Cellulose Nitrate","Glass Fiber","Polycarbonate", "Polyethersulfone"))+
  scale_x_discrete(limits = c("Not target", "Fish", "Bacteria", "Phytoplankton","Invertebrate"))+
  geom_text(aes(label = count_by_ft), size=3, 
            position = position_dodge(0.9),
            color="black",vjust = 0,hjust = 0.5)+
  theme_classic() +
  xlab("")+
  ylab("Number of studies (n = 177)")+
  theme(legend.position=c(0.9, 0.9))+
  ggtitle("a)") +
  set

ggsave('barplot_typeorg_ft.pdf', bar_toft,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)

#### Barplot - filter type ~ system - 3b ####

#Separating papers with more than 1 system (one each row)
unique(data$system)

data_sy<- data%>%
  mutate(system = strsplit(as.character(system), ", ")) %>%
  unnest(system)
unique(data_sy$system)


#Selecting the 5 most freq filter type
data_sy<- data_sy%>%
  select(filter_type, system)%>%
  filter(filter_type %in% c("Polyethersulfone", "Polycarbonate", "Glass_Fiber", "Cellulose_Esters", "Cellulose_Nitrate"))

#Counting and filtering the 5 most common systems
count_sy<- data_sy%>%
  group_by(system)%>%
  dplyr::summarize(n())%>%
  drop_na()

data_sy<- data_sy%>%
  filter(system %in% c("River", "Coastal Ocean", "Lake", "Bay", "Coral reef"))

data_sy$filter_type<- as.factor(data_sy$filter_type)
data_sy$system<- as.factor(data_sy$system)

#Counting papers
data_sy_summary<- data_sy %>%
  group_by(system, filter_type) %>%
  dplyr::summarize(count_by_ft = n())%>%
  complete(filter_type, fill=list(count_by_ft=0))

#Side-by-side barplot: system ~ filter type

bar_syft<-ggplot(data = data_sy_summary, aes(x = system, y= count_by_ft, fill = filter_type)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=alpha(c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"), 0.8), name=NULL,
                    labels = c("Cellulose Esters", "Cellulose Nitrate","Glass Fiber","Polycarbonate", "Polyethersulfone"))+
  scale_x_discrete(limits = c("Coastal Ocean", "Coral reef", "Bay", "River","Lake"))+
  geom_text(aes(label = count_by_ft), size=3, 
            position = position_dodge(0.9),
            color="black",vjust = 0,hjust = 0.5)+
  theme_classic() +
  xlab("")+
  ylab("Number of studies (n = 140)")+
  theme(legend.position="none")+
  ggtitle("b)") +
  set

ggsave('barplot_sys_ft.pdf', bar_syft,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)

#### Pannel - filter type ~ organism and system ####
barplots<-bar_toft/bar_syft 

ggsave('fig_barplots_0102.pdf', barplots,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)

#### Barplot - extraction method ~ organism  - 3c ####

unique(data$DNA_extr)

data_extr_to<- data %>% #filtering only 5 most frequent extr method
  select(type_organism, DNA_extr)%>%
  filter(DNA_extr %in% c("QBT_Kit", "QPW_Kit", "QPS_Kit", "PCI", "CTAB")) #kits-only studies that followed manufacture protocol

#Counting and filtering the 5 most common organisms
count_to<- data_extr_to%>%
  group_by(type_organism)%>%
  dplyr::summarize(n())%>%
  drop_na()

data_extr_to<- data_extr_to%>%
  filter(type_organism %in% c("Fish", "Not target", "Bacteria","Invertebrate", "Protistan"))

data_extr_to$DNA_extr<- as.factor(data_extr_to$DNA_extr)
data_extr_to$type_organism<- as.factor(data_extr_to$type_organism)

#Counting papers
data_ex_to_summary<- data_extr_to %>%
  group_by(type_organism, DNA_extr) %>%
  dplyr::summarize(count_by_ext = n())%>%
  complete(DNA_extr, fill=list(count_by_ext=0))

#Side-by-side barplot: type organism ~ DNA extraction method

bar_ext1<-ggplot(data = data_ex_to_summary, aes(x = type_organism, y= count_by_ext, fill = DNA_extr)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=alpha(c("lightskyblue2","#b3cde3","#8c96c6","#8856a7","#810f7c"), 0.8), name=NULL,
                    labels=c("CTAB", "PCI", "QBT Kit", "QPS Kit", "QPW Kit"))+
  scale_x_discrete(limits = c("Not target", "Fish", "Bacteria","Invertebrate", "Protistan"))+
  geom_text(aes(label = count_by_ext), size=3, 
            position = position_dodge(0.9),
            color="black",vjust = 0,hjust = 0.5)+
  theme_classic() +
  xlab("")+
  ylab("Number of studies (n = 129)")+
  theme(legend.position=c(0.9, 0.9))+
  #theme(legend.position = "top")+
  ggtitle("a)") +
  set

ggsave('barplot_ext_org.pdf', bar_ext1,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)

#### Barplot - extraction method ~ system - 3c ####


#Separating papers with more than 1 system (one each row)
unique(data$system)

data_ext_sy<- data%>%
  mutate(system = strsplit(as.character(system), ", ")) %>%
  unnest(system)
unique(data_ext_sy$system)

data_ext_sy<- data_ext_sy %>% #filtering only 5 most frequent extr method
  select(system, DNA_extr)%>%
  filter(DNA_extr %in% c("QBT_Kit", "QPW_Kit", "QPS_Kit", "PCI", "CTAB")) #kits-only studies that followed manufacture protocol

#Counting and filtering the 5 most common systems
count_sy<- data_ext_sy%>%
  group_by(system)%>%
  dplyr::summarize(n())%>%
  drop_na()

data_ext_sy<- data_ext_sy%>%
  filter(system %in% c("River", "Coastal Ocean", "Lake", "Bay", "Open Ocean"))

data_ext_sy$DNA_extr<- as.factor(data_ext_sy$DNA_extr)
data_ext_sy$system<- as.factor(data_ext_sy$system)

#Counting papers
data_ext_sy_summary<- data_ext_sy %>%
  group_by(system, DNA_extr) %>%
  dplyr::summarize(count_by_ext = n())%>%
  complete(DNA_extr, fill=list(count_by_ext=0))

#Side-by-side barplot: type organism ~ DNA extraction method

bar_ext2<-ggplot(data = data_ext_sy_summary, aes(x = system, y= count_by_ext, fill = DNA_extr)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=alpha(c("lightskyblue2","#b3cde3","#8c96c6","#8856a7","#810f7c"), 0.8), name=NULL,
                    labels=c("CTAB", "PCI", "QBT Kit", "QPS Kit", "QPW Kit"))+
  scale_x_discrete(limits = c("Open Ocean", "Coastal Ocean", "Bay", "River", "Lake"))+
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

ggsave('barplot_ext_sys.pdf', bar_ext2,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)

#### Pannel - extraction method ~ organism and system ####
barplots2<-bar_ext1/bar_ext2

ggsave('fig_barplots_ext_0102.pdf', barplots2,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)

#### Barplot - loci ~ organism ####
#Separating papers with more than 1 filter size (one each row)
unique(data$loci)

data_loci<- data%>%
  mutate(loci = strsplit(as.character(loci), ", ")) %>%
  unnest(loci)
unique(data_loci$loci)

#Selecting the 5 most freq organisms
data_loci<- data_loci%>%
  select(loci, type_organism)%>%
  filter(type_organism %in% c("Fish", "Not target", "Bacteria","Invertebrate", "Phytoplankton" ))

#Counting and filtering the 5 most common loci
count_loci<- data_loci%>%
  group_by(loci)%>%
  dplyr::summarize(n())%>%
  drop_na()

data_loci<- data_loci%>%
  filter(loci %in% c("12S", "16S", "18S", "COI", "cytb"))

data_loci$type_organism<- as.factor(data_loci$type_organism)
data_loci$loci<- as.factor(data_loci$loci)

#Counting papers
data_loci_summary<- data_loci %>%
  group_by(loci,type_organism) %>%
  dplyr::summarize(count_by_loci = n())%>%
  complete(type_organism, fill=list(count_by_loci=0))

#Side-by-side barplot: type of org ~ filter type

barplot_loci<-ggplot(data = data_loci_summary, aes(x = type_organism, y= count_by_loci, fill = loci)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=alpha(c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"), 0.8), name=NULL,
                    labels = c("12S", "16S", "18S", "COI", "cytb"))+
  scale_x_discrete(limits = c("Not target", "Fish", "Bacteria", "Phytoplankton","Invertebrate"))+
  geom_text(aes(label = count_by_loci), size=3, 
            position = position_dodge(0.9),
            color="black",vjust = 0,hjust = 0.5)+
  theme_classic() +
  xlab("")+
  ylab("Number of studies (n = 331)")+
  theme(legend.position="top")+
  set

ggsave('barplot_typeorg_loci.pdf', barplot_loci,
       width = 21, height = 13, units = c('cm'),
       dpi = 600)


