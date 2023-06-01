############################
#### Summary statistics ####
############################

#Required packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)

##General data setup
setwd("/Users/ingridbunholi/Desktop/eDNA_RNA_meta/metadata") #working directory
data<- read.csv("eDNA_RNA_meta_0515.csv") #metadata


#Summary statistics - supplementary tables

#### Year eDNA and eRNA ####
unique(data$DNA_RNA)

data_y<- data%>%
  mutate(DNA_RNA = strsplit(as.character(DNA_RNA), ", ")) %>%
  unnest(DNA_RNA)
unique(data_y$DNA_RNA)

y<- data_y%>%
  group_by(year, DNA_RNA)%>%
  summarize(Count=n(),
            Percentage = n() / nrow(data_fs) * 100)

#### Continent ####
unique(data$continent)

data_c<- data%>%
  mutate(continent = strsplit(as.character(continent), ", ")) %>%
  unnest(continent)
unique(data_c$continent)

c1<- data_c%>%
  group_by(continent, DNA_RNA)%>%
  summarize(Count=n(),
            Percentage = n() / nrow(data_c) * 100)


#### Country ####
unique(data$country)

data_cy<- data%>%
  mutate(country = strsplit(as.character(country), ", ")) %>%
  unnest(country)
unique(data_cy$country)

c<- data_cy%>%
  group_by(country, DNA_RNA)%>%
  summarize(Count=n(),
            Percentage = n() / nrow(data_cy) * 100)

#### System ####

unique(data$system)

data_s<- data%>%
  mutate(system = strsplit(as.character(system), ", ")) %>%
  unnest(system)
unique(data_s$system)

s<- data_s%>%
  group_by(system)%>%
  summarize(Count=n(),
            Percentage = n() / nrow(data_s) * 100)

#### Organism ####

unique(data$type_organism)

data_o<- data%>%
  mutate(type_organism = strsplit(as.character(type_organism), ", ")) %>%
  unnest(type_organism)
unique(data_o$type_organism)

o<- data_o%>%
  group_by(type_organism)%>%
  summarize(Count=n(),
            Percentage = n() / nrow(data_o) * 100)

#### Filter type ####

unique(data_y$filter_type)

data_ft<- data_y%>%
  mutate(filter_type = strsplit(as.character(filter_type), ", ")) %>%
  unnest(filter_type)
unique(data_ft$filter_type)

ft<- data_ft%>%
  filter(DNA_RNA %in% ("DNA"))

ft<- ft%>%
  group_by(filter_type, DNA_RNA)%>%
  summarize(Count=n(),
            Percentage = n() / nrow(ft) * 100)

#### Filter size ####
unique(data$filter_size_µm)

data_fs<- data%>%
  mutate(filter_size_µm = strsplit(as.character(filter_size_µm), ", ")) %>%
  unnest(filter_size_µm)
unique(data_fs$filter_size_µm)

data_fs<- data_fs%>%
  mutate(type_organism = strsplit(as.character(type_organism), ", ")) %>%
  unnest(type_organism)
unique(data_fs$type_organism)

fo <- data_fs %>% 
  group_by(filter_size_µm, type_organism) %>% 
  summarize(count = n(),
    percentage = n() / nrow(data_fs) * 100)

data_nt<- data_fs %>%
  filter (type_organism %in% "Not target")

fo_not_target <- data_nt %>% 
  group_by(filter_size_µm, type_organism) %>% 
  summarize(count = n(),
            percentage = n() / nrow(data_nt) * 100)

data_fish<- data_fs %>%
  filter (type_organism %in% "Fish")

fo_fish <- data_fish %>% 
  group_by(filter_size_µm, type_organism) %>% 
  summarize(count = n(),
            percentage = n() / nrow(data_fish) * 100)


#### Extraction ####

unique(data_ex$DNA_extr)

data_ex<- data_y%>%
  mutate(DNA_extr = strsplit(as.character(DNA_extr), ", ")) %>%
  unnest(DNA_extr)
unique(data_ex$DNA_extr)

data_ex_dna<- data_ex%>%
  filter(DNA_RNA %in% c("DNA"))

ex_dna<- data_ex_dna%>%
  group_by(DNA_extr, DNA_RNA)%>%
  summarize(Count=n(),
            Percentage = n() / nrow(data_ex_dna) * 100)

data_ex_rna<- data_ex%>%
  filter(DNA_RNA %in% c("RNA"))

ex_rna<- data_ex_rna%>%
  group_by(DNA_extr, DNA_RNA)%>%
  summarize(Count=n(),
            Percentage = n() / nrow(data_ex_rna) * 100)

#### Markers ####
unique(data_y$loci)

data_l<- data_y%>%
  mutate(loci = strsplit(as.character(loci), ", ")) %>%
  unnest(loci)
unique(data_l$loci)

loci<- data_l%>%
  group_by(loci, DNA_RNA)%>%
  summarize(Count=n(),
            Percentage = n() / nrow(data_l) * 100)

#genetic marker per organism

data_lo<- data_l%>%
  mutate(type_organism = strsplit(as.character(type_organism), ", ")) %>%
  unnest(type_organism)
unique(data_lo$type_organism)

lo <- data_lo %>% 
  group_by(loci, type_organism) %>% 
  summarize(count = n(),
            percentage = n() / nrow(data_lo) * 100)

#### ID type ####

unique(data$id_type)

data_id<- data%>%
  mutate(id_type = strsplit(as.character(id_type), ", ")) %>%
  unnest(id_type)
unique(data_id$id_type)

id<- data_id%>%
  group_by(id_type)%>%
  summarize(Count=n(),
            Percentage = n() / nrow(data_id) * 100)

#genetic id per organism

data_ido<- data_id%>%
  mutate(type_organism = strsplit(as.character(type_organism), ", ")) %>%
  unnest(type_organism)
unique(data_ido$type_organism)

ido <- data_ido %>% 
  group_by(id_type, type_organism) %>% 
  summarize(count = n(),
            percentage = n() / nrow(data_ido) * 100)

#### Volume ####

data_rna<- data_y%>%
  filter(DNA_RNA %in% ("RNA"))

vol_rna<-data_rna %>%
  summarise(
    count = n(),
    mean = mean(volume_l, na.rm = TRUE),
    sd = sd(volume_l, na.rm = TRUE),
    median = median(volume_l, na.rm = TRUE),
    IQR = IQR(volume_l, na.rm = TRUE),
    max = max(volume_l, na.rm = TRUE),
    min = min(volume_l, na.rm = TRUE)
  )

data_dna<- data_y%>%
  filter(DNA_RNA %in% ("DNA"))

vol_dna<-data_dna %>%
  summarise(
    count = n(),
    mean = mean(volume_l, na.rm = TRUE),
    sd = sd(volume_l, na.rm = TRUE),
    median = median(volume_l, na.rm = TRUE),
    IQR = IQR(volume_l, na.rm = TRUE),
    max = max(volume_l, na.rm = TRUE),
    min = min(volume_l, na.rm = TRUE)
  )

#Volume per organism

data_dna<- data_dna%>%
  mutate(type_organism = strsplit(as.character(type_organism), ", ")) %>%
  unnest(type_organism)
unique(data_org$type_organism)

vol_dna_org<-data_dna %>%
  group_by(type_organism)%>%
  summarise(
    count = n(),
    mean = mean(volume_l, na.rm = TRUE),
    sd = sd(volume_l, na.rm = TRUE),
    median = median(volume_l, na.rm = TRUE),
    IQR = IQR(volume_l, na.rm = TRUE),
    max = max(volume_l, na.rm = TRUE),
    min = min(volume_l, na.rm = TRUE)
  )

