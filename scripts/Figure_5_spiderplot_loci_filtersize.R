#######################################################
#### Figure 5 - Spider plot - loci and filter size ####
#######################################################

##Required packages
library(ggplot2)
library(fmsb)
library(tidyverse)
library(scales)

##General data setup
setwd("/Users/ingridbunholi/Desktop/eDNA_RNA_meta/metadata") #working directory
data<- read.csv("eDNA_RNA_meta_0515.csv") #metadata


#### Spider plot - by filter size ####

#Adjusting and Separating papers with more than 1 filter size (one each row)
data_fs<- data%>%
  mutate(filter_size_µm = strsplit(as.character(filter_size_µm), ", ")) %>%
  unnest(filter_size_µm)
unique(data_fs$filter_size_µm)

data_fs<- data_fs%>%
  mutate(filter_size_µm = replace(filter_size_µm, filter_size_µm=="<0.22", "≤0.22"))
unique(data_fs$filter_size_µm)

data_fs<- data_fs%>%
  mutate(filter_size_µm = replace(filter_size_µm, filter_size_µm=="0.4_1", ">0.22-1"))
unique(data_fs$filter_size_µm)


#Filtering data - only 6 most frequent organisms and counting
data_fs_f<- data_fs%>%
  select(filter_size_µm, type_organism)%>%
  filter(type_organism %in% c("Fish", "Non target", "Bacteria","Invertebrate", "Phytoplankton", "Protist"))%>%
  drop_na()

#Counting papers
data_fs_summary<- data_fs_f %>%
  group_by(type_organism, filter_size_µm) %>%
  summarize(count_by_fs = n())

#Reshaping data - long to wide
data_fs_rs<- data_fs_summary %>%
  spread(key=type_organism, value=count_by_fs) %>%
  replace(is.na(.), 0) 

#Adjusting before plotting
data_fs_rs <- data_fs_rs %>%
  remove_rownames %>% 
  column_to_rownames(var="filter_size_µm")
data_fs_rs<- data_fs_rs [c(1,3,2),] 
# Adding maximum and minimum to the data 
data_fs_spi <- rbind(rep(80,5), rep(0,5), data_fs_rs)

#Plotting spider plot by filter size
colors_bord<- c("#E7B800","#00AFBB", "#FC4E07")
colors_in<- c("#E7B800","#00AFBB","#FC4E07")

pdf(file = "spideplot_filter_size_organism_test1.pdf", width = 10, height = 9)

radarchart(data_fs_spi, axistype=1, 
           #custom polygon
           pcol=colors_bord, pfcol=scales::alpha(colors_in, 0.1), plwd=3 , plty=1,
           #custom the grid
           cglcol="black", cglty=3, axislabcol="black", caxislabels=seq(0,80,20), cglwd=1.2,
           #custom labels
           vlcex=1)
legend(x="bottom", legend = rownames(data_fs_spi[-c(1,2),]), 
                  horiz = TRUE, bty = "n", pch=20 , col=colors_in , 
                  text.col = "black", cex=1.2, pt.cex=2)

dev.off()

#### Spider plot - by loci ####

#Separating papers with more than 1 loci (one each row)
data_loci<- data%>%
  mutate(loci = strsplit(as.character(loci), ", ")) %>%
  unnest(loci)
unique(data_loci$loci)

#Filtering data - only 6 most frequent organisms and loci
data_loci<- data_loci%>%
  select(loci, type_organism)%>%
  filter(type_organism %in% c("Fish", "Non target", "Bacteria","Invertebrate", "Phytoplankton", "Protist"))%>%
  filter(loci %in% c("12S", "16S", "18S", "COI"))%>%
  drop_na()

#Counting papers
data_loci_summary<- data_loci %>%
  group_by(type_organism, loci) %>%
  summarize(count_by_loci = n())

#Reshaping data - long to wide
data_loci_rs<- data_loci_summary %>%
  spread(key=type_organism, value=count_by_loci) %>%
  replace(is.na(.), 0) 

#Adjusting before plotting
data_loci_rs <- data_loci_rs %>%
  remove_rownames %>% 
  column_to_rownames(var="loci")
# Adding maximum and minimum to the data 
data_loci_spi <- rbind(rep(100,5), rep(0,5), data_loci_rs)

#Spider plot by filter size
colors_bord<- c("#00AFBB", "#E7B800", "#FC4E07", "#8856a7")
colors_in<- c("#00AFBB", "#E7B800", "#FC4E07", "#8856a7")

pdf(file = "spideplot_loci_test3.pdf", width = 10, height = 9)

radarchart(data_loci_spi, axistype=1, 
           #custom polygon
           pcol=colors_bord, pfcol=scales::alpha(colors_in, 0.1), plwd=3 , plty=1,
           #custom the grid
           cglcol="black", cglty=3, axislabcol="black", caxislabels=seq(0,100,25), cglwd=1.2,
           #custom labels
           vlcex=1)
legend(x="bottom", legend = rownames(data_loci_spi[-c(1,2),]), 
       horiz = TRUE, bty = "n", pch=20 , col=colors_in , 
       text.col = "black", cex=1.5, pt.cex=2)

dev.off()


