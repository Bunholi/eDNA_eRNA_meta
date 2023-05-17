This readme file was generated on 2023-05-16 by Ingrid Bunholi

This repository contains the code and data to reproduce all tables and figures presented in Bunholi et al. *"Environmental DNA and RNA in aquatic community ecology toward methodological standardization"* 


## General Information

Title of Dataset: eDNA_RNA_metadata

Name: Ingrid Bunholi ORCID: 0000-0001-5489-276X Email: ingridbunholi@gmail.com

Institution: University of Texas at Austin Marine Science Institute (UTMSI); Address: 750 Channel View Dr, Port Aransas, TX 78373

## Details
The elements of this project include: 

1) metadata: this folder includes raw metadata that is necessary to run the scripts
3) scripts: this folder includes all the scripts to generate the figures and summary statistics
2) output: this folder includes all outputs from the R-script - include figures and summary statistics 


## DATA & FILE OVERVIEW

### Metadata 
The metadata used to generate all figures and statistical metrics is eDNA_RNA_meta_0515.csv. This file contains every metric collected from the 300 papers: technique (eDNA or eRNA), aquatic environment (marine or freshwater), region (continent and country), target organism (e.g. fish, invertebrate, bacteria, non-target, etc), aquatic system (e.g. river, coastal ocean, bay, coral reef, etc), filtered water volume (L), filter specifications (material and pore size), extraction method, targeted genetic marker, and reported units of genetic diversity (ASV, OTU, and/or species).

### Scripts

All the figures were generated using functions from packages described below. 


## METHODOLOGICAL INFORMATION

*Description of methods used for collection/generation of data:* the data collection was performed through a Boolean literature search through the Web Of Science (https://www.webofscience.com/wos/woscc/basic-search) using the following search terms: (“eDNA” OR “environmental DNA” OR “eRNA” OR “environmental RNA”) AND ALL = (marine OR freshwater) AND ALL (communit* OR assembl*). Date of data collection: 2023-01-05

*Methods for processing the data:* the metadata was manually collected after deciding the target variables. Although we specified the search only to marine and freswater as well as community and assemblages, we noticed that the search still included papers not relevant for this review. Therefore, we did a manual inspection removing studies outside the scope of the review, such as eDNA collected from sources other than water, invasive species detection, species-specific studies, and reviews. For studies with multiple techniques, we treated each independently. We also standardized some variables across studies. For instance, for filter pore size we created three categories: less than or equal to 0.22 µm, greater than 0.22 and less than or equal to 1 µm, and greater than 1 µm. For multiple volumes of water collected, we calculated the mean. To facilitate some analyses, we transformed some categorical variables into binary data.

*Instrument- or software-specific information needed to interpret the data:* we used the statistical software R version 4.2.1 for all data analysis and visualization. Necessary packages to run the scripts: tidyverse, ggplot2, dplyr, viridis, packcircles, maps, treemap, RColorBrewer, patchwork, ggdist, gghalves, fmsb, scales, limma. 

Please contact me for any issue or question (ingridbunholi@gmail.com)
