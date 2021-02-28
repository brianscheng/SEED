#### Cleaning Script for SML Intertidal Data #####

# Clear Working Directory ---------
rm(list=ls())

# Load packages---------------------
library('tidyverse')

# Set Working directory--------------
setwd("C:/Users/kylla/Documents/Research/R Projects/SEED")

# Load Data---------------------
pc_data = read.csv("percent_cover_data_v2.csv", header=TRUE, 
                   na.strings = c("NA"))

ct_data = read.csv("counts_data_v2.csv", header=TRUE, 
                   na.strings = c("NA"))

sizes_data = read.csv("sizes_data_v2.csv", header=TRUE, 
                   na.strings = c("NA"))

sw_sizes_data = read.csv("fucus_asco_max_size.csv", header=TRUE, 
                      na.strings = c("NA"))

# Data Clean Up ------------------

# in SML data set p = present (but unknown value) and
# nd = no data (but could have been collected)
# 'NA' = truly not applicable
# 'p' could be retained for biodiversity indices that use present/absent only

#### PERCENT COVER

# filtering data set to remove any instances of NA or nd or p 
# note, "p" should be kept in any biodiversity analyses
# also filtering out rows with errant notes in Percent_cover column
# all other Percent_cover entries are assumed real of observation of spp., converted to number if possible

unique(pc_data$Percent_cover) #view unique entries to find rows to filter out or replace

pc_filtered = pc_data %>%
  filter(Percent_cover != "p") %>%
  filter(Percent_cover != "nd") %>%
  filter(Percent_cover != "<1") %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "0. 5", 0.5)) %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "10, phymato", 10)) %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "10% (est'd canopy)", 10)) %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "15 (80 sub canopy)", 80)) %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "15% (est'd canopy)", 15)) %>%
  filter(Percent_cover != "1patch") %>%
  filter(Percent_cover != "1stipe") %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "25% (est'd canopy)", 25)) %>%
  filter(Percent_cover != "2patches") %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "34 phymato", 34)) %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "43 phymato", 43)) %>%
  filter(Percent_cover != "50%Sb") %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "60 phymato", 60)) %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "65 phymato", 65)) %>%
  filter(Percent_cover != "6stipes") %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "70 phymato", 70)) %>%
  filter(Percent_cover != "if a species is not listed to the left, please put it in the notes column. Thanks!") %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "upper 15.6%/substrate 1.2%", 15.6))

unique(pc_filtered$Percent_cover) #check for any missed
pc_filtered["Percent_cover"]<- as.numeric(pc_filtered[,"Percent_cover"]) #change to numeric variable
is.numeric(pc_filtered$Percent_cover)


#### COUNTS

# filtering data set to remove any instances of NA or nd or p 
# note, "p" should be kept in any biodiversity analyses
# also filtering out rows with errant notes in Count column

unique(ct_data$Count) #view unique entries to find rows to filter out or replace

ct_filtered = ct_data %>%
  filter(Count != "p") %>%
  filter(Count != "nd") %>%
  filter(Count != "casings present") %>%
  filter(Count != "sp100")

unique(ct_filtered$Count) #check for any missed
ct_filtered["Count"]<- as.numeric(ct_filtered[,"Count"]) #change to numeric variable
is.numeric(ct_filtered$Count)


#### INVERT SIZES

# filtering data set to remove any instances of NA or nd or p 
# note, "p" should be kept in any biodiversity analyses
# also filtering out rows with errant notes in Count column

unique(sizes_data$Count)

sz_filtered = sizes_data %>%
  filter(Count != "p") %>%
  filter(Count != "nd") %>%
  filter(Count != "12.74.8") %>%
  filter(Count != "Fspp")

unique(sz_filtered$Count) #check for any missed
sz_filtered["Count"]<- as.numeric(sz_filtered[,"Count"]) #change to numeric variable
is.numeric(sz_filtered$Count)

# size classes converted to 'date' - replace  

unique(sz_filtered$Size_class)

sz_filtered = sz_filtered %>%
  mutate(Size_class = replace(Size_class, Size_class == "20-Nov", "11-20")) %>%
  mutate(Size_class = replace(Size_class, Size_class == "10-Jun", "6-10")) %>%
  mutate(Size_class = replace(Size_class, Size_class == "5-Mar", "3-5"))

unique(sz_filtered$Size_class)


#### FUCUS & ASCOPHYLUM SIZES

# READ! -> many instances of more than one Fucus spp. observed in Fucus_maxlength column
# need to continue work on this dataframe (28FEB2021)

# filtering data set to remove any instances of NA or nd or p 
# note, "p" should be kept in any biodiversity analyses
# also filtering out rows with errant notes in a size column
# all other observations are assumed real of observation of spp., converted to number if possible

unique(sw_sizes_data$Asco_maxbladders)
unique(sw_sizes_data$Fucus_maxlength)
unique(sw_sizes_data$Asco_maxlength)

sw_filtered = sw_sizes_data %>%
  filter(Asco_maxbladders != "p") %>%
  filter(Asco_maxbladders != "nd") %>%
  filter(Asco_maxbladders != "none") %>%
  filter(Asco_maxbladders != "x") %>%
  mutate(Asco_maxbladders = replace(Asco_maxbladders, Asco_maxbladders == "0*", 0)) %>%
  filter(Fucus_maxlength != "nd") %>%
  filter(Fucus_maxlength != "NOT RECORDED") %>%
  mutate(Fucus_maxlength = replace(Fucus_maxlength, Fucus_maxlength == "105mm", 105)) %>%
  mutate(Fucus_maxlength = replace(Fucus_maxlength, Fucus_maxlength == "105mm", 105)) %>%
  filter(Asco_maxlength != "p") %>%
  filter(Asco_maxlength != "nd") %>%
  filter(Asco_maxlength != "51?") %>%
  mutate(Asco_maxlength = replace(Asco_maxlength, Asco_maxlength == "120mm", 120))

unique(sw_filtered$Asco_maxbladders)
unique(sw_filtered$Fucus_maxlength)
unique(sw_filtered$Asco_maxlength)



 
