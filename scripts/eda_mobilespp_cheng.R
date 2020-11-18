#using shoals intertidal dataset, perform exploratory data analysis on mobile species
#source file loaded on Oct 31, 2020 from https://environmentaldatainitiative.org/edis-featured-data-contributions/rocky-intertidal-monitoring-appledore/

library(tidyverse)
library(lubridate)
library(here)
here<-here::here #bc of conflicts with lubridate

#data importing - count data has all mobile species within quadrats
data<-read.csv(file = here("data/intertidal/counts_data.csv"), na.strings = "NA")

#metadata
str(data)
summary(data$Year)      #1982 to 2017 coverage, KB says 2018 and 2019 are coming
summary(data$Transect)  #1-28 transects
summary(data$Replicate) #factor, unsure what this variable is?
summary(data$Data_taken)#factor, no, yes, NA
summary(data$Organism)  #factor, species and counts throughout entire dataset
summary(data$Count)     #factor, lots of numbers here, but then others and NAs, how is NA specified?


#data wrangling
data2<-  filter(data, Count != "p", Count != "sp100", Count != "casings present")
data2$Count2<-as.numeric(as.character(data2$Count))
str(data2)
test<-as.numeric(as.character("p")) #classifies as NA?
class(test)
test

#questions
count.list<-unique(data2$Count)
count.list
#1)Count has NA, but also "p", "casings present", "sp100"
#2)Count also has a 0.5???
#3)Are NA = 0?
#4)What is a replicate?


#mobile species showing declines in Petraitis and Dudgeon 2020
#L. littorea, M. edulis, S. balanoides, N. lapillus, T. testudinalis, L. obtusata not declining
Nucella <-filter(data2, Organism == "Nucella lapillus")
str(Nucella)
Nucella$Count2<-as.numeric(as.character(Nucella$Count)) #behaves correctly!
#Nucella$Count2<-as.numeric(Nucella$Count) #returns really fucked up behavior
hist(Nucella$Count2)

ggplot(Nucella, aes(x=factor(Year),y=Count2))+geom_boxplot()
ggplot(Nucella, aes(x=factor(Transect),y=Count2))+geom_boxplot()
ggplot(Nucella, aes(x=factor(Replicate),y=Count2))+geom_boxplot()

