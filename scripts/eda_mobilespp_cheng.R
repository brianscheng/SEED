#using shoals intertidal dataset, perform exploratory data analysis on mobile species
#source file loaded on Oct 31, 2020 from https://environmentaldatainitiative.org/edis-featured-data-contributions/rocky-intertidal-monitoring-appledore/

library(tidyverse)
library(scales)
library(lubridate)
library(here)
here<-here::here #bc of conflicts with lubridate

#data importing - count data has all mobile species within quadrats
data<-read.csv(file = here("data/intertidal/counts_data.csv"), na.strings = "NA")

#metadata
str(data)
summary(data$Year)      #1982 to 2017 coverage, KB says 2018 and 2019 are coming
summary(data$Transect)  #1-28 transects
summary(data$Level)     #-4 to 19, measured in feet below the pin (located at top of transect), negative value is feet above pin
summary(data$Replicate) #replicate quadrat (1-3 in documentation but we have 1a/b and 2a/b and 4?)
summary(data$Data_taken)#factor, no, yes, NA
summary(data$Organism)  #factor, species and counts throughout entire dataset
summary(data$Count)     #factor, lots of numbers here, but then others and NAs, how is NA specified?
length(which(data$Count=="p"))                   #1501 occurrences
length(which(data$Count=="sp100"))               #one occurrence
length(which(data$Count=="casings present"))     #one occurrence
length(which(data$Count=="0.5")) #one occurrence #one occurrence

#data wrangling
data2<-  filter(data, Count != "p", Count != "sp100", Count != "casings present") #unsure why data2 is returning 450150 rows?
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

#mobile species showing declines in Petraitis and Dudgeon 2020
#L. littorea, M. edulis, S. balanoides, N. lapillus, T. testudinalis, L. obtusata not declining
#focal mobile species
data3 <-filter(data2, Organism == "Nucella lapillus"| Organism == "Littorina littorea" |
                 Organism == "Tectura testudinalis" | Organism == "Littorina obtusata" |
                 Organism == "Mytilus edulis" | Organism == "Semibalanus balanoides")
data3$Organism<-factor(data3$Organism)

mean         = t(tapply(data3$Count2, list(data3$Organism,factor(data3$Year)), mean))
summary      = data.frame(mean)
summary$Year = as.numeric(row.names(mean))
summary      = gather(summary,Organism,mean, Littorina.littorea:Tectura.testudinalis)

sd           = tapply(data3$Count2, list(data3$Organism,factor(data3$Year)), sd)
n            = tapply(data3$Count2, list(data3$Organism,factor(data3$Year)), length)
sem          = data.frame(t(sd/sqrt(n)))
sem$Year     = as.numeric(row.names(sem))
sem          = gather(sem,Organism,sem, Littorina.littorea:Tectura.testudinalis)

summary$sem<-sem$sem

plot1<-ggplot(summary,(aes(x=Year,y=mean)))+geom_point()+geom_errorbar(aes(ymin=mean-sem, ymax = mean+sem), width=0)+
  theme_bw()+theme(axis.text=element_text(size=22),axis.title=element_text(size=24), strip.text.y = element_text(size = 18))+ 
  labs(y="Mean count per quadrat(+/- SEM)")+geom_smooth(method="loess")+facet_grid(Organism~., scales="free_y")+
  scale_x_continuous(limits = c(1980,2020), breaks = c(1980, 1990, 2000, 2010, 2020))
plot1

ppi=300 #define a pixels per inch term
png(here("figures/mobile_foundation_spp.png"),width=8*ppi, height=16*ppi, res=ppi) #define your png file
plot1
dev.off()


