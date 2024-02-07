library(dplyr)
library(tidyverse)

#Set personal working directory
#setwd("C:/EDS Capstone 2024/2024_VTCapstone_Diel/200_Data/RAW_data")

#Download Data
STA1Air <- read.csv("HBEF_STA1airTemp_15min.csv")
STA23Air <- read.csv("HBEF_STA23airTemp_15min.csv")
StreamTemp <- read.csv("HBEF_streamtemp_roughlyCleaned.csv")

#Combines the two air temperature files into one and deletes the extra column 
allairtemps <- bind_rows(STA1Air, STA23Air)
allairtemps <- select(allairtemps,-FLAG)

#Formats the datetime for air temp correctly
allairtemps$DateTime <- as.POSIXct(allairtemps$DateTime, format = "%Y-%m-%d %H:%M:%S")

#Filters the air temperature file hourly
allairtemps <- allairtemps %>%
  filter(format(DateTime, "%M") == "00")

#Formates the datetime for streams correctly 
StreamTemp$TIMESTAMP <- as.POSIXct(StreamTemp$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")

#Filters the stream temperature file hourly 
streamtemps <- StreamTemp %>%
  filter(format(TIMESTAMP, "%M") == "00")

#combining the data sets
tempscombined <- streamtemps %>%
  pivot_longer(cols = starts_with("Streamtemp_"),
               names_to = "Location") %>%
  mutate(streamtemps = case_when(    
    Location %in% c("Streamtemp_W1", "Streamtemp_W2", "Streamtemp_W3", "Streamtemp_W4", "Streamtemp_W5", "Streamtemp_W6") ~ "STA_1",   
    Location %in% c("Streamtemp_W7", "Streamtemp_W8", "Streamtemp_W9") ~ "STA_23"  
  )) 

#joins data sets
alldata <- left_join(tempscombined, allairtemps, by=c('TIMESTAMP'='DateTime', 'streamtemps'='STA'))

#renames columns and drops extra column
datasetrenamed <- rename(alldata,c('StreamTemp'='value', 'AirTemp'='airTemp')) 
dataset <- select(datasetrenamed, -streamtemps)  %>%
filter(between(TIMESTAMP,as.Date('2015-05-19'),as.Date('2022-09-12')))

write_csv(dataset,"combinedData.csv")



