library(dplyr)
library(tidyverse)

#reads in data
STA1Air <- read_csv("HBEF_STA1airTemp_15min.csv")
STA23Air <- read_csv("HBEF_STA23AirTemp_15min.csv")
StreamTemp <- read_csv("HBEF_streamtemp_roughlyCleaned.csv")

#combines the two air temperatures into one and deletes the extra column
allairtemps <- bind_rows(STA1Air, STA23Air)
allairtemps <- select(allairtemps,-FLAG)

#filters the air temperatures hourly
allairtemps <- allairtemps %>%
  filter(format(DateTime, "%M") == "00")

#filters the stream temperatures hourly
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
dataset <- select(datasetrenamed, -streamtemps)
