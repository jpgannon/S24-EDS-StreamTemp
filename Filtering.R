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

tempscombined <- streamtemps %>%
  pivot_longer(cols = starts_with("Streamtemp"),
               names_to = 'Location') %>%
  mutate(streamtemps = case_when(
    Location = Streamtemp_W1 ~ STA_1,
    Location = Streamtemp_W2 ~ STA_1,
    Location = Streamtemp_W3 ~ STA_1,
    Location = Streamtemp_W4 ~ STA_1,
    Location = Streamtemp_W5 ~ STA_1,
    Location = Streamtemp_W6 ~ STA_1,
    Location = Streamtemp_W7 ~ STA_23,
    Location = Streamtemp_W8 ~ STA_23,
    Location = Streamtemp_W9 ~ STA_23
 )) %>%
  join_by(TIMESTAMP == DateTime)

    
