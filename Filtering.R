library(dplyr)
library(tidyverse)

#reads in data
STA1Air <- read_csv("HBEF_STA1airTemp_15min.csv")
STA23Air <- read_csv("HBEF_STA23AirTemp_15min.csv")
StreamTemp <- read_csv("HBEF_streamtemp_roughlyCleaned.csv")

#combines the two air temperatures into one and deletes the exta column
allairtemps <- bind_rows(STA1Air, STA23Air)
allairtemps <- select(allairtemps,-FLAG)

#filters the air temperatures hourly
allairtemps <- allairtemps %>%
  filter(format(DateTime, "%M") == "00")

#filters the stream temperatures hourly
streamtemps <- StreamTemp %>%
  filter(format(TIMESTAMP, "%M") == "00")

prac <- allairtemps |> 
  group_by(STA) |> 
  pivot_wider(names_from = STA, values_from = airTemp)

test <- left_join(streamtemps, prac, by = c("TIMESTAMP" = "DateTime"))
