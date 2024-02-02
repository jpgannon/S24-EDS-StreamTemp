library(dplyr)
library(tidyverse)

#combines the two air temperatures into one and deletes the exta column
allairtemps <- bind_rows(HBEF_STA1airTemp_15min, HBEF_STA23airTemp_15min)
allairtemps <- select(allairtemps,-FLAG)

#filters the air temperatures hourly
allairtemps <- allairtemps %>%
  filter(format(DateTime, "%M") == "00")

#filters the stream temperatures hourly
streamtemps <- HBEF_streamtemp_roughlyCleaned %>%
  filter(format(TIMESTAMP, "%M") == "00")
