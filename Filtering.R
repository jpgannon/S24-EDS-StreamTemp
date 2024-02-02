library(dplyr)
library(tidyverse)

allairtemps <- bind_rows(HBEF_STA1airTemp_15min, HBEF_STA23airTemp_15min)
allairtemps <- select(allairtemps,-FLAG)

allairtemps <- allairtemps %>%
  filter(format(DateTime, "%M") == "00")

streamtemps <- HBEF_streamtemp_roughlyCleaned %>%
  filter(format(TIMESTAMP, "%M") == "00")
