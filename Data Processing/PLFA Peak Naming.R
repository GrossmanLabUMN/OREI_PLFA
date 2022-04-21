# METADATA -----------------
# PLFA Naming Attempt #1 by EM, TS, & RF

library(tidyverse)
library(readxl)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

raw_dat = read_xlsx(
  "../Raw data/20181029_GC_Gutknecht_FL_Batch1_again_text only.xlsx",
  sheet = "F1CO2-log",
  skip = 50
) %>% 
  dplyr::select(Index, DataFileName, RetTimeSecs, MajorHeightnA)

peak_dat = read_xlsx(
  "../Raw data/20181029_GC_Gutknecht_FL_Batch1_again_text only.xlsx",
  sheet = "Data for Naming"
) %>% 
  mutate(area_ht = TotalPeakArea1 / MajorHeightnA)

# raw_datL = split(raw_dat, by = "DataFileName")

raw_practice = peak_dat %>% 
  filter(DataFileName == "98.raw")

# Get the 13:0 identified

thirteen = raw_practice %>% 
  filter(
    MajorHeightnA >= 0.4 * max(MajorHeightnA) &
      RetTimeSecs > 250
  )
min(thirteen$RetTimeSecs)

# 16:0 

sixteen = raw_practice %>% 
  filter(
    RetTimeSecs < 2100 & RetTimeSecs > 1900
  )
sixteen$RetTimeSecs[sixteen$MajorHeightnA==max(sixteen$MajorHeightnA)]

# 15:0 ISO

fifteenISO = raw_practice %>% 
  filter(
    RetTimeSecs > 1500 & MajorHeightnA >= 0.01 * max(MajorHeightnA)
    # Ret Time is at least 1500 secs AND peak height is at least 1% of the max peak height 
  )
min(fifteenISO$RetTimeSecs)

# Fifteen ANTE ISO

fifteenANTE = raw_practice %>% 
  filter(
    RetTimeSecs > 1500 & MajorHeightnA >= 0.01 * max(MajorHeightnA)
    # Ret Time is at least 1500 secs AND peak height is at least 1% of the max peak height 
  ) %>% 
  filter(
    RetTimeSecs != min(.$RetTimeSecs)
  )
min(fifteenANTE$RetTimeSecs)

# 15:0 (lil bb)

fifteen = raw_practice %>% 
  filter(
    RetTimeSecs > 1600 & RetTimeSecs < 1750
  )
fifteen$RetTimeSecs[fifteen$area_ht==max(fifteen$area_ht)]

# 16:1w9c

sixteen_1w9c = raw_practice %>% 
  filter(
    RetTimeSecs > 1800 & MajorHeightnA >= 0.005 * max(MajorHeightnA)
  )
min(sixteen_1w9c$RetTimeSecs)

# 16:1w7c

sixteen_1w7c = raw_practice %>% 
  filter(
    RetTimeSecs > 1800 & MajorHeightnA >= 0.005 * max(MajorHeightnA)
  ) %>% 
  filter(
    RetTimeSecs != min(sixteen_1w9c$RetTimeSecs)
  )
min(sixteen_1w7c$RetTimeSecs)

# 16:1w5c

sixteen_1w5c = raw_practice %>% 
  filter(
    RetTimeSecs > 1800 & MajorHeightnA >= 0.005 * max(MajorHeightnA)
  ) %>% 
  filter(
    RetTimeSecs != min(sixteen_1w9c$RetTimeSecs) &
    RetTimeSecs != min(sixteen_1w7c$RetTimeSecs) 
      
  )
min(sixteen_1w5c$RetTimeSecs)
