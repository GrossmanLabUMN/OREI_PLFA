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
)

# raw_datL = split(raw_dat, by = "DataFileName")

raw_practice = raw_dat %>% 
  filter(DataFileName == "22.raw")

# Get the 13:0 identified

thirteen = raw_practice %>% 
  filter(
    MajorHeightnA >= 0.4 * max(MajorHeightnA) &
      RetTimeSecs > 250
  )
min(thirteen$RetTimeSecs)
