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



# Set up loop that will name peaks across all spectra

# First, create a list of data frames to loop over

peak_datL = peak_dat %>% 
  filter(
    !str_detect(DataFileName, "M1M2") 
    & !str_detect(DataFileName, "FAME")
  ) %>% 
  mutate(name = NA)

peak_datL = split(
  peak_datL, 
  peak_datL$DataFileName)

for(i in names(peak_datL)){
  
  # 13:0
  thirteen = peak_datL[[i]] %>% 
    filter(
      MajorHeightnA >= 0.4 * max(MajorHeightnA) &
        RetTimeSecs > 250
    )
  thirteen_secs = min(thirteen$RetTimeSecs)
  peak_datL[[i]]$name[peak_datL[[i]]$RetTimeSecs == thirteen_secs] = "13:0"
  
  # 15:0 ISO
  fifteenISO = peak_datL[[i]] %>% 
    filter(
      RetTimeSecs > thirteen_secs &
      RetTimeSecs > 1500 & MajorHeightnA >= 0.01 * max(MajorHeightnA)
      # Ret Time is at least 1500 secs AND peak height is at least 1% of the max peak height 
    )
  fifteenISO_secs = min(fifteenISO$RetTimeSecs)
  peak_datL[[i]]$name[peak_datL[[i]]$RetTimeSecs == fifteenISO_secs] = "15:0 ISO"
  
  # Fifteen ANTE ISO
  fifteenANTE = peak_datL[[i]] %>% 
    filter(
      RetTimeSecs > 1500 & MajorHeightnA >= 0.01 * max(MajorHeightnA)
      # Ret Time is at least 1500 secs AND peak height is at least 1% of the max peak height 
    ) %>% 
    filter(
      RetTimeSecs > fifteenISO_secs
    )
  fifteenANTE_secs = min(fifteenANTE$RetTimeSecs)
  peak_datL[[i]]$name[peak_datL[[i]]$RetTimeSecs == fifteenANTE_secs] = "15:0 ANTE ISO"
  
  # 15:0 (lil bb)
  fifteen = peak_datL[[i]] %>% 
    filter(
      RetTimeSecs > fifteenANTE_secs &
      RetTimeSecs > 1600 & RetTimeSecs < 1700
    )
  fifteen_secs = fifteen$RetTimeSecs[fifteen$area_ht==max(fifteen$area_ht)]
  peak_datL[[i]]$name[peak_datL[[i]]$RetTimeSecs == fifteen_secs] = "15:0"
  
  # 16:1w9c
  sixteen_1w9c = peak_datL[[i]] %>% 
    filter(
      RetTimeSecs > 1800 & MajorHeightnA >= 0.005 * max(MajorHeightnA)
    )
  sixteen_1w9c_secs = min(sixteen_1w9c$RetTimeSecs)
  peak_datL[[i]]$name[peak_datL[[i]]$RetTimeSecs == sixteen_1w9c_secs] = "16:1w9c"
  
  # 16:1w7c
  sixteen_1w7c = peak_datL[[i]] %>% 
    filter(
      RetTimeSecs > 1800 & MajorHeightnA >= 0.005 * max(MajorHeightnA)
    ) %>% 
    filter(RetTimeSecs > sixteen_1w9c_secs)
  sixteen_1w7c_secs = min(sixteen_1w7c$RetTimeSecs)
  peak_datL[[i]]$name[peak_datL[[i]]$RetTimeSecs == sixteen_1w7c_secs] = "16:1w7c"
  
  # 16:1w5c
  sixteen_1w5c = peak_datL[[i]] %>% 
    filter(
      RetTimeSecs > 1800 & MajorHeightnA >= 0.005 * max(MajorHeightnA)
    ) %>% 
    filter(RetTimeSecs > sixteen_1w7c_secs)
  sixteen_1w5c_secs = min(sixteen_1w5c$RetTimeSecs)
  peak_datL[[i]]$name[peak_datL[[i]]$RetTimeSecs == sixteen_1w5c_secs] = "16:1w5c"
  
  # 16:0 
  sixteen = peak_datL[[i]] %>% 
    filter(
        RetTimeSecs < 2100 & RetTimeSecs > 1900
    )
  sixteen_secs = sixteen$RetTimeSecs[sixteen$MajorHeightnA==max(sixteen$MajorHeightnA)]
  peak_datL[[i]]$name[peak_datL[[i]]$RetTimeSecs == sixteen_secs] = "16:0"
  
  # 16:0 10me
  sixteen_10me = peak_datL[[i]] %>% 
    filter(
        MajorHeightnA > 0.2 * peak_datL[[i]]$MajorHeightnA[peak_datL[[i]]$name=="16:0"]
    )
  sixteen_10me_secs = min(sixteen_10me$RetTimeSecs)
  peak_datL[[i]]$name[peak_datL[[i]]$RetTimeSecs == sixteen_10me_secs] = "16:0 10me"
}

peak_dat2 = peak_datL %>% 
  bind_rows()

# Notes from 5/16 ###
# First tested 22_raw - 15:0 is at 1730.6 (from the loop) rather than 1678.9 (where it actually is)
    # Then changed code to pick peak with largest area before 1700 - problem solved
# 35_raw all points correct up to 16:0

# Notes from 5/19 
# Need to add if statements to deal with spectra that don't have all peaks
# Need to figure out why 16:0 10me is not working