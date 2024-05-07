## LOAD PACKAGES ----

library(tidyverse)

## LOAD DATA ----

voters_pred <- readRDS("data/race_pred/pred_voters.rds")

# read in list of chinese surnames
chinese_surnames <- str_trim(scan("data/race_pred/quan_chinese_surnames.txt", what = "character", sep = ","))

## IDENTIFY HIGH-PROBABILITY CHINESE VOTERS ----

asi_threshold <- 0.9 # set probability threshold of being Asian

# filter for matched chinese surname
# with telephone number or email address
# NOT registered dem
chinese_non_dems <- voters_pred %>% 
  filter(((pred.asi > asi_threshold) 
          & (political_party != "DEM") 
          & (surname %in% chinese_surnames))
         & (!is.na(telephone_optional) | !is.na(email_address)))

# registered dem
chinese_dems <- voters_pred %>% 
  filter(((pred.asi > asi_threshold) 
          & (political_party == "DEM") 
          & (surname %in% chinese_surnames))
         & (!is.na(telephone_optional) | !is.na(email_address)))

## OUTPUT ----

chinese_non_dems %>% 
  select(!c(state, county, tract, block, latitude, longitude, starts_with("pred"))) %>% 
  write_csv("data/race_pred/chinese_non_dem_voters.csv")
# 24,685 voters

chinese_dems %>% 
  select(!c(state, county, tract, block, latitude, longitude, starts_with("pred"))) %>% 
  write_csv("data/race_pred/chinese_dem_voters.csv")
# 26,624 voters