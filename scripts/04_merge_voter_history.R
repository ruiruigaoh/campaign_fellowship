## LOAD PACKAGES ----

library(tidyverse)
library(vroom)

## READ IN DATA ----

# processed voter history file (from erin)
voter_history <- vroom("data/voting_history_primarycount20to23.csv", delim = ",")

# voter file record with predicted races
voter_demogs <- vroom("data/race_pred/pred_voters_for_erin.csv", delim = "\t")

## DATA PROCESSING ----

# Specify the columns for which you want to find the maximum
cols_to_check <- c("pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth")

# Get the column index of the maximum value in each row across specified columns
max_col_index <- max.col(voter_demos[cols_to_check])
max_col_index

# Create a new column to flag which column contains the maximum value
voter_demos$max_prob_race <- cols_to_check[max_col_index]

# create column race_est
voter_demos  <- voter_demos %>%
  mutate(race_est = case_when(
    str_detect(max_prob_race, "\\.whi$") ~ "white",
    str_detect(max_prob_race, "\\.asi$") ~ "asian",
    str_detect(max_prob_race, "\\.his$") ~ "hispanic",
    str_detect(max_prob_race, "\\.bla$") ~ "black",
    str_detect(max_prob_race, "\\.oth$") ~ "other",
  )) 

# Clean up unneeded columns
voter_demos <- voter_demos %>% 
  select(!contains(c("pred", "max")))

# Perform merge
voters_merged <- full_join(voter_history, voter_demos, by = "county_emsid") %>% 
  relocate(pr_count, .after = race_est)

## OUTPUT ----

# Export as CSV
write_csv(voters_merged, file = "data/voters_merged.csv")
