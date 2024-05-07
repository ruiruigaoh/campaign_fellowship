## LOAD PACKAGES ----

library(tidyverse)
library(vroom)
library(wru)
library(ggmap)
library(sf)
library(osmdata)

## READ IN DATA ----

q_voters_gc <- vroom("data/geocoding/q_voters_geocodio_output.csv",
  .name_repair = janitor::make_clean_names
)

## MAPPING VOTER GEOCORDINATES TO BLOCKS ----

# convert geocoded voters into a shapefile
voters_sf <- st_as_sf(q_voters_gc, coords = c("longitude", "latitude"), remove = FALSE, crs = 4326)

# load block-level shapefile for new york
blocks <- st_read("data/geocoding/nhgis_ny_blocks_2020/NY_block_2020.shp")

# set the crs of the voter shapefile equal to the crs of blocks
voters_sf <- st_transform(voters_sf, crs = st_crs(blocks))

# map voter geocoordinates to blocks
voters_blocks <- st_join(voters_sf, blocks)

## RUN WRU PACKAGE FOR RACE PREDICTION ----

# rename columns as required by the package
voters_blocks_processed <- voters_blocks |>
  st_drop_geometry() |>
  select(!county) |>
  rename(
    county = COUNTYFP20,
    tract = TRACTCE20,
    block = BLOCKCE20
  ) |>
  # remove non-Queens county FIPS codes in the voter records
  filter(!(county %in% c("047", "059") | is.na(county))) |>
  select(
    county_emsid, surname, first, state, county, tract, block,
    address, birth_date, gender, political_party, other_party, telephone_optional,
    email_address, latitude, longitude
  )

# load census data
queens_census <- readRDS("data/race_pred/census_2020_queens.RData")

# run race prediction
future::plan(future.callr::callr)
voters_pred <- predict_race(
  voter.file = voters_blocks_processed,
  census.geo = "block",
  census.data = queens_census,
  age = FALSE,
  sex = FALSE,
  year = "2020",
  surname.year = 2020
)

## OUTPUT ----

# full dataset
saveRDS(voters_pred, "data/race_pred/pred_voters.rds")

# smaller dataset w/o extraneous columns for erin
voters_pred %>%
  select(county_emsid, surname, first, address, birth_date, gender, political_party, other_party, telephone_optional, email_address, starts_with("pred")) %>%
  write_csv("data/race_pred/pred_voters_for_erin.csv", delim)
