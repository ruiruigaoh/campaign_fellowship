## LOAD PACKAGES ----

library(tidyverse)
library(scales)
library(vroom)
library(wru)

## READ IN DATA ----

# read in data layout file (converted from original word doc to csv)
col_props <- read_csv("data/wendy_data/voters2_cols.csv")

# read in voter record file
q_voters <- vroom_fwf("data/wendy_data/Queens_voters2.txt",
  col_positions = fwf_positions(col_props$start, col_props$end, col_props$col_name),
  .name_repair = janitor::make_clean_names,
  col_types = cols(
    house_number_suffix = "c",
    mailing_address_3 = "c",
    mailing_address_4 = "c",
    birth_date = col_date(format = "%Y%m%d"),
    registration_date = col_date(format = "%Y%m%d"),
    eff_status_change_date = col_date(format = "%Y%m%d"),
    year_last_voted = "i",
    future_party = "c",
    future_other_party = "c",
    future_party_effective_date = "d",
    zip_code = "c"
  )
)

## DATA MISSINGNESS ----

# calculate number of missing values in each column
apply(q_voters, 2, function(x) sum(is.na(x)))

# drop completely empty columns
# and remove rows where last_name and first_name are both missing
q_voters <- q_voters |>
  select(!starts_with("future")) %>%
  filter(!(is.na(surname) & is.na(first)))

## DATA FORMATTING ISSUES ----

# these processing steps lower the chance that batch geocoding fails
# due to improper address formatting issues.
# the formatting steps below led to a 99.7% geocoding rate with OpenStreetMap
# as tested on a small randomly generated sample of 1000 addresses.

# add ordinal suffixes to street_name (where applicable), city, and state
q_voters_gc <- q_voters |>
  mutate(
    num_street_name = label_ordinal()(as.integer(str_extract(street_name, "\\b\\d+\\b"))),
    street_name = str_replace(street_name, "\\b\\d+\\b", num_street_name),
    city = "QUEENS",
    state = "NY",
    country = "USA",
    .after = street_name
  ) |>
  select(!num_street_name)

# combine address into one column
q_voters_gc <- q_voters |>
  unite("address",
    c("house_number", "street_name"),
    sep = " ", remove = TRUE, na.rm = TRUE
  ) |>
  unite("address",
    c("address", "city", "state", "zip_code", "country"),
    sep = ", ", remove = FALSE, na.rm = TRUE
  )

## OUTPUT ----

vroom_write(q_voters, file = "data/geocoding/q_voters_geocodio_input.csv",
            delim = ",", col_names = TRUE)
