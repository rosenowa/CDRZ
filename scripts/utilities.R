library(tidyverse)
library(tidylog)
library(janitor)
library(here)
library(sf)
library(tigris)
library(dplyr)


# Read in raw data with cdrz tracts
#
cdrz_full_data = read.csv(here("data", "data-raw", "cdrz", "cdrz_raw_fema_2024.csv")) %>% 
  clean_names() %>% 
  mutate(state_fips = str_pad(state_fips, 2, "left", "0"),
         county_fips = str_pad(county_fips, 3, "left", "0"),
         county_code = str_c(state_fips, county_fips),
         cdrz = 1) %>% 
  rename(geoid = geoid20,
         tribal_name = namelsad)

cdrz_data <- cdrz_full_data %>% filter(cdrz_designation_date == "September 6, 2023") #filtering out new tribal and territory designations
  
# list of cdrz tracts to filter on
cdrz_tracts <- cdrz_data %>% pull(geoid)

# list of county tract combinations
cdrz_counties <- cdrz_data %>% 
  select(geoid, county_code) 

# list of county tract combinations - wide format so one row per county
cdrz_counties_wide <- cdrz_data %>% 
  select(geoid, county_code) %>% 
  group_by(county_code) %>% 
  mutate(n = 1:n()) %>% 
  # one row per county
  pivot_wider(id_cols = county_code, 
              names_from = n, 
              names_glue = "cdrz_{n}", 
              values_from = geoid)

