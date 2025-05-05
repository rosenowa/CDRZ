##########################################
# Programmer: Amy Rogin (used by Annie Rosenow)
# Date: 10/2024
#
# (1) Housekeeping
# (2) 2021 ACS data for full list of variables on CDRZs
# (3) 2010 to 2019 ACS data 
#
##########################################
# (1) Housekeeping
library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(janitor)
library(tidylog)
library(tidycensus)
library(purrr)
library(sf)
library(skimr)
library(writexl)
library(dplyr)
library(tigris)
library(urbnthemes)
library(ggplot2)
set_urbn_defaults(style = "print")


# source box link and cdrz tracts
source(here("scripts", "utilities.R"))

# list of states for api call
states <- c(state.abb, "DC")

##########################################
# (2) 2021 ACS data for full range of variables
#  on CDRZs

# How many places are in MSA or outside 
# Land area / population density
# count by state
# histogram of population density color coded by region
# governance set up (msa/unincorporated)
# people living in cdrz by population 
# Overall 
# how many people
# Regional 
# 4 regions (NE/SE/)
# National and regional numbers and then CRDZ numbers 
# population by state - what share of state lives in a cdrz
# congressional district - what population live in democratic vs republican
# who has thought through census tract designations as urban/rural/exurban

# Total population - B01003_001
# Race/Ethnicity 
#  - White alone, not H/L 
#  - Black alone, not H/L 
#  - Asian alone, not H/L 
#  - Native Hawaiian and Other Pacific Islander alone, not H/L 
#  - Two or more races alone, not H/L 
#  - Hispanic or Latino 
# Disability Status 
#  - Total civilian non-institutionalized population 
#  - With a disability 
# Language Spoken at home 
#  - English only 
#  - Language other than English  
# Computers and internet use 
#  - Total households 
#  - With a broadband internet subscription 
# Income & Benefits 
#  - Total households 
#  - Median household income 
#  - Mean household income 
#  - With Food Stamp/SNAP benefits in the past 12 months 
# Poverty 
#  - Percent below poverty line 
# Housing Tenure 
#  - Occupied housing units 
#  - Owner occupied 
#  - Renter occupied 
# Gross Rent as a percentage of household income 
#  - Occupied units paying rent 
#  - 30.0 to 34.9 percent 
#  - 35% or more 
#  - Not computed

#  reviewing acs variables
acs_variable_list <- load_variables(year = 2020,
dataset = "acs5", 
cache = TRUE)

# Vars to fetch from ACS
acs_demographic_vars <- c(
  total_pop_ = "B01003_001",
  ethnicity_denom_ = "B03002_001", #total population by race
  non_hisp_total_ = "B03002_002", 
  non_hisp_white_ = "B03002_003", 
  non_hisp_black_ = "B03002_004",
  non_hisp_native_ = "B03002_005", 
  non_hisp_asian_ = "B03002_006", 
  non_hisp_pi_ = "B03002_007",
  non_hisp_two_ = "B03002_009",
  hisp_total_ = "B03002_012",
  
  # Age
  age_total_ = "B01001_001", 
  m_under_5_ = "B01001_003", 
  m_5_to_9_ = "B01001_004",  
  m_10_to_14_ = "B01001_005", 
  m_15_to_17_ = "B01001_006", 
  m_65_to_66_ = "B01001_020", 
  m_67_to_69_ = "B01001_021", 
  m_70_to_74_ = "B01001_022", 
  m_75_to_79_ = "B01001_023",
  m_80_to_84_ = "B01001_024", 
  m_85_plus_ = "B01001_025", 
  
  f_under_5_ = "B01001_027", 
  f_5_to_9_ = "B01001_028",  
  f_10_to_14_ = "B01001_029", 
  f_15_to_17_ = "B01001_030", 
  f_65_to_66_ = "B01001_044", 
  f_67_to_69_ = "B01001_045", 
  f_70_to_74_ = "B01001_046", 
  f_75_to_79_ = "B01001_047",
  f_80_to_84_ = "B01001_048", 
  f_85_plus_ = "B01001_049", 
  
  # Unemployment
  civilian_labor_force_ = "B23025_003",
  unemployed_ = "B23025_005",
  
  # Disability Status
  disability_total_ = "B18101_001",
  disability_m_under5_ = "B18101_004",
  disability_m_5_17_ = "B18101_007",
  disability_m_18_34_ = "B18101_010",
  disability_m_35_64_ = "B18101_013",
  disability_m_65_74_ = "B18101_016",
  disability_m_75_over_ = "B18101_019",
  disability_f_under5_ = "B18101_023",
  disability_f_5_17_ = "B18101_026",
  disability_f_18_34_ = "B18101_029",
  disability_f_35_64_ = "B18101_032",
  disability_f_65_74_ = "B18101_035",
  disability_f_75_over_ = "B18101_038",
  
  # Home ownership by race/ethnicity
  tenure_estimate_total_ = "B25003_001", 
  tenure_owner_occ_ = "B25003_002",
  
  # OCCUPANCY STATUS
  occupancy_total_ = "B25002_001", 
  occupied_ = "B25002_002", 
  vacant_ = "B25002_003",
  
  # 100% FPL
  fpl_est_tot = "B17020_001",
  fpl_below = "B17020_002",
  
  # Median household income
  median_hh_income_ = "B19013_001",
  median_hh_income_white_ = "B19013A_001", 
  median_hh_income_non_hisp_white_ = "B19013H_001", 
  median_hh_income_black_ = "B19013B_001", 
  median_hh_income_native_ = "B19013C_001", 
  median_hh_income_asian_ = "B19013D_001",
  median_hh_income_pi_ = "B19013E_001",
  median_hh_income_hisp_ = "B19013I_001",
  
  # Households Receiving Snap
  num_hh_snap_tot_ = "B22003_001",
  num_hh_receive_snap_ = "B22003_002",
  
  # Language spoken at home
  language_est_tot_ = "B16001_001", 
  english_home_  = "B16001_002",
  
  # Internet subscription 
  internet_est_tot_ = "B28002_001", 
  broadband_ = "B28002_004", 
  
  # Gross rent as a percentage of household income
  gross_rent_est_tot_ = "B25070_001", 
  gross_rent_30_35_ =  "B25070_007", 
  gross_rent_35_40_ = "B25070_008", 
  gross_rent_40_50_ = "B25070_009", 
  gross_rent_50_plus_ = "B25070_010",
  gross_rent_not_comp_ = "B25070_011"
)

# Fetch data from census api
acs_demographic_df_raw <- get_acs(
  geography = "tract",
  variables = acs_demographic_vars,
  year = 2023,
  state = states,
  survey = "acs5",
  geometry = FALSE,
  output = "wide") %>% 
  clean_names() 

# Split geoid into state and county codes and separate state and county names
# Also drop Puerto Rico
acs_demographics_clean <- acs_demographic_df_raw %>% 
  mutate(
    state_fips = substr(geoid, 1, 2),
    tract_fips = substr(geoid, 3, 9), 
    state_name = str_extract(name, "([^, ]*$)")
  ) %>% 
  rename_with(~sub("_e$", "", .), ends_with("_e")) %>% 
  filter(state_fips != "72") %>% 
  # Calculate percentages for variables
  mutate(
    # Race and ethnicity
    per_non_hisp_white = non_hisp_white / ethnicity_denom, 
    per_non_white = 1 - per_non_hisp_white,
    per_non_hisp_black = non_hisp_black / ethnicity_denom,
    per_non_hisp_native = non_hisp_native / ethnicity_denom, 
    per_non_hisp_asian = non_hisp_asian / ethnicity_denom, 
    per_non_hisp_pi = non_hisp_pi / ethnicity_denom,
    per_non_hisp_two = non_hisp_two / ethnicity_denom, 
    
    per_hisp = hisp_total / ethnicity_denom,  
    
    # Unemployment rate
    per_unemp = unemployed / civilian_labor_force,
    
    # Percent with disability
    per_disability = (disability_m_under5 + disability_m_5_17 + disability_m_18_34 + 
                        disability_m_35_64 + disability_m_65_74 + disability_m_75_over + 
                        disability_f_under5 + disability_f_5_17 + disability_f_18_34 + 
                        disability_f_35_64+ disability_f_65_74 + disability_f_75_over) / disability_total,
    
    per_17_under = (m_under_5 + m_5_to_9 + m_10_to_14 + m_15_to_17 +
                      f_under_5 + f_5_to_9 + f_10_to_14 + f_15_to_17) / age_total,
    
    per_65_over = (m_65_to_66 + m_67_to_69 + m_70_to_74 + m_75_to_79 +
                     m_80_to_84 + m_85_plus + f_65_to_66 + f_67_to_69 + 
                     f_70_to_74 + f_75_to_79 + f_80_to_84 + f_85_plus)/age_total, 
    # Percent homeowner by race
    per_h_owner = tenure_owner_occ / tenure_estimate_total,
    
    # Percent below FPL by race
    per_below_fpl_overall = ifelse(fpl_est_tot > 0,
                                   fpl_below / fpl_est_tot, 0),
    
    # HH receiving SNAP
    per_hh_receive_snap = num_hh_receive_snap / num_hh_snap_tot,
    
    # broadband
    per_broadband = broadband / internet_est_tot, 
    
    # language spoken at home
    per_english = english_home / language_est_tot,
    per_non_english = 1 - per_english,
    
    # occupancy
    per_occupied = occupied / occupancy_total, 
    per_vacant = vacant / occupancy_total, 
    
    # gross rent 
    per_gross_rent_30_35 = gross_rent_30_35 / gross_rent_est_tot, 
    per_gross_rent_35_plus = (gross_rent_35_40 + gross_rent_40_50 + gross_rent_50_plus)/ gross_rent_est_tot,
    per_rent_burden = (gross_rent_30_35 + gross_rent_35_40 + gross_rent_40_50 + gross_rent_50_plus)/ gross_rent_est_tot
  ) #%>% 
  select(geoid, state_name, state_fips, tract_fips, total_pop, 
         median_hh_income,
         per_non_hisp_white:per_hisp,
         per_unemp,
         per_disability,
         per_h_owner,
         per_below_fpl_overall,
         per_gross_rent_30_35, 
         per_gross_rent_35_plus, 
         per_rent_burden, 
         per_occupied, 
         per_vacant,
         per_broadband, 
         per_hh_receive_snap,
         per_17_under, 
         per_65_over
  ) 


### joining CDRZs and ACS data by geoid 
acs_cdrz <- cdrz_data %>%
  left_join(acs_demographics_clean %>% select(-state_fips, -tract_fips, -state_name), by = "geoid") 


###################################################################################
########JOINING ACS DATA WITH GEOGRAPHY (TRACTS) ##################################
###################################################################################
###################################################################################

# tract shapefile 
tract_sf <- tigris::tracts(year = 2022, cb = TRUE) 

# create list of states in each region
northeast <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont",
               "New Jersey", "New York", "Pennsylvania")

midwest <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin",
             "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", 
             "North Dakota", "South Dakota")

south <- c("Delaware", "District of Columbia", "Florida", "Georgia", "Maryland",
           "North Carolina", "South Carolina", "Virginia", "West Virginia",
           "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana", 
           "Oklahoma", "Texas")

west <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming", 
          "Alaska", "California", "Hawaii", "Oregon", "Washington")

# ACS demographic data
acs_cdrz_geography <- acs_cdrz %>%
# join tract_sf
left_join(tract_sf %>% select(GEOID, NAMELSADCO, STUSPS, geometry, STATE_NAME, STATEFP, COUNTYFP), by = c("geoid" = "GEOID")) %>% 
  # code in regions
  mutate(region = case_when(
    STATE_NAME %in% northeast ~ "Northeast", 
    STATE_NAME %in% midwest ~ "Midwest", 
    STATE_NAME %in% south ~ "South", 
    STATE_NAME %in% west ~ "West" ))%>% 
  #creating a county_state_code column for the acs_demographics_tract dataset to use when joining the rural urban dataset
  mutate(county_state_code = paste0(STATEFP, COUNTYFP)) %>% 
  rename(county_name = NAMELSADCO,
         state_abbreviation = STUSPS) %>% select(-COUNTYFP,-STATEFP, -STATE_NAME, -county_type) %>% st_as_sf() 

##connecticut has one missing CDRZs tract- likely because of the tract alignment from 2023-2021
missing <- acs_cdrz %>% filter(is.na(total_pop))


#################################################################################
#################################################################################
########################    Adding additional data      #########################
#################################################################################
##########################  URBAN RURAL     #####################################
#################################################################################

# URBAN RURAL  ------------------------------------------------------------

# Rural urban measure downloaded from the USDA's 2023 Rural-Urban Continuum Codes
# https://www.ers.usda.gov/data-products/atlas-of-rural-and-small-town-america/download-the-data

# rural / urban flag
rural_urban = read.csv(here("data", "data-raw", "rural_urban_continuum_code_2023.csv")) %>% 
  clean_names() %>% 
  mutate(fips = str_pad(fips, 5, "left", "0"), 
         urban_designation = case_when(
           rural_urban_continuum_code2013 %in% c(4:9) ~ "rural", 
           rural_urban_continuum_code2013 %in% c(1:3) ~ "urban"
         )) %>%
    rename(county_state_code = fips) %>%
  select(county_state_code, urban_designation)

################################################################################
################################################################################
##################  Headwater Rural Capcity Index   ############################
################################################################################
################################################################################
##Headwater Rural Capcity Index Data 2024 (https://headwaterseconomics.org/equity/rural-capacity-map/#methods)

county_capacity = read.csv(here("data", "data-raw", "HE_Rural_Capacity_Index_March_2024.csv")) %>%
      mutate(FIPS = str_pad(FIPS, 5, "left", "0")) %>% 
  rename(county_state_code = FIPS) %>% 
  clean_names() %>% 
  select(county_state_code, 
         rural_capacity_index,
         percent_of_counties_nationwide_with_higher_capacity) 
 

################################################################################
################################################################################
###################### NRI Ratings Data       ##################################
################################################################################
################################################################################
### NRI Ratings Data from https://hazards.fema.gov/nri/
nri_index_tracts = read.csv(here("data", "data-raw", "nri", "NRI_Table_CensusTracts.csv")) %>% clean_names()

################################################################################
################################################################################
###################### ACS County Population Data       ########################
################################################################################
################################################################################
acs_county_demographic_vars <- c(total_pop = "B01003_001")
  
# Fetch data from census api
acs_demographic_county_raw <- get_acs(
  geography = "county",
  variables = acs_county_demographic_vars,
  year = 2022,
  state = states,
  survey = "acs5",
  geometry = FALSE,
  output = "wide") %>% 
  clean_names() %>% select(geoid, name, total_pop_e) %>%
  rename(total_pop_county = total_pop_e,
         county_state_code = geoid)


# ##########################################################################################
# ##########################################################################################
# ################################   Capital flows data      ###############################
# ##########################################################################################
# ##########################################################################################
# ###reading in capital flows from Urban Institute: https://datacatalog.urban.org/dataset/capital-flows-disparities-cities-counties-and-states
#capital_flows_by_county <- read.csv(here("data", "data-raw", "capital-flows-and-disparities.csv"))


# ################################################################################
# ################################################################################
# ###################### First street data      ##################################
# ################################################################################
# ################################################################################
# ### First street data from https://urbanorg.app.box.com/s/bg7b1b4nk0uo7khs9z51ssj9e45v1ph7
# ### 
# ### Minimal == 1, 
# ### Minor == 2, 
# ### Moderate == 3, 4
# ### Major == 5, 6
# ### Severe == 7, 8 
# ### Extreme  == 9, 10 
# fsf_fire_tract = read.csv(here("data", "data-raw", "first-street", "fsf_fire_tract_summary.csv")) %>% 
#   mutate(fips = str_pad(fips, 11, "left", "0")) %>% 
#   rename(geoid = fips)%>% 
#   mutate(per_minimal_fire = count_firefactor1/count_property, 
#          per_minor_fire = count_firefactor2/count_property, 
#          per_moderate_fire = (count_firefactor3 + count_firefactor4) /count_property, 
#          per_major_fire = (count_firefactor5 + count_firefactor6) /count_property, 
#          per_severe_fire = (count_firefactor7 + count_firefactor8) /count_property,
#          per_extreme_fire = (count_firefactor9 + count_firefactor10) /count_property) # Creating a rating system (that fsf uses)
# 
# fsf_heat_tract = read.csv(here("data", "data-raw", "first-street", "fsf_heat_tract_summary.csv")) %>% 
#   mutate(fips = str_pad(fips, 11, "left", "0")) %>% 
#   rename(geoid = fips) %>% 
#   mutate(per_minimal_heat = count_heatfactor1/count_property, 
#          per_minor_heat = count_heatfactor2/count_property, 
#          per_moderate_heat = (count_heatfactor3 + count_heatfactor4) /count_property, 
#          per_major_heat = (count_heatfactor5 + count_heatfactor6) /count_property, 
#          per_severe_heat = (count_heatfactor7 + count_heatfactor8) /count_property,
#          per_extreme_heat = (count_heatfactor9 + count_heatfactor10) /count_property) # Creating a rating system (that fsf uses)
# 
# fsf_flood_tract = read.csv(here("data", "data-raw", "first-street", "fsf_flood_tract_summary.csv")) %>% 
#   mutate(fips = str_pad(fips, 11, "left", "0")) %>% 
#   rename(geoid = fips) %>% 
#   mutate(per_minimal_flood = count_floodfactor1/count_property, 
#          per_minor_flood = count_floodfactor2/count_property, 
#          per_moderate_flood = (count_floodfactor3 + count_floodfactor4) /count_property, 
#          per_major_flood = (count_floodfactor5 + count_floodfactor6) /count_property, 
#          per_severe_flood = (count_floodfactor7 + count_floodfactor8) /count_property,
#          per_extreme_flood = (count_floodfactor9 + count_floodfactor10) /count_property)# Creating a rating system (that fsf uses)
# 
# #joining heat, flood, and fire data together 
# fsf_data <- fsf_flood_tract %>%
#   # Joining with fsf_heat_tract
#   left_join(fsf_heat_tract, by = "geoid") %>%
#   # Joining with fsf_fire_tract
#   left_join(fsf_fire_tract, by = "geoid") %>% 
#   mutate(geoid = as.character(geoid))

################################################################################
################################################################################
###################### Joining data with CDRZ_ACS ###############################
################################################################################
################################################################################

cdrz_all_data <- acs_cdrz_geography %>%
  # Joining with rural-urbanclassification 
  left_join(rural_urban, by = "county_state_code") %>%
  # Joining with county capacity 
  left_join(county_capacity %>% select(rural_capacity_index, county_state_code, 
                                       percent_of_counties_nationwide_with_higher_capacity), 
            by = "county_state_code") #%>% 
  # Joining with NRI Index
  # left_join(nri_index_tracts %>% select(nri_id, risk_score, risk_ratng,
  #                                           eal_score, eal_ratng, sovi_score, sovi_ratng, resl_score, resl_ratng),
  #                 by = "nri_id")  %>% 
  #joining ACS county population 
  #left_join(acs_demographic_county_raw %>% select(total_pop_county, county_state_code), 
            by = "county_state_code") 
            
  # # Joining with first street data 
  #   left_join(fsf_data %>% select(geoid, 
  #                                 starts_with("per_minimal_"),
  #                                 starts_with("per_minor_"),
  #                                 starts_with("per_moderate_"),
  #                                 starts_with("per_major_"),
  #                                 starts_with("per_severe_"),
  #                                 starts_with("per_extreme_")), by = "geoid") 

################################################################################
################################################################################
###########################    SBP + GEOS FOOTPRINT    #########################
################################################################################
################################################################################
################################################################################

partner_cdrz_all_data <- cdrz_all_data %>% 
  mutate(
    # SBP geographies
    sbp = case_when(
      (state_abbreviation == "TX")| ## county_name %in% c("Orange County", "Jefferson County", "Hardin County", "Jasper County")) |
        (state_abbreviation == "AL" & county_name == "Mobile County") |
        (state_abbreviation == "SC" & county_name %in% c("Berkeley County", "Dorchester County", "Charleston County")) |
        (state_abbreviation == "MS" & county_name %in% c("Covington County", "Forrest County", "George County", "Greene County", "Hancock County", 
                                                         "Harrison County", "Jackson County", "Jefferson Davis County", "Jones County", 
                                                         "Lamar County", "Marion County", "Pearl River County", "Perry County", "Stone County", 
                                                         "Wayne County"))|
        (state_abbreviation == "FL" & county_name == "Charlotte County") ~ "1",# All other cases set to 0
      TRUE ~ "0"), 
    
    # GEOS geographies
    geos = case_when(
      (state_abbreviation == "NC" & county_name %in% c("Beaufort County", "Carteret County", "Craven County", "Dare County", "Hyde County", 
                                                       "New Hanover County", "Onslow County", "Sampson County", "Tyrrell County", "Washington County")) |
        (state_abbreviation == "SC" & county_name %in% c("Beaufort County", "Berkeley County", "Charleston County", "Dorchester County", "Horry County")) |
        (state_abbreviation == "GA" & county_name %in% c("Bulloch County", "Chatham County","Liberty County", "McIntosh County", "Seminole County", "Wayne County")) |
        (state_abbreviation == "FL" & county_name %in% c("Bay County", "Brevard County", "Broward County", "Charlotte County", "Collier County", "DeSoto County",
                                                         "Hendry County", "Hillsborough County", "Indian River County", "Martin County", "Miami-Dade County", 
                                                         "Palm Beach County", "Pinellas County", "St. Lucie County", "Washington County")) ~ "1",
      TRUE ~ "0"), 
    
    # overlapping geographies (both SBP and GEOs)
    partner_overlap = case_when(
      (state_abbreviation == "SC" & county_name %in% c("Berkeley County", "Charleston County")) | 
        (state_abbreviation == "FL" & county_name == "Charlotte County") ~ "1",
      TRUE ~ "0"  ),
    
    # column to combine sbp, geos, and overlapping cdrzs (both SBP and GEOs)
    sbp_geos_footprint = case_when(
      sbp == "1" & geos == "1" ~ "Partner overlap",  # If both SBP and GEOs are working with the CDRZ
      sbp == "1" ~ "SBP",                            # If only SBP is working with the CDRZ
      geos == "1" ~ "GEOs",                          # If only GEOs is working with the CDRZ
      TRUE ~ NA_character_  ), 
    
    # Host Entity Defined by SBP
    host_entity_sbp = case_when(
      sbp == "1" &  state_abbreviation == "AL" & county_name == "Mobile County" ~ "City of Mobile", 
      sbp == "1" &  state_abbreviation == "FL" & county_name == "Charlotte County" ~ "Charlotte County, FL",
      sbp == "1" &  state_abbreviation == "MS" ~ "Southern Mississippi Planning District  (co-placed with Water Insitute of the Gulf)",
      sbp == "1" &  state_abbreviation == "TX" & county_name %in% c("Orange County", "Jefferson County", "Hardin County", "Jasper County") ~ "Southeast Texas Regional Planning Commission", 
      sbp == "1" &  state_abbreviation == "TX" & !county_name %in% c("Orange County", "Jefferson County", "Hardin County", "Jasper County") ~ "Texas State's General Land Office (GLO)",
      sbp == "1" &  state_abbreviation == "SC" ~ "Berkeley-Charleston-Dorchester Council of Government (BCDCOG)"), 
    
    # Defining SBP Communities 
    sbp_community_cluster = case_when(
      sbp == "1" & state_abbreviation == "AL" & county_name == "Mobile County" ~ "City of Mobile", 
      sbp == "1" & state_abbreviation == "SC" & geoid %in% c(45035010603, 45035010700) ~ "Dorchester County",
      sbp == "1" & state_abbreviation == "SC" & county_name == "Charleston County" | geoid == 45035010818  ~ "North Charleston City", 
      sbp == "1" ~ paste0(county_name)), 
    
    # Number of SBP fellows
    number_of_fellows_sbp = case_when(
      host_entity_sbp == "City of Mobile" ~ 1, 
      host_entity_sbp == "Charlotte County, FL" ~ 1, 
      host_entity_sbp == "Southern Mississippi Planning District  (co-placed with Water Insitute of the Gulf)" ~ 1, 
      host_entity_sbp == "Southeast Texas Regional Planning Commission" ~ 1, 
      host_entity_sbp == "Texas State's General Land Office (GLO)" ~ 1, 
      host_entity_sbp == "Berkeley-Charleston-Dorchester Council of Government (BCDCOG)" ~ 1), 
    
    #name of SBP Fellow (until we have the list of the fellow names)
    fellow_name_sbp = case_when(
    sbp == "1" & state == "Florida" ~ "Kurt Williams",
    sbp == "1" & state == "South Carolina" ~ "Shayla Jefferson",
    sbp == "1" & state == "Alabama" ~ "Madeleine Dotson",
    sbp == "1" & host_entity_sbp == "Southeast Texas Regional Planning Commission" ~ "No staff",
    sbp == "1" & host_entity_sbp == "Texas State's General Land Office (GLO)" ~ "Will McCrory",
    host_entity_sbp == "Southern Mississippi Planning District  (co-placed with Water Insitute of the Gulf)" ~ "No staff",
    TRUE ~ NA_character_), 
    
    # SBP Fellow Group Managers
    fellows_group_manager_sbp = case_when(
      sbp == "1" & state_abbreviation %in% c("TX", "SC") ~ "Michelle Bohrson (MBohrson@sbpusa.org)", 
      sbp == "1" & state_abbreviation %in% c("AL", "MS", "FL") ~ "Don Gardner (DGardner@sbpusa.org)"), 
    
    # SBP Analyst
    fellows_analyst_sbp = "Makisha Mosley (MMosley@sbpusa.org)", 
    
    # SBP SME
    fellows_SME_sbp = "Sherry Risk (SRisk@sbpusa.org)", 
    
    #Geos Nicknames for CDRZs
    nickname_geos = case_when(
      geos == "1" & state_abbreviation == "SC" & county_name == "Beaufort County" ~ "Bluffton",
      geos == "1" & state_abbreviation == "SC" & county_name == "Berkeley County" ~ "Goose Creek",
      geos == "1" & state_abbreviation == "SC" & county_name == "Charleston County" ~ "Nth Charlston",
      geos == "1" & state_abbreviation == "SC" & county_name == "Dorchester County" ~ "Summerville",
      geos == "1" & state_abbreviation == "SC" & county_name == "Horry County" ~ "Myrtle Beach",
      geos == "1" & state_abbreviation == "SC" & county_name == "Bulloch County" ~ "Statesboro",
      geos == "1" & state_abbreviation == "GA" & county_name == "Chatham County" & geoid == 13051004001 ~ "Census Tract 40.01",
      geos == "1" & state_abbreviation == "GA" & county_name == "Chatham County" & geoid == 13051010601 ~ "Garden City",
      geos == "1" & state_abbreviation == "GA" & county_name == "Chatham County" & geoid == 13051011700 ~ "NW Savannah",
      geos == "1" & state_abbreviation == "GA" & county_name == "Chatham County" & geoid == 13051011900 ~ "SE Historic District", 
      geos == "1" & state_abbreviation == "FL" & county_name == "Brevard County" ~ "City of Melbourne", 
      geos == "1" & state_abbreviation == "FL" & county_name == "Indian River County" ~ "Indian River/ Fellsmere", 
      geos == "1" & state_abbreviation == "FL" & county_name == "Martin County" ~ "City of Stuart", 
      geos == "1" & state_abbreviation == "FL" & county_name == "Broward County" & geoid == 12011010307 ~ "Deerfield Beach", 
      geos == "1" & state_abbreviation == "FL" & county_name == "Broward County" & geoid %in% c(12011030500,12011030601) ~ "Pompano Beach", 
      geos == "1" & state_abbreviation == "FL" & county_name == "Broward County" & geoid == 12011042502 ~ "Ft Lauderdale",
      geos == "1" & state_abbreviation == "FL" & county_name == "Broward County" & geoid %in% c(12011050207, 12011050208) ~ "Oakland & Nth Andrews Parks",
      geos == "1" & state_abbreviation == "FL" & county_name == "Broward County" & geoid == 12011110600 ~ "Dania Beach",
      geos == "1" & state_abbreviation == "FL" & county_name == "Miami-Dade County" & geoid == 12086009010~ "Doral",
      geos == "1" & state_abbreviation == "FL" & county_name == "Miami-Dade County" & geoid %in% c(12086009040, 12086009102) ~ "Doral & Hialeach",
      geos == "1" & state_abbreviation == "FL" & county_name == "Miami-Dade County" & geoid == 12086011500 ~ "Homestead & Everglades",
      geos == "1" & state_abbreviation == "FL" & county_name == "Palm Beach County" & geoid == 12099005000 ~ "Atlantis & Lantana",
      geos == "1" & state_abbreviation == "FL" & county_name == "Palm Beach County" & geoid == 12099007100 ~ "Boca Raton",
      geos == "1" & state_abbreviation == "FL" & county_name == "Pinellas County" ~ "Clearwater City",
      TRUE ~ NA_character_), 
    
    #Groups defined by GEOs
    geos_community_cluster = case_when(
      ## SOUTH CAROLINA  (5 grpups)
      geos == "1" & state_abbreviation == "SC" & county_name == "Beaufort County" ~ "Beaufort (Bluffton) County, SC",
      geos == "1" & state_abbreviation == "SC" & county_name == "Berkeley County" ~ "Berkeley (Goose Creek) County, SC",
      geos == "1" & state_abbreviation == "SC" & county_name == "Charleston County" ~ "Charleston (Nth Charleston) County, SC",
      geos == "1" & state_abbreviation == "SC" & county_name == "Dorchester County" ~ "Dorchester (Summerville) County, SC",
      geos == "1" & state_abbreviation == "SC" & county_name == "Horry County" ~ "Horry (Myrtle Beach) County, SC",
      
      ## GEORGIA (6 groups- but only two have nicknames)
      geos == "1" & state_abbreviation == "GA" & county_name == "Chatham County" & geoid %in% c(13051004001,13051004002,
                                                                                                13051010504, 13051980000,
                                                                                                13051010601, 13051011700, 13051011900)  ~ "Chatham County (Census Tract 40.01, Garden City, NW Savannah, SE Historic District), GA",
      geos == "1" & state_abbreviation == "GA" & county_name == "Bulloch County" ~ "Bulloch (Statesboro) County, GA",
      # geos == "1" & state_abbreviation == "GA" & county_name == "Liberty County" ~ "Liberty Group",
      
      #FLORIDA (16 groups)
      geos == "1" & state_abbreviation == "FL" & county_name == "Brevard County" ~ "Brevard County (City of Melbourne), FL",
      geos == "1" & state_abbreviation == "FL" & county_name == "Indian River County" ~ "Indian River County (Indian River/Fellsmere), FL", 
      geos == "1" & state_abbreviation == "FL" & county_name == "Martin County" ~ "Martin County (City of Stuart), FL", 
      geos == "1" & state_abbreviation == "FL" & county_name == "St. Lucie County" ~ "St. Lucie (Fort Pierce) County, FL",
      geos == "1" & state_abbreviation == "FL" & county_name == "Broward County" & geoid == 12011010307 ~ "Broward (Deerfield Beach) County, FL",
      geos == "1" & state_abbreviation == "FL" & county_name == "Broward County" & geoid %in% c(12011030500, 12011030601) ~ "Broward (Pompano Beach) County, FL",
      geos == "1" & state_abbreviation == "FL" & county_name == "Broward County" & geoid == 12011042502 ~ "Broward (Ft. Lauderdale) County, FL",
      geos == "1" & state_abbreviation == "FL" & county_name == "Broward County" & geoid %in% c(12011050207, 12011050208) ~ "Broward (Oakland & Nth Andrews Parks) County, FL",
      geos == "1" & state_abbreviation == "FL" & county_name == "Broward County" & geoid == 12011110600 ~ "Broward (Dania Beach) County, FL",
      geos == "1" & state_abbreviation == "FL" & county_name == "Miami-Dade County" & geoid %in% c(12086009010, 12086009040, 12086009102) ~ "Miami-Dade (Doral & Hialeach) County, FL", 
      geos == "1" & state_abbreviation == "FL" & county_name == "Miami-Dade County" & geoid == 12086011500 ~ "Miami-Dade (Homestead & Everglades) County, FL",
      geos == "1" & state_abbreviation == "FL" & county_name == "Palm Beach County" ~ "Palm Beach (Atlantis, FL",     #Palm Beach (Atlantis, Lantana, & Boca Raton) County, FL"
      geos == "1" & state_abbreviation == "FL" & county_name == "Pinellas County" ~ "Pinellas County (Clearwater City), FL",
      geos == "1" ~ paste(county_name, state_abbreviation, sep = ", "),
      TRUE ~ NA_character_),
    
    
    # Navigator name Defined by GEOs
    navigator_name_geos = case_when(
      geos == "1" & state_abbreviation == "NC" ~ "Holly White & Helene Whetherington",
      geos == "1" & state_abbreviation == "SC" ~  "Adelaide",
      geos == "1" & state_abbreviation == "GA" ~ "Courtney & Monet",
      geos == "1" & state_abbreviation == "FL" & county_name %in% c( "Brevard County", "Indian River County", "Martin County", "St. Lucie County")~ "Holly Abeels", 
      geos == "1" & state_abbreviation == "FL" & county_name == "Bay County" ~ "Jackson",
      geos == "1" & state_abbreviation == "FL" & county_name == "Washington County" ~ "Stevenson",
      geos == "1" & state_abbreviation == "FL" & county_name %in% c("Hillsborough County", "Pinellas County") ~ "Madhosingh-Hector",
      geos == "1" & state_abbreviation == "FL" & county_name %in% c("Broward County", "Miami-Dade County", "Palm Beach County") ~ "Betancourt & Hart",
      geos == "1" & state_abbreviation == "FL" & county_name %in% c("DeSoto County", "Hendry County") ~ "Ubeda",
      geos == "1" & state_abbreviation == "FL" & county_name %in% c("Charlotte County", "Collier County") ~ "Wilson"), 
    
    # GEOs Supporting Staff
    supporting_staff_geos = "NA", 
    
    #geos community unique ID - Helps to determine how many communities a navigator will oversee 
    unique_id_geos = dense_rank(geos_community_cluster), 
    
    #sbp community unique ID - Helps to determine how many communities a fellows will oversee 
    unique_id_sbp = dense_rank(sbp_community_cluster), 
    
    #host entities defined by Geos 
    host_entity_geos = case_when(
      geos == "1" & state_abbreviation == "GA" ~ "Georgia Conservancy", 
      geos == "1" & state_abbreviation == "NC" ~ "North Carolina Office of Recovery and Resiliency",
      geos == "1" & state_abbreviation == "SC" ~ "Shi Institute for Sustainable Communities at Furman University",
      geos == "1" & state_abbreviation == "FL" ~ "Florida Climate Institute"), 
    
    sbp_geos = case_when(
      sbp == "1" & geos == "0" ~ "sbp",
      geos == "1" & sbp == "0" ~ "geos",
      geos == "1" & sbp == "1" ~ "both")) 

# partner_cdrz_all_data <- partner_cdrz_all_data %>% st_drop_geometry()
# write.csv(partner_cdrz_all_data, "partner_cdrz_all_data.csv")


cdrz_count <- partner_cdrz_all_data %>% 
  st_drop_geometry() %>% 
  group_by(sbp_geos, state) %>%
  count()

sbp_communities_cdrz <- partner_cdrz_all_data %>% 
  st_drop_geometry() %>% 
  filter(sbp_geos %in% c("sbp", "both")) %>%
  group_by(state, county, sbp_community_cluster) %>%
  summarise(
    cdrz = n())%>%
  adorn_totals("row")

geos_communities_cdrz <- partner_cdrz_all_data %>% 
  st_drop_geometry() %>% 
  filter(sbp_geos %in% c("geos", "both")) %>%
  group_by(state, county, geos_community_cluster) %>%
  summarise(
    cdrz = n()) %>%
  adorn_totals("row")

states = partner_cdrz_all_data %>% filter(!is.na(sbp_geos)) %>% 
  group_by(state) %>% 
  count()

##########################################################################################
##########################################################################################
####################### ADDING PLACES FOR SBP AND GEOS ###################################
#########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##getting census data on places
us_places <- places(cb = TRUE)

#using st_filter to filter us_places using the geographies of CDRZs so that the intersection is quicker 
cdrz_places <- us_places %>%
  st_filter(partner_cdrz_all_data) 

##intersecting places and census tracts so we know what communties CDRZs fall in
cdrz_tracts_places1 <- st_join(partner_cdrz_all_data, cdrz_places, join = st_intersects)

#cleaning the dataset 
cdrzs_tracts_places_clean <- cdrz_tracts_places1 %>% 
  select(-c(STATEFP, PLACEFP, PLACENS,AFFGEOID, GEOID, NAME, STUSPS, LSAD, ALAND, AWATER, STATE_NAME, county_code)) %>% 
  rename(place_name = NAMELSAD)%>% 
  clean_names() 

#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################


final_table <- cdrzs_tracts_places_clean %>% 
  st_drop_geometry() %>%
  select(-tribal_name, -objectid,-state_fips, 
         -county, -county_fips,-county_state_code, -cdrz, -shape_area, 
         -shape_length, -sbp, -geos, -partner_overlap, 
         -nri_id, -tract_fips, -county_state_code )  %>%
  select(sbp_geos_footprint, geoid, county_name, place_name, 
         state, state_abbreviation, region, 
         #SBP columns
         unique_id_sbp, sbp_community_cluster, host_entity_sbp, fellow_name_sbp,
         number_of_fellows_sbp, fellows_group_manager_sbp, 
         fellows_analyst_sbp, fellows_sme_sbp, 
         #GEOs columns
         unique_id_geos, geos_community_cluster, nickname_geos, host_entity_geos, navigator_name_geos, supporting_staff_geos, 
         cdrz_designation_date, urban_designation, rural_capacity_index, 
         percent_of_counties_nationwide_with_higher_capacity, everything()) %>%
  group_by(geoid) %>% 
    summarize(
    intersecting_places = paste(place_name, collapse = ", "), 
    across( -place_name, 
            ~ first(.),
            .names = "{.col}")) %>% 
  select(sbp_geos_footprint, geoid, county_name, intersecting_places, everything())

geos <- final_table %>% filter(sbp_geos_footprint != "SBP") %>% 
  group_by(state, geos_community_cluster) %>% 
  summarize(
  number_of_CDRZs= n()
  )

 #write_xlsx(final_table, "final_table.xlsx")

########################################################################################
########################################################################################
################################  GEOS final table #####################################
########################################################################################
########################################################################################
geos_portfolio <- cdrzs_tracts_places_clean %>% 
  st_drop_geometry() %>%
  filter(geos == 1) %>%
  select(-tribal_name, -objectid,-state_fips, 
         -county, -county_fips,-county_state_code, -cdrz, -shape_area, 
         -shape_length, -sbp, -geos, -partner_overlap, 
         -nri_id, -tract_fips, -county_state_code, 
         -unique_id_sbp, -sbp_community_cluster, -host_entity_sbp, -fellow_name_sbp,
         -number_of_fellows_sbp, -fellows_group_manager_sbp, 
         -fellows_analyst_sbp, -host_entity_sbp, -fellows_sme_sbp)  %>%
  select(sbp_geos_footprint, geoid, county_name, place_name, 
         state, state_abbreviation, region,
         #GEOs columns
         unique_id_geos, geos_community_cluster, nickname_geos, host_entity_geos, navigator_name_geos, supporting_staff_geos, 
         cdrz_designation_date, urban_designation, 
         rural_capacity_index, 
         percent_of_counties_nationwide_with_higher_capacity, everything()) %>%
  group_by(geoid) %>% 
  summarize(
    intersecting_places = paste(place_name, collapse = ", "), 
    across( -place_name, 
            ~ first(.),
            .names = "{.col}")) %>% 
  select(sbp_geos_footprint, geoid, county_name, intersecting_places, everything())

# write_xlsx(geos_portfolio, "geos_portfolio.xlsx")

########################################################################################
########################################################################################
################################  SBP final table #####################################
########################################################################################
########################################################################################
sbp_portfolio <- cdrzs_tracts_places_clean %>% 
  st_drop_geometry() %>%
  filter(sbp == 1) %>%
  select(-tribal_name, -objectid,-state_fips, 
         -county, -county_fips,-county_state_code, -cdrz, -shape_area, 
         -shape_length, -sbp, -geos, -partner_overlap, 
         -nri_id, -tract_fips, -county_state_code,
         -unique_id_geos, -geos_community_cluster, 
         -nickname_geos, -host_entity_geos, 
         -navigator_name_geos, -supporting_staff_geos)  %>%
  select(sbp_geos_footprint, geoid, county_name, place_name, 
         state, state_abbreviation, region, 
         #SBP columns
         unique_id_sbp, sbp_community_cluster, host_entity_sbp, fellow_name_sbp,
         number_of_fellows_sbp, fellows_group_manager_sbp, 
         fellows_analyst_sbp, fellows_sme_sbp, cdrz_designation_date, urban_designation, 
         rural_capacity_index, 
         percent_of_counties_nationwide_with_higher_capacity, everything()) %>%
  group_by(geoid) %>% 
  summarize(
    intersecting_places = paste(place_name, collapse = ", "), 
    across( -place_name, 
            ~ first(.),
            .names = "{.col}")) %>% 
  select(sbp_geos_footprint, geoid, county_name, intersecting_places, everything())

# write_xlsx(sbp_portfolio, "sbp_portfolio.xlsx")

#################################################################################
#################################################################################
#################################################################################
##################### MAKING A MAP TO VISUALIZE CDRZs ###########################
#################################################################################
#################################################################################
 
us_states <- tigris::states(year = 2022, cb = TRUE) %>% 
  filter(!(STUSPS %in% c("GU", "MP", "VI", "AS", "PR")))%>%
  tigris::shift_geometry()

all_cdrz_map <- ggplot() + 
  geom_sf(us_states,
          mapping = aes(),
          fill = "transparent",
          color = "#ADABAC") +
  geom_sf(cdrzs_tracts_places_clean %>% filter(geos == 1) %>%
            mutate(group = "GEOs CDRZs"),
          mapping = aes(color = group),
          fill = "NA") +
  geom_sf(cdrzs_tracts_places_clean %>% filter(sbp == 1) %>% 
            mutate(group = "SBP CDRZs"),
          mapping = aes(color = group),
          fill = "NA") +
  geom_sf(cdrzs_tracts_places_clean %>% filter(is.na(sbp_geos_footprint)) %>% 
            tigris::shift_geometry() %>%
            mutate(group = "CDRZs Outside the SPB and GEOs Footprint"),
          mapping = aes(color = group),
          fill = "NA") +
  # geom_sf(cdrz_places %>% 
  #         tigris::shift_geometry() %>%
  #         mutate(group = "Surrounding Places"),
  #         mapping = aes(color = group),
  #         fill = "NA",
  #         size = 0.1) +
  scale_color_manual(values = c(
    "GEOs CDRZs" = "#FCCB41",
    "CDRZs Outside the SPB and GEOs Footprint" = "#E46AA7",
    "SBP CDRZs" = "#46ABDB"#,
   # "Surrounding Places" = "#E46AA7"
  )) + 
  # name = "Key",
  # labels = c("CDRZs Outside the SPB/GEOs Footprint", "GEOs CDRZs", "SBP CDRZs")) + #"Surrounding Places"
  # theme(
  #   legend.text = element_text(size = 4),
  #   legend.title = element_text(size = 4),
  #   legend.key.size = unit(0.3, "cm"),
  #   legend.spacing.y = unit(0.2, "cm")
  # )+
  theme(legend.position = "none") +
  theme_urbn_map() 
print(all_cdrz_map)
ggsave("all_cdrz_map.png", plot = all_cdrz_map, width = 6, height = 4, dpi = 300)


all_cdrz_map <- ggplot() + 
  geom_sf(us_states,
          mapping = aes(),
          fill = "transparent",
          color = "#ADABAC") +
  
  geom_sf(partner_cdrz_all_data %>% filter(sbp_geos == "geos"),
          mapping = aes(),
          color = "#FCCB41",
          fill = "#FCCB41") +
  
  geom_sf(partner_cdrz_all_data %>% filter(sbp_geos == "sbp"),
          mapping = aes(),
          color = "#46ABDB",
          fill = "#46ABDB") +
  
  geom_sf(partner_cdrz_all_data %>% filter(sbp_geos == "both"),
          mapping = aes(),
          color = "#78C26D",
          fill = "#78C26D") +
  
  geom_sf(partner_cdrz_all_data %>% 
            filter(is.na(sbp_geos)) %>% 
            tigris::shift_geometry(),
          mapping = aes(),
          color = "#E46AA7",
          fill = "#E46AA7",
          size = 0.1) +
  theme_urbn_map()
all_cdrz_map
ggsave("all_cdrz_map.png", plot = all_cdrz_map, width = 6, height = 4, dpi = 1000)

###################################################################################
###################################################################################
###################################################################################
###################################################################################
##################################################################################
geos_sbp_cdrzs <- partner_cdrz_all_data %>% filter(!is.na(sbp_geos_footprint))

sbp_geos_states <- tigris::states(year = 2022, cb = TRUE) %>% 
  filter(STUSPS %in% c("AL", "FL", "GA", "LA", "MS", "NC", "SC", "TX"))
  
#using st_filter to filter us_places using the geographies of SBP and GEOs CDRZs  
geos_sbp_cdrzs_places <- us_places %>%
    st_filter(geos_sbp_cdrzs) 

sbp_geos_map_places <- ggplot() + 
  geom_sf(sbp_geos_states,
          mapping = aes(),
          fill = "transparent",
          color = "#ADABAC") +
  geom_sf(sbp_geos_states %>% filter(STUSPS == "LA"), 
          mapping = aes(),
          fill = "#D5D5D4",
          color = "#ADABAC") +
  geom_sf(cdrzs_tracts_places_clean %>% filter(sbp_geos == "geos")%>%
            mutate(group = "GEOs CDRZs"),
          mapping = aes(color = group),
          fill = "#FCCB41") +
  geom_sf(cdrzs_tracts_places_clean %>% filter(sbp_geos == "sbp") %>%
            mutate(group = "SBP CDRZs"),
          mapping = aes(color = group),
          fill = "#46ABDB")  +
  geom_sf(cdrzs_tracts_places_clean %>% filter(sbp_geos == "both") %>%
            mutate(group = "Overlapping CDRZs"),
          mapping = aes(color = group),
          fill = "#78C26D")  +
  #geom_sf(geos_sbp_cdrzs_places %>%
          #   mutate(group = "Surrounding Places"),
          # mapping = aes(color = group),
          # fill = "#E46AA7",
          # size = 0.01) +
  scale_color_manual(values = c(
    "GEOs CDRZs" = "#FCCB41",
    "SBP CDRZs" = "#46ABDB",
    "Overlapping CDRZs" = "#78C26D")) + 
 #  "Surrounding Places" = "#E46AA7"), 
  #name = "Key",
  #labels = c( "GEOs CDRZs", "SBP CDRZs", "Overlapping CDRZs", "Surrounding Places")) +
  theme_urbn_map() 


print(sbp_geos_map_places)
ggsave("sbp_geos_map_places.png", plot = sbp_geos_map_places, width = 8, height = 6, dpi = 300)




##########################################################################################
##########################################################################################
####################### SUMMARY TABLES           #########################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

combined_footprint_row <- partner_cdrz_all_data %>% 
  filter(!is.na(sbp_geos)) %>% 
  summarize(
    Jurisdiction = "Combined Footprint", 
    cdrz_count = n(),
    total_pop = sum(total_pop),
    #total_pop_county = sum(total_pop_county, na.rm = TRUE),
    per_65_over = mean(per_65_over, na.rm = TRUE),
    per_non_hisp_white = mean(per_non_hisp_white, na.rm = TRUE),
    per_non_white = mean(per_non_white, na.rm = TRUE),
    median_hh_income = mean(median_hh_income, na.rm = TRUE),
    per_broadband = mean(per_broadband, na.rm = TRUE), 
    per_rural = sum(urban_designation == "rural")/n()) 

geos_footprint_row <- partner_cdrz_all_data %>% 
  filter(sbp_geos %in% c("geos", "both")) %>%  
  summarize(
    Jurisdiction = "GEOs Footprint", 
    cdrz_count = n(),
    total_pop = sum(total_pop),
    #total_pop_county = sum(total_pop_county, na.rm =TRUE),
    per_65_over = mean(per_65_over, na.rm = TRUE),
    per_non_hisp_white = mean(per_non_hisp_white, na.rm = TRUE),
    per_non_white = mean(per_non_white, na.rm = TRUE),
    median_hh_income = mean(median_hh_income, na.rm = TRUE),
    per_broadband = mean(per_broadband, na.rm = TRUE), 
    per_rural = sum(urban_designation == "rural")/n()) 

sbp_footprint_row <- partner_cdrz_all_data %>% 
  filter(sbp_geos %in% c("sbp", "both")) %>%    
  summarize(
    Jurisdiction = "SBP Footprint", 
    cdrz_count = n(),
    total_pop = sum(total_pop),
    per_65_over = mean(per_65_over, na.rm = TRUE),
    per_non_hisp_white = mean(per_non_hisp_white, na.rm = TRUE),
    per_non_white = mean(per_non_white, na.rm = TRUE),
    median_hh_income = mean(median_hh_income, na.rm = TRUE),
    per_broadband = mean(per_broadband, na.rm = TRUE), 
    per_rural = sum(urban_designation == "rural")/n())


all_cdrzs_row_total <- partner_cdrz_all_data %>%
  summarize(
    Jurisdiction = "All CDRZs Footprint", 
    cdrz_count = n(),
    total_pop = sum(total_pop, na.rm =TRUE),
    per_65_over = mean(per_65_over, na.rm = TRUE),
    per_non_hisp_white = mean(per_non_hisp_white, na.rm = TRUE),
    per_non_white = mean(per_non_white, na.rm = TRUE),
    per_non_hisp_black = mean(per_non_hisp_black, na.rm = TRUE),
    median_hh_income = mean(median_hh_income, na.rm = TRUE),
    per_broadband = mean(per_broadband, na.rm = TRUE), 
    per_rural = sum(urban_designation == "rural", na.rm= TRUE)/n())

spb_geos_countY_population <- spb_geos_counties %>%
  group_by(county_state_code) %>%
  summarise(
    county_population = sum(total_pop, na.rm = TRUE),
    state = first(state))

##getting summaries by partner type
geos_states <- geos_cdrz %>% 
  group_by(state) %>% 
  summarise(
    total_cdrz = n(),
    county_count= n_distinct(county_name)) 

sbp_counties <- sbp_cdrz %>% 
  group_by(state, county) %>% 
  summarise(
    total_cdrz = n()) 
#county_count= n_distinct(county_name)) 

cobined_footprint_table <- walmart_cdrzs %>% 
  group_by(overlapping, partner, state) %>% 
  summarize(
    CDRZ_count = n(),
    counties = n_distinct(county), 
    county_names = str_c(unique(county), collapse = ", "),
    CDRZs_geoids = str_c(geoid, collapse = ", ")) 

write_xlsx(cobined_footprint_table, "cobined_footprint_table.xlsx")

#####################################################################################
#####################################################################################
#####################################################################################
#################  DEMOGRAPHIC BREAKDOWNS FOR SBP AND GEOS###########################
#####################################################################################
#####################################################################################
#####################################################################################

geos_summary <- all_cdrzs_full_data %>% 
  filter(partner == "geos")%>%
  group_by(geoid) %>%
  summarize(
    partner = "GEOS",
    overlapping = first(overlapping),
    cdrz_count = n(),
    county_state_code = first(county_state_code), 
    designation_date = first(cdrz_designation_date),
    county_name = first(county_name), 
    state_name = first(state), 
    state_abbreviation = first(state_abbreviation),
    region = first(region),
    navigator_name = str_c(unique(navigator_name), collapse = ", "),
    grouped =  str_c(unique(grouped), collapse = ", "),
    urban_designation = first(urban_designation),
    risk_rating_average = mean(RISK_SCORE, na.rm = TRUE), 
    expected_annual_loss_averge = mean(EAL_SCORE, na.rm = TRUE), 
    social_vulnerability_average = mean(SOVI_SCORE, na.rm = TRUE),
    community_resilience_average = mean(RESL_SCORE, na.rm = TRUE),
    total_pop_in_cdrzs = sum(total_pop, na.rm =TRUE),
    county_population= first(county_population),
    across( 
      .cols = c(starts_with("per_"), median_hh_income), 
      .fns = ~ mean(.x, na.rm = TRUE), 
      .names = "mean_{.col}")) #~ mean(.x, na.rm =TRUE), 
     
write_xlsx(geos_summary, "geos_summary.xlsx")

  
sbp_summary <- all_cdrzs_full_data %>% 
  filter(partner == "sbp") %>%  
  group_by(geoid) %>%
  summarize(
    partner = "SBP",
    overlapping = first(overlapping),
    cdrz_count = n(),
    county_state_code = first(county_state_code), 
    designation_date = first(cdrz_designation_date),
    county_name = first(county_name), 
    state_name = first(state), 
    state_abbreviation = first(state_abbreviation),
    region = first(region),
    navigator_name = str_c(unique(navigator_name), collapse = ", "),
    grouped =  str_c(unique(grouped), collapse = ", "),
    urban_designation = first(urban_designation),
    risk_rating_average = mean(RISK_SCORE, na.rm = TRUE), 
    expected_annual_loss_averge = mean(EAL_SCORE, na.rm = TRUE), 
    social_vulnerability_average = mean(SOVI_SCORE, na.rm = TRUE),
    community_resilience_average = mean(RESL_SCORE, na.rm = TRUE),
    total_pop_in_cdrzs = sum(total_pop, na.rm =TRUE),
    county_population= first(county_population),
    across( 
      .cols = c(starts_with("per_"), median_hh_income), 
      .fns = ~ mean(.x, na.rm = TRUE), 
      .names = "mean_{.col}"))    


sbp_summary_for_hannah <- sbp_places_cdrz %>% 
  group_by(geoid) %>%
  summarize(
    cdrz_count = n(),
    total_pop = sum(total_pop),
    per_65_over = mean(per_65_over, na.rm = TRUE),
    per_non_hisp_white = mean(per_non_hisp_white, na.rm = TRUE),
    per_non_white = mean(per_non_white, na.rm = TRUE),
    median_hh_income = mean(median_hh_income, na.rm = TRUE),
    per_broadband = mean(per_broadband, na.rm = TRUE), 
    per_rural = sum(urban_designation == "rural")/n())
   













