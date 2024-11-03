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
library(here)
library(sf)
library(janitor)
library(tidylog)
library(tidycensus)
library(purrr)
library(sf)
library(skimr)

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
acs_variable_list <- load_variables(year = 2021,
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
  year = 2022,
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
  ) %>% 
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

acs_cdrz <- cdrz_data %>%
  left_join(acs_demographics_clean %>% select(-state_fips, -tract_fips, -state_name), by = "geoid") 

# save ACS demographics csv
write_csv(acs_demographics_clean, "acs_demographic_clean.csv")


###################################################################################
###################################################################################
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
  #creating a county_fips column for the acs_demographics_tract dataset to use when joining the rural urban dataset
  mutate(county_fips = paste0(STATEFP, COUNTYFP)) %>% 
  rename(county_name = NAMELSADCO,
         state_abbreviation = STUSPS) %>% select(-COUNTYFP,-STATEFP, -STATE_NAME, -county_type)
  

##connecticut has one missing CDRZs tract- likely because of the tract alignment from 2023-2021
missing <- acs_cdrz %>% filter(is.na(total_pop))

################################################################################
################################################################################
###########################    SBP FOOTPRINT    ################################
################################################################################
################################################################################
#filtering the cdrzs overseen by SBP (Mississippi and Texas= all CDRZs, South Carolina = all CDRZ in Berkeley-Dorchester-Charleston COG, Florida= Charlotte County, Alabama = City of Mobile) 54walmart


sbp_cdrz<- acs_cdrz_geography %>%
  filter((state_abbreviation == "TX")| ## county_name %in% c("Orange County", "Jefferson County", "Hardin County", "Jasper County")) |
           (state_abbreviation == "AL" & county_name == "Mobile County") |
           (state_abbreviation == "SC" & county_name %in% c("Berkeley County", "Dorchester County", "Charleston County")) |
           (state_abbreviation == "MS" & county_name %in% c("Covington County", "Forrest County", "George County", "Greene County", "Hancock County", 
                                                "Harrison County", "Jackson County", "Jefferson Davis County", "Jones County", 
                                                "Lamar County", "Marion County", "Pearl River County", "Perry County", "Stone County", 
                                                "Wayne County"))|
           (state_abbreviation == "FL" & county_name == "Charlotte County")) %>% 
  mutate(partner = "sbp", 
         grouped = case_when(
           state_abbreviation == "MS" ~ "Southern Mississippi Planning District",
           state_abbreviation == "TX" & county_name %in% c("Orange County", "Jefferson County", "Hardin County", "Jasper County") ~ "Southeast Texas Regional Planning Commission", 
           state_abbreviation == "TX" & !county_name %in% c("Orange County", "Jefferson County", "Hardin County", "Jasper County") ~ "Texas State's General Land Office (GLO)",
           state_abbreviation == "SC" ~ "Berkeley-Charleston-Dorchester Council of Government (BCDCOG)")
           )

################################################################################
################################################################################
###########################    GEOS FOOTPRINT    ###############################
################################################################################
################################################################################

geos_cdrz<- acs_cdrz_geography %>%
  filter((state_abbreviation == "NC" & county_name %in% c("Beaufort County", "Carteret County", "Craven County", "Dare County", "Hyde County", 
                                              "New Hanover County", "Onslow County", "Sampson County",
                                              "Tyrrell County", "Washington County"))  | ## washington counties?? 
           (state_abbreviation == "SC" & county_name %in% c("Beaufort County", "Berkeley County", "Charleston County", "Dorchester County", "Horry County"))|
           (state_abbreviation == "GA" & county_name %in% c("Bulloch County", "Chatham County","Liberty County", "McIntosh County", "Seminole County", "Wayne County")) |
           (state_abbreviation == "FL" & county_name %in% c( "Bay County", "Brevard County", "Broward County", "Charlotte County", "Collier County",  "DeSoto County",
                                                 "Hendry County", "Hillsborough County", "Indian River County", "Martin County", "Miami-Dade County", "Palm Beach County",
                                                 "Pinellas County", "St. Lucie County", "Washington County"))) %>% 
  mutate(partner = "geos", 
         navigator_name = case_when(
           state_abbreviation == "NC" ~ "Holly White & Helene Whetherington",
           state_abbreviation == "SC" ~  "Adelaide",
           state_abbreviation == "GA" ~ "Courtney & Monet",
           state_abbreviation == "FL" & county_name %in% c( "Brevard County", "Indian River County", "Martin County", "St. Lucie County")~ "Holly Abeels", 
           state_abbreviation == "FL" & county_name == "Bay County" ~ "Jackson",
           state_abbreviation == "FL" & county_name == "Washington County" ~ "Stevenson",
           state_abbreviation == "FL" & county_name %in% c("Hillsborough County", "Pinellas County") ~ "Madhosingh-Hector",
           state_abbreviation == "FL" & county_name %in% c("Broward County", "Miami-Dade County", "Palm Beach County") ~ "Betancourt & Hart",
           state_abbreviation == "FL" & county_name %in% c("DeSoto County", "Hendry County") ~ "Ubeda",
           state_abbreviation == "FL" & county_name %in% c("Charlotte County", "Collier County") ~ "Wilson"), 
         grouped = case_when(
           state_abbreviation == "NC" & county_name == "Beaufort County" ~ "Beaufort Group",
           state_abbreviation == "NC" & county_name == "Craven County" ~ "Craven Group",
           state_abbreviation == "NC" & county_name == "Dare County" ~ "Dare Group",
           state_abbreviation == "NC" & county_name == "Hyde County" ~ "Hyde Group",
           state_abbreviation == "SC" & county_name == "Charleston County" ~ "Charleston Group",
           state_abbreviation == "SC" & county_name == "Dorchester County" ~ "Dorchester Group",
           state_abbreviation == "GA" & county_name == "Chatham County" ~ "Chatham Group",
           state_abbreviation == "GA" & county_name == "Liberty County" ~ "Liberty Group",
           state_abbreviation == "FL" & county_name == "St. Lucie County" ~ "St. Lucie Group",
           state_abbreviation == "FL" & county_name == "Broward County" & geoid20 %in% c(12011030500, 12011030601) ~ "Broward Group (1)",
           state_abbreviation == "FL" & county_name == "Broward County" & geoid20 %in% c(12011050207, 12011050208) ~ "Broward Group (2)",
           state_abbreviation == "FL" & county_name == "Miami-Dade County" & geoid20 %in% c(12086009010, 12086009040, 12086009102) ~ "Miami-Dade Group",
           state_abbreviation == "FL" & county_name == "Palm Beach County" ~ "Palm Beach Group",
           state_abbreviation == "FL" & county_name == "Bay County" ~ "Bay Group",
           state_abbreviation == "FL" & county_name == "Washington County" ~ "Washington Group",
           state_abbreviation == "FL" & county_name == "Hendry County" ~ "Hendry Group",
           state_abbreviation == "FL" & county_name == "Collier County" ~ "Collier Group"))


################################################################################
################################################################################
#######################    COMBINED FOOTPRINT    ###############################
################################################################################
################################################################################

overlapping_cdrzs <- bind_rows(sbp_cdrz, geos_cdrz) %>%
  group_by(geoid) %>% 
  count() %>% 
  rename(shared = n) %>% 
  mutate(overlapping = case_when(
    shared == 1 ~ "No overlap", 
    shared == 2 ~ "Partner overlap" )) 

#sbp and geos cdrzs with partner overlap 
walmart_cdrzs <- bind_rows(sbp_cdrz, geos_cdrz) %>% 
  left_join(overlapping_cdrzs %>% select(overlapping), by = "geoid")

#THERE ARE 9 CDRZS THAT FALL UNDER BOTH SBP AND GEOS 
overlapping <- walmart_cdrzs %>% filter(overlapping== "Partner overlap" )


##getting summaries by partner type
geos_states <- geos_cdrz %>% 
  group_by(state) %>% 
  summarise(
    total_cdrz = n(),
    county_count= n_distinct(county_name)) 

sbp_states <- sbp_cdrz %>% 
  group_by(state) %>% 
  summarise(
    total_cdrz = n(),
    county_count= n_distinct(county_name)) 


cobined_footprint_table <- walmart_cdrzs %>% 
  group_by(overlapping, partner, state) %>% 
  summarize(
    CDRZ_count = n(),
    counties = n_distinct(county), 
    county_names = str_c(unique(county), collapse = ", "),
    CDRZs_geoids = str_c(geoid, collapse = ", ")) 

write_csv(cobined_footprint_table, "cobined_footprint_table.csv")



#################################################################################
#################################################################################
########################    Adding additional data      #########################
#################################################################################
##########################  URBAN RURAL     #####################################
#################################################################################

# URBAN RURAL  ------------------------------------------------------------

# Rural urban measure downloaded from the USDA's 2023 Rural-Urban Continuum Codes
# https://www.ers.usda.gov/data-products/atlas-of-rural-and-small-town-america/download-the-data/


# rural / urban flag
rural_urban = read.csv(here("data", "data-raw", "rural_urban_continuum_code_2023.csv")) %>% 
  clean_names() %>% 
  mutate(fips = str_pad(fips, 5, "left", "0"), 
         urban_designation = case_when(
           rucc_2023 %in% c(4:9) ~ "rural", 
           rucc_2023 %in% c(1:3) ~ "urban"
         )) %>%
    rename(county_fips = fips) %>%
  select(county_fips, urban_designation)

# joining rural urban with acs demographic tracts
acs_cdrz_geography_rural_urban <- walmart_cdrzs %>%
 left_join(rural_urban, by = c("county_fips" = "county_fips")) 

################################################################################
################################################################################
##################  Headwater Rural Capcity Index   ############################
################################################################################
################################################################################
##Headwater Rural Capcity Index Data 2024 (https://headwaterseconomics.org/equity/rural-capacity-map/#methods)

county_capacity = read.csv(here("data", "data-raw", "HE_Rural_Capacity_Index_March_2024.csv")) %>%
      mutate(FIPS = str_pad(FIPS, 5, "left", "0")) %>% 
  rename(county_fips = FIPS) %>% 
  clean_names()

capacity_ru_acs_tracts <- acs_cdrz_geography_rural_urban %>%
  left_join(county_capacity %>% select(rural_capacity_index, county_fips, percent_of_counties_nationwide_with_higher_capacity), by = c("county_fips" = "county_fips")) 

################################################################################
################################################################################
###################### NRI Ratings Data       ##################################
################################################################################
################################################################################
### NRI Ratings Data from https://hazards.fema.gov/nri/
nri_index_tracts = read.csv(here("data", "data-raw", "nri", "NRI_Table_CensusTracts.csv")) 

### joining nri index to walmart cdrzs
walmart_cdrzs_nri <- acs_cdrz_geography_rural_urban %>% 
  left_join(nri_index_tracts %>% select(NRI_ID, COUNTY, TRACT, POPULATION, RISK_VALUE,
                                        RISK_SCORE,RISK_RATNG, EAL_SCORE, EAL_RATNG,
                                        SOVI_SCORE, SOVI_RATNG, RESL_SCORE, RESL_RATNG),
            by = c("nri_id" = "NRI_ID")) 

##########################################################################################
##########################################################################################
################################   Capital flows data      ###############################
##########################################################################################
##########################################################################################
###reading in capital flows from Urban Institute: https://datacatalog.urban.org/dataset/capital-flows-disparities-cities-counties-and-states
capital_flows_by_county <- read.csv(here("data", "data-raw", "capital-flows-and-disparities.csv"))

##joinging in capital flows to cdzrs - THERE are only 73 CDRZs tracts that have this information (out of 107) 
capital_flow_walmart_cdrz <- walmart_cdrzs_nri %>% filter(county_name %in% capital_flows_by_county$county_name & 
                                                            state_abbreviation %in% capital_flows_by_county$state)

##########################################################################################
##########################################################################################
####################### SUMMARY TABLES           #########################################
##########################################################################################
##########################################################################################


combined_footprint_row <- walmart_cdrzs_nri %>% 
  filter(!(overlapping == "Partner overlap" & partner == "sbp")) %>%  #removing duplicates
  summarize(
    Jurisdiction = "Combined Footprint", 
    cdrz_count = n(),
    total_pop = sum(total_pop),
    per_65_over = mean(per_65_over, na.rm = TRUE),
    per_non_hisp_white = mean(per_non_hisp_white, na.rm = TRUE),
    per_non_white = mean(per_non_white, na.rm = TRUE),
    median_hh_income = mean(median_hh_income, na.rm = TRUE),
    per_broadband = mean(per_broadband, na.rm = TRUE), 
    per_rural = sum(urban_designation == "rural")/n()) 

geos_footprint_row <- walmart_cdrzs_nri %>% 
  filter(partner == "geos") %>%  
  summarize(
    Jurisdiction = "GEOs Footprint", 
    cdrz_count = n(),
    total_pop = sum(total_pop),
    per_65_over = mean(per_65_over, na.rm = TRUE),
    per_non_hisp_white = mean(per_non_hisp_white, na.rm = TRUE),
    per_non_white = mean(per_non_white, na.rm = TRUE),
    median_hh_income = mean(median_hh_income, na.rm = TRUE),
    per_broadband = mean(per_broadband, na.rm = TRUE), 
    per_rural = sum(urban_designation == "rural")/n()) 

sbp_footprint_row <- walmart_cdrzs_nri %>% 
  filter(partner == "sbp") %>%  
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

#all_cdrzs_full_data <- walmart_cdrzs_nri 

all_cdrzs_row_total <- all_cdrzs_full_data %>%
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



#####################################################################################
#####################################################################################
#####################################################################################
#################  DEMOGRAPHIC BREAKDOWNS FOR SBP AND GEOS###########################
#####################################################################################
#####################################################################################
#####################################################################################



geos_summary <- walmart_cdrzs_nri %>% 
  filter(partner == "geos") %>%  
  group_by(county_fips) %>%
  summarize(
    partner = "GEOs Institute",
    cdrz_count = n(),
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
    total_pop_by_county = sum(total_pop, na.rm =TRUE),
    across( 
      .cols = c(starts_with("per_"), median_hh_income), 
      .fns = mean, 
      .names = "mean_{.col}"))
write_csv(geos_summary, "geos_summary.cvs")

  
sbp_summary <- walmart_cdrzs_nri %>% 
  filter(partner == "sbp") %>%  
  group_by(county_fips) %>%
  summarize(
    partner = "SBP",
    cdrz_count = n(),
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
    total_pop_by_county = sum(total_pop, na.rm =TRUE),
    across( 
      .cols = c(starts_with("per_"), median_hh_income), 
      .fns = mean, 
      .names = "mean_{.col}"))    
   
write_csv(sbp_summary, "sbp_summary.cvs")


combined_sbp_geos_cdrz_data <- rbind(geos_summary, sbp_summary)

write_csv(combined_sbp_geos_cdrz_data, "combined_sbp_geos_cdrz_data.cvs")




























