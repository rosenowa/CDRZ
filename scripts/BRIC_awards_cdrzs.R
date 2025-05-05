library(janitor)
library(here)
library(readr)
library(dplyr)
library(writexl)
library(stringr)
#("rfema", repos = "https://ropensci.r-universe.dev")
library(rfema)

#reading in Hazard Mitigation Subapplications from Open FEMA
HmaSubapplications_open_fema = rfema::open_fema(
  data_set = "HmaSubapplications",
  ask_before_call = FALSE) %>%
  janitor::clean_names() %>% 
  #Filtering for BRIC
  filter(program == "Building Resilient Infrastructure and Communities")



bric_awards_1<- filtered_bric_data_counties %>% mutate(total_subapplication_amount = as.numeric(total_subapplication_amount))

bric_awards_wide <- HmaSubapplications_open_fema %>%
  #splitting the benefiting_counties column by comma to create a list of the counties 
  mutate(benefiting_counties = str_split(benefiting_counties, ",")) %>%
  #splitting the lists into columns and naming the columns "benefiting_counties_1, benefiting_counties_2, etc.)
  unnest_wider(benefiting_counties, names_sep = "_")%>%
  #capitalizing the first letter of each word
  mutate(across(starts_with("benefiting_counties"), ~ str_to_title(.))) %>%
  #removing white space from before and after the county name
  mutate(across(starts_with("benefiting_counties"), ~ str_trim(.)))

#creating a list of counties grouped by state to check the bric awards dataset 
#to find counties in the benefiting_counties columns that match the state abbreviation
sbp_geos_counties <- list(
  TX = c("Orange County", "Jefferson County", "Hardin County", "Jasper County"),
  AL = c("Mobile County"),
  SC = c("Berkeley County", "Dorchester County", "Charleston County"),
  MS = c("Covington County", "Forrest County", "George County", "Greene County", 
         "Hancock County", "Harrison County", "Jackson County", "Jefferson Davis County", 
         "Jones County", "Lamar County", "Marion County", "Pearl River County", 
         "Perry County", "Stone County", "Wayne County"),
  FL = c("Charlotte County", "Bay County", "Brevard County", "Broward County", "Collier County", 
         "DeSoto County", "Hendry County", "Hillsborough County", "Indian River County", "Martin County", 
         "Miami-Dade County", "Palm Beach County", "Pinellas County", "St. Lucie County", "Washington County"),
  NC = c("Beaufort County", "Carteret County", "Craven County", "Dare County", "Hyde County", 
         "New Hanover County", "Onslow County", "Sampson County", "Tyrrell County", "Washington County"),
  GA = c("Bulloch County", "Chatham County", "Liberty County", "McIntosh County", "Seminole County", 
         "Wayne County")) 

#creating columns to flag SBP and GEOs locations
#if the subapplicant_state_abbreviation equals the state abbreviation, then it proceedes to checking
#for county names that are listed in sbp_geos_counties for that given state.
#If there is at least 1 match across the benefiting counties columns, then the value will be 1. 
#this is repeated for sbp and geos using their defined footprints 
bric_awards_wide_1 <- bric_awards_wide %>%
    mutate(
      # Create 'sbp' column based on subapplicant_state_abbreviation and county matches
      sbp = case_when(
        (subapplicant_state_abbreviation == "TX" & 
           rowSums(across(starts_with("benefiting_counties_"), ~ . %in% sbp_geos_counties$TX)) > 0) ~ "1",
        (subapplicant_state_abbreviation == "AL" & 
           rowSums(across(starts_with("benefiting_counties_"), ~ . %in% sbp_geos_counties$AL)) > 0) ~ "1",
        (subapplicant_state_abbreviation == "SC" & 
           rowSums(across(starts_with("benefiting_counties_"), ~ . %in% sbp_geos_counties$SC)) > 0) ~ "1",
        (subapplicant_state_abbreviation == "MS" & 
           rowSums(across(starts_with("benefiting_counties_"), ~ . %in% sbp_geos_counties$MS)) > 0) ~ "1",
        (subapplicant_state_abbreviation == "FL" & 
           rowSums(across(starts_with("benefiting_counties_"), ~ . %in% sbp_geos_counties$FL)) > 0) ~ "1",
        TRUE ~ "0"
      ),
      
      # Create 'geos' column based on subapplicant_state_abbreviation and county matches
      geos = case_when(
        (subapplicant_state_abbreviation == "NC" & 
           rowSums(across(starts_with("benefiting_counties_"), ~ . %in% sbp_geos_counties$NC)) > 0) ~ "1",
        (subapplicant_state_abbreviation == "SC" & 
           rowSums(across(starts_with("benefiting_counties_"), ~ . %in% sbp_geos_counties$SC)) > 0) ~ "1",
        (subapplicant_state_abbreviation == "GA" & 
           rowSums(across(starts_with("benefiting_counties_"), ~ . %in% sbp_geos_counties$GA)) > 0) ~ "1",
        (subapplicant_state_abbreviation == "FL" & 
           rowSums(across(starts_with("benefiting_counties_"), ~ . %in% sbp_geos_counties$FL)) > 0) ~ "1",
        TRUE ~ "0"), 
      #create a column that combines sbp_geos columns 
      sbp_geos = case_when(
        sbp == "1" & geos == "1" ~ "both",
        sbp == "0" & geos == "1" ~ "geos",
        sbp == "1" & geos == "0" ~ "sbp",
        TRUE ~ na_chr))
  
#filtering out NAs in sbp_geos to create the sbp_geo dataset
bric_awards_sbp_geos <-  bric_awards_wide_1 %>%
  filter(!is.na(sbp_geos)) %>%
  #removing the columns created with individual county names
  select(-starts_with("benefiting_counties_"))

#Filtering df for only Accepted and Submitted awards
#Counting the number of projects per year, status, sbp_geos, and state
#and summing the total_subapplication_amount
bric_awards_funding <- bric_awards_sbp_geos %>%
  #converting the total_subapplication_amount column to be numeric instead of a character
  mutate(total_subapplication_amount = as.numeric(total_subapplication_amount)) %>%
  filter(status %in% c("subAwardAccepted", "submittedToFEMA")) %>%
  group_by(fiscal_year,status,sbp_geos, subapplicant_state) %>%
  summarise(
    num_of_projects = n(),
    total_subapplication_amount =sum(total_subapplication_amount)) %>%
  #adding a total 
  adorn_totals("row")

#writing csv file
#write.csv(bric_awards_funding,"bric_awards_funding.csv")



