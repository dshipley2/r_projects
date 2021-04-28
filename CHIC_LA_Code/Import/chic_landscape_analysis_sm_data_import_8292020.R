# Title: CHIC Landscape Analysis - Summer/Fall 2020, DATA IMPORT
# Author: Danica Shipley
# Date: 8/29/2020

# Background----
# This code is to download and save raw Surveymonkey data for the CHIC Landscape Analysis
# There are 2 surveys: community (English & Spanish) and organizational

# SET-UP-------------------------------------------------------------

# Load packages
library(pacman)
p_load(surveymonkey, here, writexl, dplyr, janitor, stringr)

# IMPORT DATA--------------------------------------------------------

# Find survey IDS
surveys <- browse_surveys(200) # see your most recent 200 surveys

#### Save individual surveys as a dataframe - ENGLISH VERSION
individual_survey_eng <- fetch_survey_obj(290568837) %>%
  parse_survey

# There are duplicate records based on the email address collected at the end so want to drop duplicates before saving the "raw" file
individual_survey_eng_unique <- individual_survey_eng %>%
  # Removes metadata not needed
  select(-c(3:5,7:10, 122:131)) %>%
  # Replace values in the "type" and "required" columns with NA so that I can drop records that have no responses to remaining fields
  mutate(
    respondent_id = case_when(
      # For records where type and email have values, get rid of the respondent id
      (type == "email" & required == "FALSE") ~ NA_character_,
      TRUE ~ respondent_id),
    # Drop values for the type field
    type = case_when(
      type == "email" ~ NA_character_,
      TRUE ~ type),
    # Drop values for the required field
    required = case_when(
      required == "FALSE" ~ NA,
      TRUE ~ required)) %>%
  # Removes duplicate data caused by the last question on email address (column previously removed)
  remove_empty("rows")

#### Save individual surveys as a dataframe - SPANISH VERSION
individual_survey_spa <- fetch_survey_obj(291549868) %>%
  parse_survey()

#### Check to see if they're are any duplicates between the English and Spanish Versions of the individual surveys
# If there are duplicates, save the email addresses to a dataframe and filter them out of the Spanish survey
individual_duplicates <- individual_survey_eng %>%
  filter(email %in% individual_survey_spa$email) %>%
  select(email) %>%
  unique()

## Create a function %notin% to exclude respondents from the Spanish survey who are in the duplicate dataframe
`%notin%` <- Negate(`%in%`)

# There are duplicate records based on the email address collected at the end so want to drop duplicates before saving the "raw" file
individual_survey_spa_unique <- individual_survey_spa %>%
  filter(email %notin% individual_duplicates$email) %>%
  # Removes metadata not needed
  select(-c(3:5,7:10, 122:131)) %>%
  # Replace values in the "type" and "required" columns with NA so that I can drop records that have no responses to remaining fields
  mutate(
    respondent_id = case_when(
      # For records where type and email have values, get rid of the respondent id
      (type == "email" & required == "FALSE") ~ NA_character_,
      TRUE ~ respondent_id),
    # Drop values for the type field
    type = case_when(
      type == "email" ~ NA_character_,
      TRUE ~ type),
    # Drop values for the required field
    required = case_when(
      required == "FALSE" ~ NA,
      TRUE ~ required)) %>%
  # Removes duplicate data caused by the last question on email address (column previously removed)
  remove_empty("rows")

#Save the organizational surveys as a dataframe
partner_org_survey <- fetch_survey_obj(290570312) %>%
  parse_survey()

# SAVE DATA----------------------------------------------------------

# Save as an .rda file for cleaning
save("individual_survey_eng_unique", "individual_survey_spa_unique", "partner_org_survey",  file = "Data/Raw/chic_landscape_analysis_surveys.rda")

# Save as an excel file for sharing raw data back with CHIC
write_xlsx(individual_survey_eng,"Outputs/Data/individual_survey_eng.xlsx")
write_xlsx(individual_survey_spa,"Outputs/Data/individual_survey_spa.xlsx")
write_xlsx(partner_org_survey,"Outputs/Data/partner_org_survey.xlsx")

