# Title: CHIC Landscape Analysis - Summer/Fall 2020, DATA CLEANING
# Author: Danica Shipley
# Date: 9/12/2020

# Background----
# This code is to clean the survey data. In doing so, the spanish version of the individual survey will be translated
  # to English and combined with the English version. Data will also be reshaped for easier analysis for both individual and organizational surveys.
# There are 2 surveys: community (English & Spanish) and organizational

# SET-UP-------------------------------------------------------------

# Load packages
library(pacman)
p_load(here, writexl, dplyr, janitor, stringr, tidyr, stringr, lubridate, janitor, readr, readxl, tidyverse, writexl)

# IMPORT DATA--------------------------------------------------------
load(here::here("Data/Raw/chic_landscape_analysis_surveys.rda"))

# CLEAN DATA---------------------------------------------------------

# Rename Spanish Survey Columns----
# Remove first fields from the English survey
individual_survey_eng_unique <- individual_survey_eng_unique %>%
  select(-c(type, required))

# Grab column names from English survey
eng_col_names <- names(individual_survey_eng_unique)

# Remove first fields from the Spanish survey
ind_spa_translated <- individual_survey_spa_unique %>%
  select(-c(type, required))

# Swap out Spanish column names for English Column Names
names(ind_spa_translated)[1:112] <- eng_col_names

# Combine English and Spanish Surveys----
ind_survey_all_responses <-
  individual_survey_eng_unique %>%
  mutate(language = "English") %>%
  bind_rows(ind_spa_translated %>% mutate(language = "Spanish")) %>%
  # Reshape survey to have main question, question, response
  gather(c(2:105), key = "question", value = "response") %>%
  # Split question column to be primary_question, question
  separate(question, c("main_question", "question"), sep = " - ") %>%
  mutate(main_question = case_when(
    str_detect(main_question, "What is your gender identity") ~ "gender_identity",
    str_detect(main_question, "What is your race") ~ "race",
    TRUE ~ main_question)) %>%
  mutate(question = case_when(
    is.na(question) ~ main_question,
    TRUE ~ question)) %>%
  # Rename gender and race question
  mutate(question = case_when(
    (main_question == "race" | main_question == "gender_identity") ~ main_question,
    TRUE ~ question)) %>%
  # Remove empty responses
  filter(!is.na(response))%>%
  clean_names()

# Clean Up Race (question was multi-select)
race_cleaned <- ind_survey_all_responses %>%
  select(respondent_id, language, question, response) %>%
  filter(question == "race") %>%
  group_by(respondent_id, question) %>%
  mutate(race_count = n()) %>%
  arrange(respondent_id)

# Join the cleaned race data in with main dataset
ind_survey_all_responses_final <- ind_survey_all_responses %>%
  left_join(race_cleaned, by = c("respondent_id" = "respondent_id", "language" = "language", "response" = "response", "question" = "question")) %>%
  group_by(respondent_id) %>%
  mutate(response = case_when(
    race_count > 1 ~ "2 or More Race/Ethnicities",
    TRUE ~ response)) %>%
  unique()

# Translate Spanish Responses ----
ind_demos_translated <- ind_survey_all_responses_final %>%
  select(language, respondent_id, age = what_is_your_age, marital_status = what_is_your_marital_status, num_children = how_many_children_under_the_age_of_18_do_you_have,
         childrens_caretaker = are_you_your_child_ren_s_primary_caretaker, edu_level = whats_the_highest_level_of_school_youve_completed,
         household_income = what_is_your_current_combined_household_income_before_taxes, city = what_city_do_you_live_in, main_question:response) %>%
  # Change data types from factor to character
  mutate(age = as.character(age),
         marital_status = as.character(marital_status),
         childrens_caretaker = as.character(childrens_caretaker),
         edu_level = as.character(edu_level),
         household_income = as.character(household_income),
         city = as.character(city),
         num_children = as.numeric(num_children)) %>%
  # Translations: Spanish to English
  # Demographic translations
  mutate(age = case_when(
    str_detect(age, "Menos de 18") ~ "Under 18",
    str_detect(age, "18 a 24") ~ "18-24",
    str_detect(age, "25 a 34") ~ "25-34",
    str_detect(age, "35 a 44") ~ "35-44",
    str_detect(age, "45 a 54") ~ "45-54",
    str_detect(age, "55 a 64") ~ "55-64",
    str_detect(age, "65 a") ~ "65+",
    TRUE ~ age),
    marital_status = case_when(
      str_detect(marital_status, "Soltero") ~ "Single, never married",
      str_detect(marital_status, "Casado") ~ "Married or domestic partnership",
      str_detect(marital_status, "Viudo") ~ "Widowed",
      str_detect(marital_status, "Divorciado") ~ "Divorced",
      str_detect(marital_status, "Separado") ~ "Separated",
      TRUE ~ marital_status),
    childrens_caretaker = case_when(
      str_detect(childrens_caretaker, "Sí") ~ "Yes",
      str_detect(childrens_caretaker, "No") ~ "No",
      str_detect(childrens_caretaker, "No Aplica") ~ "Not Applicable",
      TRUE ~ childrens_caretaker),
    edu_level = case_when(
      str_detect(edu_level, "No ha completado ning") ~ "No schooling completed",
      str_detect(edu_level, "Preescolar a 8") ~ "Nursery school to 8th grade",
      str_detect(edu_level, "Secundaria parcialmente") ~ "Some high school, no diploma",
      str_detect(edu_level, "Se graduó de la Secundaria") ~ "High school graduate, diploma or the equivalent (for example: GED)",
      str_detect(edu_level, "Crédito Universitario Parcial") ~ "Some college credit, no degree",
      str_detect(edu_level, "Entrenamiento técnico") ~ "Trade/technical/vocational training",
      str_detect(edu_level, "Diplomado") ~ "Associate degree",
      str_detect(edu_level, "Bachillerato") ~ "Bachelor’s degree",
      str_detect(edu_level, "Maestría") ~ "Master’s degree",
      str_detect(edu_level, "Licenciatura Profesional") ~ "Professional degree",
      str_detect(edu_level, "Doctorado") ~ "Doctorate degree",
      TRUE ~ edu_level),
    household_income = case_when(
      str_detect(household_income, "Menos de") ~ "Under $15,000", 
      str_detect(household_income, "15,000 y") ~ "Between $15,000 and $29,999", 
      str_detect(household_income, "30,000 y") ~ "Between $30,000 and $49,999", 
      str_detect(household_income, "50,000 y") ~ "Between $50,000 and $74,999", 
      str_detect(household_income, "75,000 y") ~ "Between $75,000 and $99,999", 
      str_detect(household_income, "100,000 y") ~ "Between $100,000 and $150,000", 
      str_detect(household_income, "150,000 o") ~ "Over $150,000",
      TRUE ~ household_income)) %>%
  # Translate Spanish Question Responses to English
  mutate(response = case_when(
    # Race
    str_detect(response, "blanco") ~ "White",
    str_detect(response, "hispano") ~ "Hispanic, Latino, or Spanish origin",
    str_detect(response, "negro") ~ "Black or African American",
    str_detect(response, "asiático") ~ "Asian",
    str_detect(response, "nativo") ~ "American Indian or Alaska Native",
    str_detect(response, "del Medio Oriente") ~ "Middle Eastern or North African",
    str_detect(response, "nativo/a de Hawái") ~ "Native Hawaiian or Other Pacific Islander",
    str_detect(response, "de 2 o Más") ~ "2 or More Race/Ethnicities",
    str_detect(response, "Prefiere no contestar") ~ "Prefer not to say",
    str_detect(response, "Otro") ~ "Other",
    # Gender
    str_detect(response, "Masculino") ~ "Male",
    str_detect(response, "Femenina") ~ "Female",
    str_detect(response, "Masculino Transgénero") ~ "Transgender Male",
    str_detect(response, "Femenina Transgénero") ~ "Transgender Female",
    str_detect(response, "No se conforma a una base binaria de genero") ~ "Non-Binary/Gender Nonconforming",
    str_detect(response, "Prefiere no decir") ~ "Prefer not to say",
    str_detect(response, "Otro") ~ "Other",
    TRUE ~ response))

# Translate Spanish non-demographic question responses to English
ind_responses_translated <- ind_demos_translated %>%
  mutate(response = case_when(
    # Q1, 4, 5, 10, 11, 16
    response == "Sí" ~ "Yes",
    response == "No" ~ "No",
    str_detect(response, "No Está Seguro") ~ "Unsure",
    # Q2: Which of the following categories best describes your employment status?
    # There were more response options in the Spanish version of the Survey than the English Version, matched as best as possible
    str_detect(response, "Tiempo completo") ~ "Employed, working full-time",
    str_detect(response, "Jubilado") ~ "Retired",
    str_detect(response, "Tiempo parcial") ~ "Employed, working part-time",
    str_detect(response, "De temporada") ~ "Seasonal / seasonal",
    str_detect(response, "Desempleado") ~ "Unemployed",
    str_detect(response, "trabaja a tiempo completo") ~ "Employed, working full-time",
    str_detect(response, "trabaja a tiempo parcial") ~ "Employed, working part-time",
    str_detect(response, "No empleado/a, buscando trabajo") ~ "Not employed, looking for work",
    str_detect(response, "No empleado/a, NO está buscando trabajo") ~ "Not employed, NOT looking for work",
    str_detect(response, "Discapacitado") ~ "Disabled, not able to work",
    # Q3: If you are unemployed, please select the reason(s) why below
    str_detect(response, "Antecedentes Penales") ~ "Criminal Background",
    str_detect(response, "Víctima de violencia doméstica") ~ "Domestic violence/sexual assault victim",
    str_detect(response, "Problema de drogas") ~ "Drug/alcohol problem",
    str_detect(response, "Falta de acceso a cuidado infantil") ~ "Lack childcare",
    str_detect(response, "Falta de un domicilio permanente") ~ "Lack permanent address",
    str_detect(response, "Falta de ropa apropiada") ~ "Lack proper clothing",
    str_detect(response, "Falta de capacitación") ~ "Lack skills/education",
    str_detect(response, "Falta de transporte") ~ "Lack transportation",
    str_detect(response, "Falta de documentación de los EEUU") ~ "Lack US documents",
    str_detect(response, "Barrera de idioma") ~ "Language barrier",
    str_detect(response, "no relacionado a COVID-19") ~ "Layoff or Downsizing (non-COVID 19 related)",
    str_detect(response, "Despido por falta de trabajo o reducción de personal debido a COVID -19") ~ "Layoff or Downsizing due to COVID-19",
    str_detect(response, "Discapacidad en el aprendizaje") ~ "Learning/developmental disability",
    str_detect(response, "Problema de salud mental") ~ "Mental health problem",
    str_detect(response, "Otros problemas de salud") ~ "Other health issues",
    str_detect(response, "Discapacidad física permanente") ~ "Permanent physical disability",
    str_detect(response, "Orientación sexual o identidad de sexo") ~ "Sexual orientation or gender identity",
    str_detect(response, "Discapacidad física temporal") ~ "Temporary physical disability",
    str_detect(response, "Menor no acompañado") ~ "Unaccompanied youth",
    str_detect(response, "Otro") ~ "Other",
    #Q6: What 3 issues are most important to you and your family?
    str_detect(response, "de verano, extracurricular, enriquecimiento educativo") ~ "Education (e.g. summer, after school, and enrichment)",
    str_detect(response, "Educación para Adultos") ~ "Adult Education (e.g. associates degree, college, retraining)",
    str_detect(response, "Seguridad de jóvenes") ~ "Youth safety (e.g. youth/young adult support and mentorship, gang alternatives, gang intervention)",
    str_detect(response, "asistencia médica básica, clínicas de salud") ~ "Health (e.g. basic health care, health clinics)",
    str_detect(response, "Salud Mental") ~ "Mental Health (e.g. counseling, therapy)",
    str_detect(response, "Asistencia para vivienda") ~ "Housing assistance (e.g. affordable housing, home purchase assistance, rent/mortgage assistance)",
    str_detect(response, "Asistencia de empleo") ~ "Employment assistance (e.g. workforce development, job placement)",
    str_detect(response, "Asistencia financiera") ~ "Financial assistance (e.g. banking, financial planning)",
    str_detect(response, "Asistencia de negocios") ~ "Business assistance (e.g. startup assistance, startup financial capital, business development)",
    str_detect(response, "Defensa") ~ "Advocacy (e.g. legal support, legal aid, immigration assistance, police justice)",
    str_detect(response, "Justicia Criminal") ~ "Criminal justice",
    # Q7: How EASY or DIFFICULT is it for you to access community supports or resources for each of the following needs?
    # Q9: How easy is it for you to contact an elected official or community leader about an issue affecting you or your community?
    # Q13:  How difficult is it for you to vote?
    response == "Muy Difícil" ~ "Very Difficult",
    response == "Difícil" ~ "Difficult",
    response == "Algo Difícil" ~ "Somewhat Difficult",
    response == "Algo Fácil" ~ "Somewhat Easy",
    response == "Fácil" ~ "Easy",
    response == "Muy Fácil" ~ " Very Easy",
    # Q8: How EFFECTIVE or INEFFECTIVE are community supports or resources for the following needs?
    response == "Muy Inefectivos" ~ "Very Ineffective",
    response == "Inefectivos" ~ "Ineffective",
    response == "Algo Inefectivos" ~ "Somewhat Ineffective",
    response == "Algo Efectivos" ~ "Somewhat Effective",
    response == "Efectivos" ~ "Effective",
    response == "Muy Efectivos" ~ "Very Effective",
    response == "No Se Aplica" ~ "Does Not Apply",
    # Q12: How important is voting to you?
    response == "Nada Importante" ~ "Very Unimportant",
    response == "No es importante" ~ "Unimportant",
    response == "Poco importante" ~ "Somewhat Unimportant",
    response == "Moderadamente importante" ~ "Somewhat Important",
    response == "Importante" ~ "Important",
    response == "Muy Importante" ~ "Very Important",
    # Q14: What makes voting difficult for you?
    str_detect(response, "Requisitos de Identificación de Votante") ~ "Voter ID requirements",
    str_detect(response, "Falta de acceso al idioma") ~ "Lack of language access",
    str_detect(response, "Eliminación de lista de votantes") ~ "Voter roll purges",
    str_detect(response, "consolidación de centros electorales") ~ "Polling place closures/consolidations",
    str_detect(response, "Reducción de votación temprana") ~ "Reduced early voting",
    str_detect(response, "Reducción de horas de votación") ~ "Reduced voting hours",
    str_detect(response, "No puede tomar tiempo libre de su trabajo para votar") ~ "Can't get off work to vote",
    # Q15: How likely are you to vote in the upcoming November election?
    response == "Nada probable" ~ "Not at All Likely",
    response == "Poco probable" ~ "Unlikely",
    response == "Algo improbable" ~ "Somewhat Unlikely",
    response == "Algo probable" ~ "Somewhat Likely",
    response == "Probable" ~ "Likely",
    response == "Muy probable" ~ "Very Likely",
    # Q17: In the past 12 months, which of the following issue areas has a community organization engaged you in?
    response == "Derechos de votación" ~ "Voting rights",
    response == "Censo" ~ "Census",
    response == "Justicia Social" ~ "Social Justice",
    response == "Justicia Criminal" ~ "Criminal Justice",
    response == "Derechos de Empleo" ~ "Employment Rights",
    response == "Derechos de Inmigración" ~ "Immigration Rights",
    response == "Derechos para personas LGBTQ+" ~ "LGBTQ+ Rights",
    response == "Asistencia médica" ~ "Healthcare",
    response == "Vivienda Asequible" ~ "Affordable Housing",
    response == "Derechos para las Mujeres" ~ "Women's Rights",
    # Q18:  In the past 12 months, which of the following activities have you done?
    response == "Ha votado" ~ "Voted",
    response == "Ha completado el Censo de 2020" ~ "Completed the 2020 Census",
    response == "Ha participado en protestas" ~ "Protested",
    response == "Ha Escrito o llamado a un funcionario del gobierno" ~ "Wrote or Called a Government Official",
    response == "Se ha postulado para algún cargo" ~ "Ran for Office",
    response == "Firmó una Petición" ~ "Signed a Petition",
    TRUE ~ response))

# Transform Individual, Community Surveys for Easier Analysis/Summarization
ind_clean <- ind_responses_translated %>%
  # Clean up question names where the question is the same as the answer (multi-select questions)
  mutate(question = case_when(
    str_detect(main_question, "If you are unemployed") ~ "Reason for unemployment",
    str_detect(main_question, "What 3 issues are most important to you and your family") ~ "Top 3 Important Issues",
    str_detect(main_question, "What makes voting difficult for you") ~ "Barriers to voting",
    str_detect(main_question, "which of the following issue areas has a community organization") ~ "Issue areas engaged in by community organizations",
    str_detect(main_question, "which of the following activities have you done") ~ "Community Advocacy",
    TRUE ~ question))

# Remove race and gender identity from question/responses
# Create Race DF
race_df <- ind_clean %>%
  select(respondent_id, question, response) %>%
  filter(question == "race") %>%
  pivot_wider(id_cols = respondent_id, names_from = question, values_from = response) %>%
  mutate(race = as.character(race)) %>%
  # Replace instances where multiple races were selected with 2 or More Race/Ethnicities
  mutate(race = case_when(
    str_detect(race, 'c\\(\\"') ~ "2 or More Race/Ethnicities",
    TRUE ~ race))

# Create Gender DF
gender_df <- ind_clean %>%
  select(respondent_id, question, response) %>%
  filter(question == "gender_identity") %>%
  pivot_wider(id_cols = respondent_id, names_from = question, values_from = response) %>%
  mutate(gender_identity = as.character(gender_identity)) %>%
  # Replace instances where Male & Female was selected for gender identity and replace with "Prefer not to say"
  mutate(gender_identity = case_when(
    (str_detect(gender_identity, "Male") & str_detect(gender_identity, "Female")) ~ "Prefer not to say",
    TRUE ~ gender_identity))

# Join the race and gender data frames back into the ind_clean dataframe, removing the original race and gender identity questions
ind_clean_final <- ind_clean %>%
  filter(main_question != "race",
         main_question != "gender_identity") %>%
  left_join(gender_df, by = "respondent_id") %>%
  left_join(race_df, by = "respondent_id") %>%
  # Rearrange columns
  select(respondent_id, language, race, gender_identity, age:response)

# Remove dataframes no longer needed
# rm(individual_survey_eng_unique, individual_survey_spa_unique, ind_spa_translated, ind_survey_all_responses, ind_responses_translated,
#    gender_df, race_df, ind_demos_translated)

## Clean Organizational Data----
comm_org_clean <- partner_org_survey %>%
  select(4, 9:116) %>%
  gather(c(3:109), key = "question", value = "response") %>%
  separate(question, c("main_question", "question"), sep = " - ") %>%
  clean_names() %>%
  rename(org_type = how_would_you_classify_your_organization) %>%
  # Clean-up multi-select question's
  mutate(question = case_when(
    str_detect(main_question, "What communities does your organization seek to serve") ~ "Target Communities",
    str_detect(main_question, "What cities does your organization serve") ~ "Cities Served",
    str_detect(main_question, "What are your organization's primary focus areas") ~ "Focal Areas",
    str_detect(main_question, "What is your mission") ~ "Org Mission",
    str_detect(main_question, "What are the main challenges to executing on your mission") ~ "Barriers to Executing on Mission",
    str_detect(main_question, "What is you vision") ~ "Org Vision",
    str_detect(main_question, "What are your values") ~ "Org Values",
    str_detect(main_question, "in which ways does your HR/Talent Management practices promote greater") ~ "HR DEI Strategies",
    str_detect(main_question, "How do you determine the kind of support and programs you ") ~ main_question,
    question == "practices/policies" ~ "Practices/Policies",
    is.na(question) ~ main_question,
    TRUE ~ question),
    main_question =  case_when(
      str_detect(main_question, "What is you vision") ~ "What is your vision?",
      str_detect(main_question, " racial demographics of your staff") ~ "Racial Representation of Staff",
      str_detect(main_question, "communities you serve") ~ "Racial Representation of Communities Served",
      str_detect(main_question, "the racial demographics of the clients you serve") ~ "Racial Representation of Clients",
      TRUE ~ main_question)) %>%
  ## Clean up responses to representation questions
  # Remove responses that don't fit the format
  mutate(response = case_when(
    (respondent_id == "11961825729" & main_question == "Racial Representation of Communities Served") ~ NA_character_,
    TRUE ~ response)) %>%
  # Remove punctuations and strings from representation questions to only get to numbers
  mutate(response = case_when(
    str_detect(main_question, "Racial Representation") ~ str_remove_all(response, "[%;a-zA-Z]"),
    TRUE ~ response),
  response = str_trim(response),
  org_type = as.character(org_type))

# SAVE DATA----------------------------------------------------------

# Save as an .rda file for cleaning
save("comm_org_clean", "ind_clean_final",  file = here::here("Data/Clean/chic_landscape_analysis_cleaned_data.rda"))

# Save as an excel file for sharing raw data back with CHIC
write_xlsx(ind_clean_final, here::here("Outputs/Data/individual_responses_cleaned.xlsx"))
write_xlsx(comm_org_clean, here::here("Outputs/Data/partner_org_responses_cleaned.xlsx"))
