# Code to turn Mapping Police Violence data into a question, agency, year dataframe for the quick facts app.

# -----------------------------------------------------------------------
# Author: Elijah Appelson
# Update Date: August 18th, 2026
# -----------------------------------------------------------------------

# Loading Libraries
library(tidyverse)
library(janitor)

# Defining the data date
newest_date <- "2026-08-18"

# Importing Data
killing_cheat <- read_csv("data_creation/data/killing_data/cheat_sheet_killing.csv")
killing_data <- read_csv(paste0("data_creation/data/killing_data/", newest_date ,"/Mapping Police Violence.csv"))

# Defining a killing dataframe
killing <- killing_data %>%
  
  # Cleaning names
  clean_names() %>%
  
  # Filtering for Louisiana killings
  filter(state == "Louisiana") %>%
  
  # Separating Parish from Parish names
  separate_wider_delim(county, delim = " Parish", names = c("parish", "extra"), too_few = "align_start") %>%
  
  # Removing "extra" column
  select(-extra) %>%
  
  # Fixing demographic variables
  mutate(race = ifelse(is.na(race), "Unknown Race", race),
         race = ifelse(race == "Unknown race", "Unknown Race", race),
         gender = ifelse(is.na(gender), "Unknown Gender", gender),
         age = ifelse(is.na(age), "Unknown Age", age),
         
         # Fixing and defining year and month variables
         date = mdy(date),
         year = year(date),
         year_month = month(date),
         
         # Creating an age category
         age_category = case_when(
           age < 18 ~ "<18",
           age >= 18 & age < 35 ~ "18 - 34",
           age >= 35 & age < 55 ~ "35 - 54",
           age >= 55 ~ "55+",
           TRUE ~ NA),
         parish = ifelse(parish == "Dallas", "Rapides", parish),
         parish = str_replace(parish, "Saint", "St."),
         parish = ifelse(parish == "Acadiana", "Acadia", parish),
         parish = ifelse(city == "Monroe", "St. Tammany", parish)) %>%
  filter(!(name == "Omarr Jackson" & race == "White")) %>%
  mutate(
    
    # Fixing the agencies spelling
    agency_responsible = case_when(
      str_detect(agency_responsible,"(US Marshals Service)") ~ "U.S. Marshals Service, Alexandria Police Department, Rapides Parish Sheriff's Office",
      TRUE ~ agency_responsible
    ),
    
    # Replacing unwanted spaces and removing special characters
    agency_name = str_split(as.character(str_replace_all(agency_responsible, ", ", ",")), ","),
    agency_name = map(agency_name, ~ str_remove_all(.x, '^"|"$|[()]')),
    id = row_number()
  ) %>%
  
  # Unnesting the agencies in lists
  unnest(agency_name) %>%
  
  # Fixing more agency names
  mutate(
    # Trim stray whitespace and normalize curly apostrophes to straight ones first,
    # so downstream string matching (case_when, cheatsheet join) is consistent
    agency_name = str_trim(agency_name),
    agency_name = str_replace_all(agency_name, "[\u2018\u2019]", "'"),
    
    agency_name = case_when(
      agency_name == "Caddo County Sheriff's Office" ~ "Caddo Parish Sheriff's Office",
      agency_name == "Calcasieu Parish Sheriff's Office" ~ "Calcasieu Parish Sheriff's Office",
      agency_name == "East Baton Rouge Sheriff's Office" ~ "East Baton Rouge Parish Sheriff's Office",
      agency_name == "Hourma Police Department" ~ "Houma Police Department",
      agency_name == "Arcadia Parish Sheriff's Office" ~ "Acadia Parish Sheriff's Office",
      agency_name == "Jefferson Parish Police Department" ~ "Jefferson Parish Sheriff's Office",
      str_detect(agency_name, regex("^St\\.? John( the| The) Baptist Parish Sheriff's Office$", ignore_case = TRUE)) ~ "St. John Parish Sheriff's Office",
      agency_name %in% c("Tangipahoa Parish Sheriff's Office", "Tangipahoa Sheriff's Department") ~ "Tangipahoa Parish Sheriff's Office",
      agency_name == "Terrebonne Parish Sheriff's Department" ~ "Terrebonne Parish Sheriff's Office",
      agency_name == "U.S. Bureau of Investigation" ~ "U.S. Federal Bureau of Investigation",
      agency_name == "US Marshals" ~ "U.S. Marshals Service",
      agency_name == "Deridder Police Department" ~ "De Ridder Police Department",
      agency_name == "Desoto Parish Sheriff's Office" ~ "De Soto Parish Sheriff's Office",
      TRUE ~ agency_name
    ),
    
    # Normalize "St" -> "St." consistently across agency names
    agency_name = str_replace(agency_name, "^St(?!\\.)\\s", "St. "),
    
    # Replacing sheriff names
    agency_name = ifelse(str_detect(agency_name, "Parish"),
                         agency_name,
                         gsub("Sheriff's Office", 
                              "Parish Sheriff's Office", 
                              agency_name))
  ) %>%
  
  # Joining by a handwritten cheatsheet of agency names
  left_join(killing_cheat, by = "agency_name") %>%
  
  # If not in killing_cheat, use the agency_name already there
  mutate(correct_agency_name = coalesce(correct_agency_name, agency_name)) %>%
  
  group_by(id) %>%
  summarize(across(-c(agency_name, correct_agency_name), ~first(.)), 
            agency_name = paste(agency_name, collapse = ", "),
            correct_agency_name = paste(correct_agency_name, collapse = ", ")
  )

setdiff(unique(killing$agency_name), killing_cheat$agency_name)

# Defining the appropriate columns
killing_func <- function(df){
  df %>%
    summarize(n_killing = n(),
              
              # Making names a list
              names_people_killed = case_when(
                n() == 1 ~ name[1],
                n() == 2 ~ paste(name, collapse = " and "),
                TRUE ~ paste(paste(head(name, -1), collapse = ", "), tail(name, 1), sep = ", and ")
              ),
              
              # Defining the number of people killed by race
              w_killed = sum(race == "White", na.rm = TRUE),
              b_killed = sum(race == "Black", na.rm = TRUE),
              h_killed = sum(race == "Hispanic", na.rm = TRUE),
              a_killed = sum(race == "Asian", na.rm = TRUE),
              u_killed = sum(race == "Unknown", na.rm = TRUE),
              m_killed = sum(gender == "Male", na.rm = TRUE),
              f_killed = sum(gender == "Female", na.rm = TRUE),
              
              # Defining the number of people killed by age 
              age_18_killed = sum(age_category == "<18", na.rm = TRUE),
              age_18_34_killed = sum(age_category == "18 - 34", na.rm = TRUE),
              age_35_54_killed = sum(age_category == "35 - 54", na.rm = TRUE),
              age_55_killed = sum(age_category == "55+", na.rm = TRUE),
              ave_age_killed = round(mean(as.integer(age), na.rm = TRUE),2),
              
              # Defining the number of people killed by mental health status
              mental_illness_yes = sum(signs_of_mental_illness == "Yes", na.rm = TRUE),
              mental_illness_no = sum(signs_of_mental_illness == "No", na.rm = TRUE),
              mental_illness_unknown = sum(signs_of_mental_illness == "Unknown", na.rm = TRUE),
              mental_illness_drugs = sum(signs_of_mental_illness == "Drug or Alcohol Use", na.rm = TRUE),
              
              # Defining the number of people killed by circumstances
              not_fleeing = sum(if_else((is.na(flee) | flee == "Not Fleeing"), 1, 0), na.rm = TRUE),
              fleeing = sum(if_else(!(is.na(flee) | flee == "Not Fleeing"), 1, 0), na.rm = TRUE),
              violent_crime = sum(ifelse(encounter_type == "Part 1 Violent Crime", 1, 0), na.rm = TRUE),
              non_violent_crime = sum(ifelse(encounter_type != "Part 1 Violent Crime", 1, 0), na.rm = TRUE),
              allegedly_armed = sum(ifelse(str_detect(allegedly_armed_with, "Gun|Edged weapon|Other|Unknown weapon"),1,0), na.rm = TRUE),
              
              # Defining the the circumstances
              disposition_pending = sum(ifelse(str_detect(tolower(disposition_official), "pending investigation"),1,0), na.rm = TRUE),
              disposition_charge = sum(ifelse(str_detect(tolower(disposition_official), "charged|convicted|acquitted|charges dropped|dismissed criminal charges"),1,0), na.rm = TRUE),
              disposition_cleared_justified = sum(ifelse(str_detect(tolower(disposition_official), "cleared|justified"),1,0), na.rm = TRUE)
    ) %>%
    mutate_all(~ replace(., is.na(.), "Unknown"))
}

# Fitting the column creation year by year
year_by_year_killing <- killing %>%
  mutate(correct_agency_name = str_split(correct_agency_name,", ")) %>%
  unnest(correct_agency_name) %>%
  group_by(correct_agency_name, year) %>%
  killing_func() %>%
  mutate(year = as.character(year),
         years_report = year) %>%
  distinct(,.keep_all = TRUE)

# Fitting the column creation on all years
all_year_killing <- killing %>%
  mutate(correct_agency_name = str_split(correct_agency_name,", ")) %>%
  unnest(correct_agency_name) %>%
  group_by(correct_agency_name) %>%
  killing_func() %>%
  mutate(year = "2013-2026") %>%
  mutate(year = as.character(year),
         years_report = "Total Years Collecting Police Killings") %>%
  distinct(,.keep_all = TRUE)

# Concatenating year by year and all killings 
df <- rbind(year_by_year_killing,all_year_killing) %>%
  pivot_longer(cols = "n_killing":"disposition_cleared_justified") %>%
  select(correct_agency_name, year, years_report, rowname = name, value) %>%
  group_by(correct_agency_name) %>%
  
  # Creating the appropriate text with the numbers
  mutate(
    text = case_when(
      rowname == "n_killing" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " person in ", " people in "), year,"."),
      rowname == "names_people_killed" ~ paste0("The names of people killed by ", correct_agency_name, " in ", year, " include ", value,"."),
      rowname == "w_killed" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " White person in ", " White people in "), year,"."),
      rowname == "b_killed" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " Black person in ", " Black people in "), year,"."),
      rowname == "a_killed" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " Asian person in ", " Asian people in "), year,"."),
      rowname == "h_killed" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " Hispanic person in ", " Hispanic people in "), year,"."),
      rowname == "u_killed" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " person with an unidentified race ", " people with unidentified race in "), year,"."),
      rowname == "m_killed" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " man in ", " men in "), year,"."),
      rowname == "f_killed" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " woman in ", " women in "), year,"."),
      rowname == "age_18_killed" ~ paste0(correct_agency_name, " killed ", value, " youth under the age of 18 in ", year,"."),
      rowname == "age_18_34_killed" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " person", " people"), " between the age of 18 and 34 in ", year,"."),
      rowname == "age_35_54_killed" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " person", " people"), " between the age of 35 and 54 in ", year,"."),
      rowname == "age_55_killed" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " person", " people"), " above the age of 55 in ", year),
      rowname == "ave_age_killed" ~ paste0("The people killed by ", correct_agency_name, " had an average age of ", value, " in ", year,"."),
      rowname == "mental_illness_yes" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " person", " people"), " experiencing apparent mental illnesses in ", year,"."),
      rowname == "mental_illness_no" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " person", " people"), " not experiencing apparent mental illnesses in ", year,"."),
      rowname == "mental_illness_unknown" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " person", " people"), " for whom it is unclear whether they were experiencing mental illnesses in ", year,"."),
      rowname == "mental_illness_drugs" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " person", " people"), " on drugs or alcohol in ", year,"."),
      rowname == "fleeing" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " person", " people"), " fleeing in ", year,"."),
      rowname == "not_fleeing" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " person", " people"), " not fleeing in ", year,"."),
      rowname == "violent_crime" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " person", " people"), " committing a violent crime in ", year,"."),
      rowname == "non_violent_crime" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " person", " people"), " not committing a violent crime in ", year,"."),
      rowname == "allegedly_armed" ~ paste0(correct_agency_name, " killed ", value, ifelse(value == 1, " person", " people"), " while allegedly armed in ", year,"."),
      rowname == "disposition_pending" ~ paste0(value, ifelse(value == 1, " police killing", " police killings"), " from ", correct_agency_name, " had a pending disposition in ", year,"."),
      rowname == "disposition_cleared_justified" ~ paste0(value, ifelse(value == 1, " police killing", " police killings"), " from ", correct_agency_name, " had a cleared or justified disposition in ", year,"."),
      rowname == "disposition_charge" ~ paste0(value, ifelse(value == 1, " police killing", " police killings"), " from ", correct_agency_name, " resulted in an officer being charged in ", year,"."),
      rowname == "percent_w_killed" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were White","."),
      rowname == "percent_b_killed" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were Black","."),
      rowname == "percent_a_killed" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were Asian","."),
      rowname == "percent_h_killed" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were Hispanic","."),
      rowname == "percent_u_killed" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " had an unidentified race","."),
      rowname == "percent_f_killed" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were female","."),
      rowname == "percent_m_killed" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were male"),
      rowname == "percent_age_18_killed" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were youth under the age of 18"),
      rowname == "percent_age_18_34_killed" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were between the ages of 18 and 34"),
      rowname == "percent_age_35_54_killed" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were between the ages of 35 and 54"),
      rowname == "percent_age_55_killed" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were older than 55 years old"),
      rowname == "percent_mental_illness_yes" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were experiencing symptoms of mental illness"),
      rowname == "percent_mental_illness_no" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were not experiencing symptoms of mental illness"),
      rowname == "percent_mental_illness_unknown" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were experiencing unclear symptoms of mental illness"),
      rowname == "percent_mental_illness_drugs" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were on drugs or alcohol"),
      rowname == "percent_fleeing" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were fleeing"),
      rowname == "percent_not_fleeing" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were not fleeing"),
      rowname == "percent_violent_crime" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " commit a violent crime"),
      rowname == "percent_non_violent_crime" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " did not commit a violent crime"),
      rowname == "percent_allegedly_armed" ~ paste0(round(as.numeric(value),2), "% of people killed by ", correct_agency_name, " in ", year, " were allegedly armed"),
      rowname == "percent_disposition_pending" ~ paste0(round(as.numeric(value),2), "% of police killings by ", correct_agency_name, " in ", year, " have pending dispositions"),
      rowname == "percent_disposition_cleared_justified" ~ paste0(round(as.numeric(value),2), "% of police killings by ", correct_agency_name, " in ", year, " are cleared and justified"),
      rowname == "percent_disposition_charge" ~ paste0(round(as.numeric(value),2), "% of police killings by ", correct_agency_name, " in ", year, " resulted in an officer being charged"),
    )
  )

# Creating the final dataframe
police_killing_df <- df %>%
  select(correct_agency_name, year, years_report, variable = rowname, value, text) %>%
  group_by(year, variable) %>%
  mutate(num_value = as.numeric(value)) %>%
  mutate(value_percentile = round(ifelse(is.na(num_value), NA, ecdf(num_value)(num_value) * 100),2)) %>%
  
  # Adding appropriate questions for each text
  mutate(
    question = case_when(
      variable == "n_killing" ~ "How many people were killed?",
      variable == "names_people_killed" ~ "What are the names of those who were killed?",
      variable == "w_killed" ~ "How many White people were killed?",
      variable == "b_killed" ~ "How many Black people were killed?",
      variable == "a_killed" ~ "How many Asian people were killed?",
      variable == "h_killed" ~ "How many Hispanic people were killed?",
      variable == "u_killed" ~ "How many people with unidentified race were killed?",
      variable == "m_killed" ~ "How many men were killed?",
      variable == "f_killed" ~ "How many women were killed?",
      variable == "age_18_killed" ~ "How many youth under 18 were killed?",
      variable == "age_18_34_killed" ~ "How many people between the ages of 18 and 34 were killed?",
      variable == "age_35_54_killed" ~ "How many people between the ages of 35 and 54 were killed?",
      variable == "age_55_killed" ~ "How many people over the age of 55 were killed?",
      variable == "ave_age_killed" ~ "What is the average age of someone killed?",
      variable == "mental_illness_yes" ~ "How many people were killed while experiencing symptoms of mental illnesses?",
      variable == "mental_illness_no" ~ "How many people were killed while not experiencing symptoms of mental illnesses?",
      variable == "mental_illness_unknown" ~ "How many people were killed while experiencing unclear symptoms of mental illnesses?",
      variable == "mental_illness_drugs" ~ "How many people were killed while on drugs or alcohol?",
      variable == "fleeing" ~ "How many people were killed while fleeing?",
      variable == "not_fleeing" ~ "How many people were killed while not fleeing?",
      variable == "violent_crime" ~ "How many people were killed while committing a violent crime?",
      variable == "non_violent_crime" ~ "How many people were killed while not committing a violent crime?",
      variable == "allegedly_armed" ~ "How many people were killed while allegedly armed?",
      variable == "disposition_pending" ~ "How many police killing dispositions are still pending?",
      variable == "disposition_cleared_justified" ~ "How many police killings are cleared or justified?",
      variable == "disposition_charge" ~ "How many police killings result in an officer being charged?",
      variable == "percent_w_killed" ~ "What percent of people killed were White?",
      variable == "percent_b_killed" ~ "What percent of people killed were Black?",
      variable == "percent_h_killed" ~ "What percent of people killed were Hispanic?",
      variable == "percent_a_killed" ~ "What percent of people killed were Asian?",
      variable == "percent_u_killed" ~ "What percent of people killed had an undefined race?",
      variable == "percent_m_killed" ~ "What percent of people killed were men?",
      variable == "percent_f_killed" ~ "What percent of people killed were women?",
      variable == "percent_age_18_killed" ~ "What percent of people killed were youth under 18?",
      variable == "percent_age_18_34_killed" ~ "What percent of people killed were 18 to 34 years old?",
      variable == "percent_age_35_54_killed" ~ "What percent of people killed were 35 to 54 years old?",
      variable == "percent_age_55_killed" ~ "What percent of people killed were 55 and older?",
      variable == "percent_mental_illness_yes" ~ "What percent of people killed were experiencing symptoms of mental illness?",
      variable == "percent_mental_illness_no" ~ "What percent of people killed were not experiencing symptoms of mental illnesses?",
      variable == "percent_mental_illness_unknown" ~ "What percent of people killed had unclear symptoms of mental illnesses?",
      variable == "percent_mental_illness_drugs" ~ "What percent of people killed were on drugs or alcohol?",
      variable == "percent_fleeing" ~ "What percent of people killed were fleeing?",
      variable == "percent_violent_crime" ~ "What percent of people killed were committing a violent crime?",
      variable == "percent_non_violent_crime" ~ "What percent of people killed were not committing a violent crime?",
      variable == "percent_allegedly_armed" ~ "What percent of people killed were allegedly armed?",
      variable == "percent_disposition_pending" ~ "What percent of police killings had pending dispositions?",
      variable == "percent_disposition_cleared_justified" ~ "What percent of police killings resulted in cleared or justified dispositions?",
      variable == "percent_disposition_charge" ~ "What percent of police killings resulted at least one officer being charged?"
    ),
    
    # Breaking the question down to be used within the app
    question_p1 = case_when(
      variable == "n_killing" ~ "How many people were killed by",
      variable == "names_people_killed" ~ "What are the names of those who were killed by",
      variable == "w_killed" ~ "How many White people were killed by",
      variable == "b_killed" ~ "How many Black people were killed by",
      variable == "a_killed" ~ "How many Asian people were killed by",
      variable == "h_killed" ~ "How many Hispanic people were killed by",
      variable == "u_killed" ~ "How many people with unidentified race were killed by",
      variable == "m_killed" ~ "How many men were killed by",
      variable == "f_killed" ~ "How many women were killed by",
      variable == "age_18_killed" ~ "How many youth under 18 were killed by",
      variable == "age_18_34_killed" ~ "How many people between the ages of 18 and 34 were killed by",
      variable == "age_35_54_killed" ~ "How many people between the ages of 35 and 54 were killed by",
      variable == "age_55_killed" ~ "How many people over the age of 55 were killed by",
      variable == "ave_age_killed" ~ "What is the average age of someone killed by",
      variable == "mental_illness_yes" ~ "How many people were killed while experiencing symptoms of mental illnesses by",
      variable == "mental_illness_no" ~ "How many people were killed while not experiencing symptoms of mental illnesses by",
      variable == "mental_illness_unknown" ~ "How many people were killed while experiencing unclear symptoms of mental illnesses by",
      variable == "mental_illness_drugs" ~ "How many people were killed while on drugs or alcohol by",
      variable == "fleeing" ~ "How many people were killed while fleeing by",
      variable == "not_fleeing" ~ "How many people were killed while not fleeing by",
      variable == "violent_crime" ~ "How many people were killed while committing a violent crime by",
      variable == "non_violent_crime" ~ "How many people were killed while not committing a violent crime by",
      variable == "allegedly_armed" ~ "How many people were killed while allegedly armed by",
      variable == "disposition_pending" ~ "How many police killing dispositions are still pending by",
      variable == "disposition_cleared_justified" ~ "How many police killings are cleared or justified by",
      variable == "disposition_charge" ~ "How many police killings result in an officer being charged by",
    ),
    
    # Defining the other selected variable
    question_p2 = correct_agency_name,
    question_p3 = years_report,
    
    # Adding the questions together into a complex question
    question_complex = paste0(question_p1, " ",question_p2, " in ", question_p3,"?"),
    category = "Killing",
    source = "Mapping Police Violence",
    link = "https://mappingpoliceviolence.org/"
  )
