# Code to turn FBI personel data into a question, agency, year dataframe for the quick facts app.

# -----------------------------------------------------------------------
# Author: Elijah Appelson
# Update Date: January 27th, 2025
# -----------------------------------------------------------------------

# Loading Libraries
library(lubridate)
library(janitor)
library(zoo)
library(tidyverse)

# Defining the data date
newest_date <- "2026-08-18"

# Defining Links
pd_sizes_link = paste0("data_creation/data/overview_data/", newest_date,"/lee_1960_2025.csv")
agency_locations_link = "data_creation/data/misconduct_data/data_agency-reference-list.csv"
pd_references_link = "data_creation/data/overview_data/35158-0001-Data.rda"
overview_cheat = "data_creation/data/overview_data/cheat_sheet_overview.csv"

# Reading in data
pd_sizes <- read_csv(here::here(pd_sizes_link))
agency_locations <- read_csv(here::here(agency_locations_link))
overview_cheat <- read_csv(here::here(overview_cheat))

# Loading in data
load(here::here(pd_references_link))

pd_references <- da35158.0001

# ------------------------------------- Cleaning Data Process ----------------------------------------------

# Renaming variables in the pd references
pd_references <- pd_references %>%
  select(ORI9, NAME) %>%
  rename(ori = ORI9,
         agency_full_name = NAME)


# Defining the type of agency
agency_locations <- agency_locations %>%
  filter(!(agency_slug %in% c("de-soto-so", "new-orleans-so"))) %>%
  mutate(
    agency_type = case_when(
      str_detect(tolower(agency_name), "university|college|campus") ~ "University or Campus Police",
      str_detect(tolower(agency_name), "marshal") ~ "Marshal's Office",
      str_detect(tolower(agency_name), "constable") ~ "Constable's Office",
      str_detect(tolower(agency_name), "sheriff") ~ "Sheriff's Office",
      str_detect(tolower(agency_name), "department|police department") ~ "Police Department",
      TRUE ~ "Other Law Enforcement Agency"
    ))

# Connecting department references with police department sizes
la_pd_sizes <- pd_sizes %>% 
  filter(state_abbr == "LA") %>%
  left_join(pd_references, by = "ori") %>%
  mutate(agency_name = str_trim(str_to_title(agency_full_name)),
         agency_name = ifelse(is.na(agency_name), pub_agency_name, agency_name),
         agency_name = str_replace(agency_name, "Dept|Dept.|Pd", "Police Department"),
         agency_name = str_remove(agency_name, "\\.$"))



# Defining overview_better
overview_better <- la_pd_sizes %>%
  left_join(overview_cheat, by = "agency_name")

# Creating a total dataframe of personel
df <- overview_better %>%
  select(correct_agency_name,
         year = data_year,
         population,
         `male_officer_ct`:`pe_ct_per_1000`) %>%
  rename(
    male_civilian_ct = male_cilvilian_ct,
    female_civilian_ct = female_cilvilian_ct,
  ) %>%
  mutate(
    male_total_ct_per_huntho = 10^5*male_total_ct/population,
    female_total_ct_per_huntho = 10^5*female_total_ct/population,
    officer_ct_per_huntho = 10^5*officer_ct/population,
    civilian_ct_per_huntho = 10^5*civilian_ct/population,
    pe_ct_per_1000 = 100*as.numeric(pe_ct_per_1000)) %>%
  mutate_all(~ replace(., is.infinite(.), NA)) %>%
  mutate_all(as.character) %>%
  pivot_longer(cols = "population":"pe_ct_per_1000") %>%
  select(correct_agency_name, year, rowname = name, value) %>%
  group_by(correct_agency_name) %>%
  
  # Turning data into usable text
  mutate(
    text = case_when( 
      rowname == "population" ~ paste0(correct_agency_name, " reportedly policed ", value, " people in ", year,"."),
      rowname == "male_officer_ct" ~ paste0(correct_agency_name, " reportedly employed ", value, " male officers in ", year,"."),
      rowname == "male_civilian_ct" ~ paste0(correct_agency_name, " reportedly employed ", value, " male civilian employees in ", year,"."),
      rowname == "male_total_ct" ~ paste0(correct_agency_name, " reportedly employed ", value, " male officer and civilian employees in ", year,"."),
      rowname == "female_officer_ct" ~ paste0(correct_agency_name, " reportedly employed ", value, " female officers in ", year,"."),
      rowname == "female_civilian_ct" ~ paste0(correct_agency_name, " reportedly employed ", value, " female civilian employees in ", year,"."),
      rowname == "female_total_ct" ~ paste0(correct_agency_name, " reportedly employed ", value, " female officer and civilian employees in ", year,"."),
      rowname == "officer_ct" ~ paste0(correct_agency_name, " reportedly employed ", value, " officers in ", year,"."),
      rowname == "civilian_ct" ~ paste0(correct_agency_name, " reportedly employed ", value, " civilian employees in ", year,"."),
      rowname == "total_pe_ct" ~ paste0(correct_agency_name, " reportedly employed ", value, " officer and civilian employees in ", year,"."),
      rowname == "pe_ct_per_1000" ~ paste0(correct_agency_name, " reportedly employed ", value, " officer and civilian employees per hundred thousand residents policed in ", year,"."),
      rowname == "male_officer_ct_per_huntho" ~ paste0(correct_agency_name, " reportedly employed ", value, " male officers per hundred thousand residents policed in ", year,"."),
      rowname == "male_civilian_ct_per_huntho" ~ paste0(correct_agency_name, " reportedly employed ", value, " male civilian employees per hundred thousand residents policed in ", year,"."),
      rowname == "male_total_ct_per_huntho" ~ paste0(correct_agency_name, " reportedly employed ", value, " male officer and civilian employees per hundred thousand residents policed in ", year,"."),
      rowname == "female_officer_ct_per_huntho" ~ paste0(correct_agency_name, " reportedly employed ", value, " female officers per hundred thousand residents policed in ", year,"."),
      rowname == "female_civilian_ct_per_huntho" ~ paste0(correct_agency_name, " reportedly employed ", value, " female civilian employees per hundred thousand residents policed in ", year,"."),
      rowname == "female_total_ct_per_huntho" ~ paste0(correct_agency_name, " reportedly employed ", value, " female officer and civilian employees per hundred thousand residents policed in ", year,"."),
      rowname == "officer_ct_per_huntho" ~ paste0(correct_agency_name, " reportedly employed ", value, " total officers per hundred thousand residents policed in ", year,"."),
      rowname == "civilian_ct_per_huntho" ~ paste0(correct_agency_name, " reportedly employed", value, " total civilian employees per hundred thousand residents policed in ", year,".")
    )
  )

# Creating the final police personel dataframe
police_personnel_df <- df %>%
  select(correct_agency_name, year, variable = rowname, value, text) %>%
  group_by(year, variable) %>%
  mutate(num_value = as.numeric(value)) %>%
  mutate(value_percentile = round(ifelse(is.na(num_value), NA, ecdf(num_value)(num_value) * 100),2)) %>%
  mutate(
    question = case_when(
      variable == "population" ~ "How many people do they police?",
      variable == "male_officer_ct" ~ "How many officers are male?",
      variable == "male_civilian_ct" ~ "How many civilian employees are male?",
      variable == "male_total_ct" ~ "How many officer and civilian employees are male?",
      variable == "female_officer_ct" ~ "How many officers are female?",
      variable == "female_civilian_ct" ~ "How many civilian employees are female?",
      variable == "female_total_ct" ~ "How many officer and civilian employees are female?",
      variable == "officer_ct" ~ "How many total officers are there?",
      variable == "civilian_ct" ~ "How many total civilian employees are there?",
      variable == "total_pe_ct" ~ "How many officer and civilian employees are there?",
      variable == "pe_ct_per_1000" ~ "How many employees are there per hundred thousand people policed?",
      variable == "male_officer_ct_per_huntho" ~ "How many male officers are there per hundred thousand people policed?",
      variable == "male_civilian_ct_per_huntho" ~ "How many male civilian employees are there per hundred thousand people policed?",
      variable == "male_total_ct_per_huntho" ~ "How many male officer and civilian employees are there per hundred thousand people policed?",
      variable == "female_officer_ct_per_huntho" ~ "How many female officers are there per hundred thousand people policed?",
      variable == "female_civilian_ct_per_huntho" ~ "How many female civilian employees are there per hundred thousand people policed?",
      variable == "female_total_ct_per_huntho" ~ "How many female officer and civilian employees are there per hundred thousand people policed?",
      variable == "officer_ct_per_huntho" ~ "How many officers are there per hundred thousand people policed?",
      variable == "civilian_ct_per_huntho" ~ "How many civilian employees are there per hundred thousand people policed?"
    ),
    question_p1 = case_when(
      variable == "population" ~ "How many",
      variable == "male_officer_ct" ~ "How many",
      variable == "male_civilian_ct" ~ "How many",
      variable == "male_total_ct" ~ "How many",
      variable == "female_officer_ct" ~ "How many",
      variable == "female_civilian_ct" ~ "How many",
      variable == "female_total_ct" ~ "How many",
      variable == "officer_ct" ~ "How many",
      variable == "civilian_ct" ~ "How many",
      variable == "total_pe_ct" ~ "How many",
      variable == "pe_ct_per_1000" ~ "How many",
      variable == "male_total_ct_per_huntho" ~ "How many",
      variable == "female_total_ct_per_huntho" ~ "How many",
      variable == "officer_ct_per_huntho" ~ "How many",
      variable == "civilian_ct_per_huntho" ~ "How many"
    ),
    question_p1 = case_when(
      variable == "population" ~ "How many people were policed by",
      variable == "male_officer_ct" ~ "How many male officers were employed by",
      variable == "male_civilian_ct" ~ "How many male civilians were employed by",
      variable == "male_total_ct" ~ "How many male officer and civilian employees were employed by",
      variable == "female_officer_ct" ~ "How many female officers were employed by",
      variable == "female_civilian_ct" ~ "How many female civilians were employed by",
      variable == "female_total_ct" ~ "How many female officer and civilian employees were employed by",
      variable == "officer_ct" ~ "How many total officers were employed by",
      variable == "civilian_ct" ~ "How many total civilians were employed by",
      variable == "total_pe_ct" ~ "How many total officer and civilian employees were employed by",
      variable == "pe_ct_per_1000" ~ "How many total officers and civilians per hundred thousand people policed were employed by"
    ),
    years_reported = year,
    question_p2 = correct_agency_name,
    question_p3 = years_reported,
    question_complex = paste0(question_p1, " ", question_p2, " in ", question_p3, "?"),
    category = "Personnel",
    source = "FBI Crime Explorer Law Enforcement Employees data",
    link = "https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/le/pe"
  )
