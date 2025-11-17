# Setup

pacman::p_load(tidyverse, lubridate, ISOweek, surveillance, purrr, janitor,
               glue, gt, grates)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, arrow, parallelly, glue, rjson, janitor, ggplot2, ggtext, stringr,
  readr, tidyr, gt, svDialogs, flextable, officer
)

# Read in date reference file
# File paths loaded in from json file. To change file input/output locations amend this file.
file_paths <-
  rjson::fromJSON(
    file = "/PHI_conf/Respiratory_Surveillance_Viral/ECOSS/Routine Scripts/ECOSS_Activity_Level_Reporting_V1.0/file_paths.json"
  )

Date_Reference <- readRDS(file_paths$Reference_Files$Date_Reference)

#Get date reference columns to link to ILI/ARI data
Date_Reference <- Date_Reference %>%
  select(date, flu_season, ISOyear, ISOweek) %>%
  mutate(year = ISOyear) %>%
  mutate(iso_week = ISOweek) %>%
  mutate(week_date = date) %>%
  select(week_date, flu_season, year, iso_week)

#read in ILI/ARI data
all_data <- readRDS("/PHI_conf/Respiratory_Surveillance_Viral/GP L8/04 Analysis/ILI ARI Evaluation/FF exploration/ILI_and_ARI_raw_for_analysis.rds") %>%
  mutate(Location = "Scotland")

#remove ILI/ARI counts without postcodes or HSCPs (maybe because practice is not in the practice masterlist for some reason)
# - appears to only be very low populated surgeries in NHS Lanarkshire and NHS Grampian
all_data <- all_data %>%
  drop_na(HSCP_Name)

#convert from date/time to date
all_data$week_date <- as.Date(all_data$week_date)

#left join date reference to all_data
all_data <- all_data %>%
  left_join(Date_Reference, by = "week_date")

#Move ILI and ARI labels into one 'consultation' column and have a count column to record their counts
all_ILI_ARI_data <- all_data %>%
  pivot_longer(
    cols = c(ILI, ARI),                # Columns to reshape
    names_to = "consultation_type",    # New column for type
    values_to = "count"                # New column for counts
  ) %>%
  mutate(rate = ifelse(is.na(pop) | pop == 0,
                       0,  # or NA if you prefer
                       (count / pop) * 100000)) #create rate column
all_ILI_ARI_data$count <- as.numeric(all_ILI_ARI_data$count)

rm(all_data)

#Tidy data into dataframe for HB
GP_hb_data_hold <- all_ILI_ARI_data %>%
  select(week_date, flu_season, year, iso_week) %>%
  distinct()
GP_hb_data <- all_ILI_ARI_data %>%
  select(health_board, week_date, consultation_type, count, pop) %>%
  group_by(health_board, week_date, consultation_type) %>%
  summarise(
    count = sum(count, na.rm = TRUE),
    pop = sum(pop, na.rm = TRUE),
    .groups = "drop") %>%
  left_join(GP_hb_data_hold, by = "week_date") %>%
  mutate(rate = ifelse(is.na(pop) | pop == 0,
                       0,  # or NA if you prefer
                       (count / pop) * 100000)) #create rate column


#Tidy data into dataframe for National
GP_nat_data_hold <- all_ILI_ARI_data %>%
  select(week_date, flu_season, year, iso_week, Location) %>%
  distinct()
GP_nat_data <- all_ILI_ARI_data %>%
  select(week_date, consultation_type, count, pop) %>%
  group_by(week_date, consultation_type) %>%
  summarise(
    count = sum(count, na.rm = TRUE),
    pop = sum(pop, na.rm = TRUE),
    .groups = "drop") %>%
  left_join(GP_nat_data_hold, by = "week_date") %>%
  mutate(rate = ifelse(is.na(pop) | pop == 0,
                       0,  # or NA if you prefer
                       (count / pop) * 100000)) #create rate column

#Tidy data into dataframe for HSCP
GP_hscp_data <- all_ILI_ARI_data %>%
  select(HSCP_Name, week_date, consultation_type, count, pop) %>%
  group_by(HSCP_Name, week_date, consultation_type) %>%
  summarise(
    count = sum(count, na.rm = TRUE),
    pop = sum(pop, na.rm = TRUE),
    .groups = "drop") %>%
  left_join(GP_hb_data_hold, by = "week_date") %>%
  mutate(rate = ifelse(is.na(pop) | pop == 0,
                       0,  # or NA if you prefer
                       (count / pop) * 100000)) #create rate column

#Tidy data into dataframe for Postcode
GP_postcode_data <- all_ILI_ARI_data %>%
  select(Postcode, week_date, consultation_type, count, pop) %>%
  group_by(Postcode, week_date, consultation_type) %>%
  summarise(
    count = sum(count, na.rm = TRUE),
    pop = sum(pop, na.rm = TRUE),
    .groups = "drop") %>%
  left_join(GP_hb_data_hold, by = "week_date") %>%
  mutate(rate = ifelse(is.na(pop) | pop == 0,
                       0,  # or NA if you prefer
                       (count / pop) * 100000)) #create rate column


