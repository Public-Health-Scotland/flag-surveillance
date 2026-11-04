#'
#' title: Get latest extract for recalculating thresholds for Respiratory pathogens
#' author: Matthew Hoyle
#' date: 29/06/2026
#'


# Load packages -----------------------------------------------------------

if (!require(pak)) install.packages("pak")
if (!require(threshtools)) pak::pak("matthh10/threshtools")
if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse, janitor, threshtools, glue, grates)


# Load data ------------------------------------------------------------

# Load ECOSS data

source("/PHI_conf/Respiratory_Surveillance_General/Matthew_Hoyle/get_ecoss_data.R")

# Load covid data from dash folder
dash_path <- glue('/conf/C19_Test_and_Protect/Test & Protect - Warehouse/Weekly Data Folders/{floor_date(today(), unit = "week", week_start = 3)}/Data/new')

dash_data <- read_csv(glue('{dash_path}/Respiratory_Pathogens_MEM_Scot.csv'))

dash_age_data <- read_csv(glue('{dash_path}/Respiratory_Pathogens_MEM_Age.csv'))

dash_hb_data <- read_csv(glue('{dash_path}/Respiratory_Pathogens_MEM_HB.csv'))


# New ILI/ARI data
#sirsp_data <- readRDS("/conf/Resp_Surv_Data_Platform/Analysis/gp_ili_ari_evaluation/outputs/GP consult data/Historic data/historic_adjusted_weekly.rds")
# With adjusted historic data
sirsp_data <- readRDS("/conf/Resp_Surv_Data_Platform/Analysis/gp_ili_ari_evaluation/outputs/GP consult data/Historic data/final_adjusted_weekly_ILI_ARI.rds")



# New NHS24 data
nhs24_raw_data <- readRDS("/PHI_conf/Respiratory_Surveillance_Viral/NHS24_weekly/Weekly Analysis/Outputs/Data/tidy_list.RDS")

# Pop estimates

raw_pop <- readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2025.rds")

# National pop
max_year <- raw_pop |>
  group_by(year) |>
  summarise(pop = sum(pop)) |>
  slice_max(year)

scot_pop <- raw_pop |>
  group_by(year) |>
  summarise(pop = sum(pop)) |>
  add_case(year = 2026, pop = max_year$pop)

# Age-banded pop
age_to_band <- function(age) {
  dplyr::case_when(
    age < 1 ~ "<1",
    age >= 1 & age <= 4 ~ "1-4",
    age >= 5 & age <= 14 ~ "5-14",
    age >= 15 & age <= 44 ~ "15-44",
    age >= 45 & age <= 64 ~ "45-64",
    age >= 65 & age <= 74 ~ "65-74",
    TRUE ~ "75+"
  )
}

max_age_year <- raw_pop |>
  mutate(age_band = age_to_band(age)) |>
  group_by(year, age_band) |>
  summarise(pop = sum(pop), .groups = "drop") |>
  filter(year == max(year))

age_pop <- raw_pop |>
  mutate(age_band = age_to_band(age)) |>
  group_by(year, age_band) |>
  summarise(pop = sum(pop)) |>
  bind_rows(max_age_year |>
              mutate(year = 2026))

# HB pop

hb_cipher <- tibble(
  demographic_group = c("Ayrshire & Arran", "Borders", "Dumfries and Galloway", "Fife",
                        "Forth Valley", "GG&C", "Grampian", "Highland", "Lanarkshire",
                        "Lothian", "Orkney", "Shetlands", "Tayside", "Western Isles"),
  hb2019name = c("NHS Ayrshire and Arran", "NHS Borders", "NHS Dumfries and Galloway",
              "NHS Fife", "NHS Forth Valley", "NHS Grampian", "NHS Greater Glasgow and Clyde",
              "NHS Highland", "NHS Lanarkshire","NHS Lothian", "NHS Orkney",
              "NHS Shetland", "NHS Tayside", "NHS Western Isles"),
  health_board = c("AA", "BR", "DG", "FF", "FV", "GC", "GR", "HG", "LN", "LO", "OR", "SH", "TY", "WI")
)

max_hb_year <- raw_pop |>
  group_by(year, hb2019name) |>
  summarise(pop = sum(pop), .groups = "drop") |>
  filter(year == max(year))

hb_pop <- raw_pop |>
  mutate(age_band = age_to_band(age)) |>
  group_by(year, hb2019name) |>
  summarise(pop = sum(pop)) |>
  bind_rows(max_hb_year |>
              mutate(year = 2026)) |>
  left_join(
    hb_cipher,
    join_by(hb2019name)
  )

# scot_pop <- Aggregate_Scot |>
#   clean_names() |>
#   select(year, pop) |>
#   distinct()


# Parameters --------------------------------------------------------------

# Age bands
age_bands <- c("<1", "1-4", "5-14", "15-44", "45-64", "65-74", "75+")


# Tidy for `{memapp}` -----------------------------------------------------

# Tidy up ILI/ARI data
syndromic_data <- sirsp_data$data$MEM$ILI$national |>
  mutate(indicator = "GPILI") |>
  bind_rows(
    sirsp_data$data$MEM$ARI$national |>
      mutate(indicator = "ARI")
  ) |>
  janitor::clean_names() |>
  rename(rate = adjusted_rate_100000,
         week = iso_week) |>
  select(year, week, indicator, rate) |>
  left_join(
    scot_pop,
    join_by(year)
  ) |>
  mutate(count = round_half_up((rate / 100000) * pop, 0))

# Tidy up NHS24 data
nhs24_data <- nhs24_raw_data$scot |>
  rename(year = isoyear,
         week = isoweek,
         rate = data) |>
  left_join(
    scot_pop,
    join_by(year)
  ) |>
  mutate(indicator = "NHS24",
         count = round_half_up((rate / 100000) * pop, 0))

# Tidy up covid data
covid_data <- dash_data |>
  janitor::clean_names() |>
  filter(pathogen == "Covid-19") |>
  mutate(indicator = "COVID-19") |>
  rename(week = iso_week,
         rate = rate_per100000) |>
  left_join(
    scot_pop,
    join_by(year)
  ) |>
  select(year, week, indicator, rate, count, pop)

# Tidy up ecoss data
ecoss_data <- Aggregate_Scot |>
  janitor::clean_names() |>
  rename(week = is_oweek,
         indicator = organism) |>
  select(year, week, indicator, rate, count, pop)

# Bind all indicator data
tidy_data <- syndromic_data |>
  bind_rows(nhs24_data) |>
  bind_rows(covid_data) |>
  bind_rows(ecoss_data) |>
  mutate(report_week = grates::isoweek(year, week),
         season = threshtools::find_flu_season(as_date(report_week))) |>
  arrange(report_week)

## Age data ----------------------------------------------------------------

# Tidy up syndromic data
syndromic_age_data <- sirsp_data$data$MEM$ILI$age |>
  mutate(indicator = "GPILI") |>
  bind_rows(
    sirsp_data$data$MEM$ARI$age |>
      mutate(indicator = "ARI")
  ) |>
  janitor::clean_names() |>
  rename(rate = adjusted_rate_100000,
         week = iso_week,
         age_band = demographic_group) |>

  filter(!is.na(age_band)) |>
  select(year, week, indicator, age_band, rate)  |>
  left_join(
    age_pop,
    join_by(year, age_band)
  ) |>
  mutate(count = round_half_up((rate / 100000) * pop, 0))

# Tidy up NHS24 data
nhs24_age_data <- nhs24_raw_data[names(nhs24_raw_data) %in% c("under1", "1-4", "5-14", "15-44", "45-64", "65-74", "over74")] |>
  bind_rows(.id = "age_band") |>
  mutate(age_band = case_when(age_band == "under1" ~ "<1",
                              age_band == "over74" ~ "75+",
                              TRUE ~ age_band)) |>
  rename(year = isoyear,
         week = isoweek,
         rate = data) |>
  mutate(indicator = "NHS24") |>
  left_join(
    age_pop,
    join_by(year, age_band)
  ) |>
  mutate(count = round_half_up((rate / 100000) * pop, 0))

# Tidy up covid data
covid_age_data <- dash_age_data |>
  janitor::clean_names() |>
  filter(pathogen == "Covid-19",
         age_group != "All ages") |>
  mutate(indicator = "COVID-19",
         age_band = str_remove_all(age_group, "years?|\\s+")) |>
  rename(week = iso_week,
         rate = rate_per100000) |>
  select(year, week, indicator, age_band, rate, count) |>
  left_join(
    age_pop,
    join_by(year, age_band)
  )

# Tidy up ecoss data
ecoss_age_data <- Aggregate_AgeGp |>
  janitor::clean_names() |>
  rename(week = is_oweek,
         age_band = report_age_band,
         indicator = organism) |>
  select(year, week, indicator, age_band, rate, count, pop)

# Bind all indicator data
tidy_age_data <- ecoss_age_data |>
  bind_rows(nhs24_age_data) |>
  bind_rows(covid_age_data) |>
  bind_rows(syndromic_age_data) |>
  mutate(report_week = grates::isoweek(year, week),
         season = threshtools::find_flu_season(as_date(report_week)),
         age_band = case_when(age_band == "Under 1" ~ "<1",
                              age_band == "Over 75" ~ "75+",
                              TRUE ~ age_band),
         age_band = factor(age_band, levels = age_bands)) |>
  arrange(report_week)


## Tidy HB data ------------------------------------------------------------

# Tidy up syndromic data
syndromic_hb_data <- sirsp_data$data$MEM$ILI$health_board |>
  mutate(indicator = "GPILI") |>
  bind_rows(
    sirsp_data$data$MEM$ARI$health_board |>
      mutate(indicator = "ARI")
  ) |>
  janitor::clean_names() |>
  left_join(
    hb_cipher, join_by(demographic_group)
  ) |>
  rename(rate = adjusted_rate_100000,
         week = iso_week) |>
  select(year, week, indicator, health_board, rate) |>
  left_join(
    hb_pop |>
      select(year, health_board, pop),
    join_by(year, health_board)
  ) |>
  mutate(count = round_half_up((rate / 100000) * pop, 0))

# Tidy up NHS24 data
nhs24_hb_data <- nhs24_raw_data[names(nhs24_raw_data) %in% hb_cipher$health_board] |>
  bind_rows(.id = "health_board") |>
  rename(year = isoyear,
         week = isoweek,
         rate = data) |>
  mutate(indicator = "NHS24") |>
  left_join(
    hb_pop |>
      select(year, health_board, pop),
    join_by(year, health_board)
  ) |>
  mutate(count = round_half_up((rate / 100000) * pop, 0))

# Tidy up covid data
covid_hb_data <- dash_hb_data |>
  janitor::clean_names() |>
  filter(pathogen == "Covid-19") |>
  mutate(indicator = "COVID-19") |>
  rename(week = iso_week,
         rate = rate_per100000,
         health_board = hb) |>
  select(year, week, indicator, health_board, rate, count)

# Tidy up ecoss data
ecoss_hb_data <- Aggregate_HB |>
  janitor::clean_names() |>
  rename(week = is_oweek,
         indicator = organism) |>
  select(year, week, indicator, health_board, rate, count)

# Bind all indicator data
tidy_hb_data <- ecoss_hb_data |>
  bind_rows(covid_hb_data) |>
  bind_rows(nhs24_hb_data) |>
  bind_rows(syndromic_hb_data) |>
  mutate(report_week = grates::isoweek(year, week),
         season = threshtools::find_flu_season(as_date(report_week))) |>
  arrange(report_week)

# Clear objects -----------------------------------------------------------

rm(list = setdiff(ls(), c("tidy_data", "tidy_age_data", "tidy_hb_data")))
