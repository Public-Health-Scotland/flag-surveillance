
#' Title: Aberration detection model of ECOSS respiratory pathogens
#' Author: Matthew Hoyle
#' Date: 01/04/2025
#'
#' Notes:
#' This script runs utilises the `farringtonFlexible()` function from the
#' {surveillance} package in R to detect aberrations in time series data.
#' Currently the model is ran on all respiratory pathogens pulled from ECOSS
#' during the production of the weekly report. Parameters are currently set to
#' include data from the last two seasons (excludes all data pre-covid).



# Load Data ---------------------------------------------------------------

source("/PHI_conf/Respiratory_Surveillance_General/Matthew_Hoyle/get_ecoss_data.R")

#load("here::here(data/model_data.rds"))


# Keep only Aggregated data objects

rm(list=setdiff(ls(), c("Aggregate_Scot", "Aggregate_HB", "Aggregate_AgeGp")))

hb_names <- arrow::read_parquet(here::here("data/hb_names.parquet"))


# Packages ----------------------------------------------------------------

pacman::p_load(tidyverse, lubridate, ISOweek, surveillance, purrr, janitor,
               glue, gt, grates)


# Set parameters ----------------------------------------------------------

start_year <- 2024

# Set Theme
old <- theme_set(theme_bw())


# Format data -------------------------------------------------------------

hb_data <- Aggregate_HB |>
  clean_names() |>
  filter(!organism %in% c("Parainfluenza (Type Not Known)",
                          "Parainfluenza Type 1",
                          "Parainfluenza Type 2",
                          "Parainfluenza Type 3",
                          "Parainfluenza Type 4")) |>
  rename(iso_week = is_oweek) |>
  mutate(week_date = as_date(grates::isoweek(year = year, week = iso_week))) |>
  arrange(week_date)


# Create sts object ------------------------------------------------------

# Vector of pathogen names
pathogens <- unique(hb_data$organism) # replace with list of pathogens that we report on
names(pathogens) <- pathogens

# Create list of sts objects for each pathogen
sts_list <- map(pathogens, hb_sts, data = hb_data)


# Run Farrington Flexible model -------------------------------------------

## Health Boards ----------------------------------------------------------

# Calculate range to plot (from the start of the season)

season_epoch <- function(sts){
  which(isoWeekYear(epoch(sts))$ISOYear >= start_year)[-c(1:39)]
}

seapoch_list <- map(sts_list, season_epoch)


# Set model parameters

con.noufaily <- list(range = seapoch_list[[1]], noPeriods = 10,
                     reweight = TRUE,
                     trend = TRUE,
                     populationOffset = TRUE,
                     powertrans = "2/3",
                     fitFun = "algo.farrington.fitGLM.flexible",
                     b = 2, w = 3,
                     weightsThreshold = 2.58, pastWeeksNotIncluded = 2,
                     pThresholdTrend = 1, thresholdMethod = "nbPlugin",
                     alpha = 0.05, limit54 = c(5, 4))


# Run model (Farrington flexible with noufaily adaptation)

hb_pc.noufaily <- map(sts_list, farringtonFlexible, con.noufaily)


## Scotland ---------------------------------------------------------------

sts_scot <- map(sts_list, aggregate, by = "unit")

# Run ff model with same parameters

scot_pc.noufaily <- map(sts_scot, farringtonFlexible, con.noufaily)


# Tidy Outputs ------------------------------------------------------------

output_scot <- map(scot_pc.noufaily, tidy_outputs)

output_hb <- map(hb_pc.noufaily, tidy_outputs)

output_list <- map2(output_scot, output_hb, bind_rows)
