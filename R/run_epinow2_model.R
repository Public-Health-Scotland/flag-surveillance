#' Title: Estimating transmission using epinow2
#' Author: Matthew Hoyle
#' Date: 03/10/2025
#'
#' Notes:
#'



# Load Data ---------------------------------------------------------------

source("/PHI_conf/Respiratory_Surveillance_General/Matthew_Hoyle/get_ecoss_data.R")

#load("data/model_data.rds")

# Keep only Aggregated data objects

#rm(list=setdiff(ls(), c("Aggregate_Scot", "Aggregate_HB", "Aggregate_AgeGp")))


# Packages ----------------------------------------------------------------

pacman::p_load(tidyverse, lubridate, purrr, janitor,
               glue, gt, grates, incidence2, EpiNow2, epiparameter)

#source("R/functions.R")



# Set parameters ----------------------------------------------------------

current_season = "2025/2026"

plot_seasons = c("2023/2024", "2024/2025", "2025/2026")

# Create incidence object -------------------------------------------------

flu_incidence <- all_episodes |>
  clean_names() |>
  rename(iso_week = is_oweek) |>
  filter(organism == "Influenza",
         type == "Type A",
         flu_season %in% plot_seasons) |>
  incidence_(
    date_index = specimen_date,
    interval = "day",
    complete_dates = TRUE,
    count_values_to = "confirm",
    date_names_to = "date"
  ) |>
  slice_head(n=90)


plot(flu_incidence)


# Define a generation time from {epiparameter} to {EpiNow2} ---------------

# access a serial interval
dat_serialint <- epiparameter::epiparameter_db(
  disease = "influenza",
  epi_name = "serial",
  single_epiparameter = TRUE
)

plot(dat_serialint)

# extract parameters from {epiparameter} object
dat_serialint_params <- epiparameter::get_parameters(dat_serialint)

# adapt {epiparameter} to {EpiNow2} distribution interface
dat_generationtime <- EpiNow2::Gamma(
  shape = dat_serialint_params["shape"],
  scale = dat_serialint_params["scale"]
)


# Define the delays from infection to case report for {EpiNow2} -----------

# define delay from symptom onset to case report
dat_reportdelay <- EpiNow2::Gamma(
  mean = EpiNow2::Normal(mean = 2, sd = 0.5),
  sd = EpiNow2::Normal(mean = 1, sd = 0.5),
  max = 5
)

# define a delay from infection to symptom onset
dat_incubationtime <- epiparameter::epiparameter_db(
  pathogen = "influenza A",
  epi_name = "incubation",
  single_epiparameter = T
)

plot(dat_incubationtime)


# incubation period: extract distribution parameters
dat_incubationtime_params <- epiparameter::get_parameters(dat_incubationtime)

# incubation period: discretize and extract maximum value (p = 99%)
dat_incubationtime_max <- dat_incubationtime |>
  epiparameter::discretise() |>
  quantile(p = 0.99)

# incubation period: adapt to {EpiNow2} distribution interface
dat_incubationtime_epinow <- EpiNow2::LogNormal(
  meanlog = dat_incubationtime_params["meanlog"],
  sdlog = dat_incubationtime_params["sdlog"],
  max = dat_incubationtime_max
)

# print required input
dat_generationtime
dat_reportdelay
dat_incubationtime_epinow


# Set the number of parallel cores for {EpiNow2} --------------------------
withr::local_options(list(mc.cores = parallel::detectCores() - 1))


# Estimate transmission using EpiNow2::epinow() ---------------------------
# with EpiNow2::*_opts() functions for generation time, delays, and stan.
estimates <- EpiNow2::epinow(
  data = flu_incidence,
  generation_time = EpiNow2::generation_time_opts(dat_incubationtime_epinow),
  delays = EpiNow2::delay_opts(dat_reportdelay + dat_incubationtime_epinow),
  stan = EpiNow2::stan_opts(samples = 1000, chains = 3)
)


# Print plot and summary table outputs ------------------------------------
summary(estimates)
plot(estimates)



# Save estimates ----------------------------------------------------------

saveRDS(estimates, "data/estimates.rds")
