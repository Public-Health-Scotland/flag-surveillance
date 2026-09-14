
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

hb_names <- arrow::read_parquet(here::here("data/hb_names.parquet"))

# Load functions ----------------------------------------------------------

source(here::here("R/functions.R"))

# Packages ----------------------------------------------------------------

pacman::p_load(tidyverse, lubridate, ISOweek, surveillance, purrr, janitor,
               glue, gt, grates)


# Set parameters ----------------------------------------------------------

start_year <- 2016

pathogens <- c("Influenza (A or B)", "RSV") |>
  purrr::set_names()

# Set Theme
old <- theme_set(theme_bw())


# Format data -------------------------------------------------------------

hb_data <- Aggregate_HB |>
  clean_names() |>
  #filter(str_detect(organism, "Influenza|RSV")) |>
  filter(organism %in% pathogens) |>
  rename(iso_week = is_oweek) |>
  mutate(week_date = as_date(grates::isoweek(year = year, week = iso_week))) |>
  arrange(week_date)

age_data <- Aggregate_AgeGp |>
  clean_names() |>
  #filter(str_detect(organism, "Influenza")) |>
  filter(organism %in% pathogens) |>
  rename(iso_week = is_oweek) |>
  mutate(week_date = as_date(grates::isoweek(year = year, week = iso_week))) |>
  arrange(week_date)


# Create sts object ------------------------------------------------------

# Vector of pathogen names
# pathogens <- unique(hb_data$organism)

# Create list of sts objects for each pathogen
sts_list <- map(pathogens, hb_sts, data = hb_data)

# Create list of sts objects for each pathogen
sts_age_list <- map(pathogens, age_sts, data = age_data)


# Run Farrington Flexible model -------------------------------------------

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


## Health Boards ----------------------------------------------------------

# Run model (Farrington flexible with noufaily adaptation)

#hb_pc.noufaily <- map(sts_list, farringtonFlexible, con.noufaily)


## Scotland ---------------------------------------------------------------

sts_scot <- map(sts_list, aggregate, by = "unit")

# Run ff model with same parameters

scot_pc.noufaily <- map(sts_scot, farringtonFlexible, con.noufaily)


## Age bands ----------------------------------------------------------

# Run model (Farrington flexible with noufaily adaptation)

#age_pc.noufaily <- map(sts_age_list, farringtonFlexible, con.noufaily)



# Tidy Outputs ------------------------------------------------------------

#output_hb <- map(hb_pc.noufaily, tidy_outputs)

output_scot <- map(scot_pc.noufaily, tidy_outputs)

#output_age <- map(age_pc.noufaily, tidy_outputs)

#output_list <- map2(output_scot, output_hb, bind_rows) |>
  #map2(output_age, bind_rows)

gg_outbreak(tidy_output = output_scot$`Influenza (A or B)`)

gg_outbreak(tidy_output = output_scot$RSV)

osd_start_week <- output_scot |>
  map(\(x){
    x |>
      mutate(season = threshtools::find_flu_season(week_date),
             week = lubridate::isoweek(week_date)) |>
      group_by(season) |>
      filter(alarm == TRUE) |>
      arrange(week_date) |>
      slice_head()
  }) |>
  bind_rows(.id = "organism")

osd_start_week |>
  filter(organism == "Influenza (A or B)")

# Calculate epidemic threshold using MEM ----------------------------------

seasons = list(
  "2025/2026" = c("2017/2018", "2018/2019", "2022/2023", "2023/2024", "2024/2025"),
  "2024/2025" = c("2016/2017", "2017/2018", "2018/2019", "2022/2023", "2023/2024"),
  "2023/2024" = c("2015/2016", "2016/2017", "2017/2018", "2018/2019", "2022/2023"),
  "2022/2023" = c("2014/2015", "2015/2016", "2016/2017", "2017/2018", "2018/2019"),
  "2021/2022" = c("2014/2015", "2015/2016", "2016/2017", "2017/2018", "2018/2019"),
  "2018/2019" = c("2013/2014", "2014/2015", "2015/2016", "2016/2017", "2017/2018"),
  "2017/2018" = c("2011/2012", "2013/2014", "2014/2015", "2015/2016", "2016/2017"),
  "2016/2017" = c("2010/2011", "2011/2012", "2013/2014", "2014/2015", "2015/2016")
)


scot_data <- Aggregate_Scot |>
  clean_names() |>
  #filter(str_detect(organism, "Influenza")) |>
  filter(organism %in% pathogens) |>
  rename(iso_week = is_oweek) |>
  mutate(week_date = as_date(grates::isoweek(year = year, week = iso_week))) |>
  arrange(week_date)

mem_data <- scot_data |>
  split(~organism) |>
  map(\(x){
    seasons |>
      map(\(y){
        x |>
          threshtools::data_to_mem(seasons = y) |>
          mem::memmodel() |>
          threshtools::tidy_mem()
      }) |>
      bind_rows(.id = "flu_season")
  }) |>
  bind_rows(.id = "organism")

mem_start_week <- scot_data |>
  left_join(
    mem_data |>
      select(organism, flu_season, low_threshold),
    join_by(organism, flu_season)
  ) |>
  group_by(organism, flu_season) |>
  filter(rate >= low_threshold) |>
  arrange(week_date) |>
  slice_head() |>
  rename(week = iso_week)

mem_start_week |>
  filter(organism == "Influenza (A or B)")


# Compare OSD to MEM ------------------------------------------------------

osd_start_week |>
    select(organism, season, week, week_date) |>
  left_join(
    mem_start_week |>
      select(organism, flu_season, week, week_date),
    join_by(organism, season == flu_season),
    suffix = c(".osd", ".mem")
  ) |>
  mutate(week_diff = interval(week_date.osd, week_date.mem) / weeks(1)) |>
  select(!starts_with("week_date")) |>
  split(~organism)

