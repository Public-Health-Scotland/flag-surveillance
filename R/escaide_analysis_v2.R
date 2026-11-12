
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

#source("/PHI_conf/Respiratory_Surveillance_General/Matthew_Hoyle/get_ecoss_data.R")

source("R/get_latest_report_data.R")

hb_names <- arrow::read_parquet(here::here("data/hb_names.parquet"))

# Load functions ----------------------------------------------------------

source(here::here("R/functions.R"))

# Packages ----------------------------------------------------------------

pacman::p_load(tidyverse, lubridate, ISOweek, surveillance, purrr, janitor,
               glue, gt, grates, threshtools)


# Set parameters ----------------------------------------------------------

start_year <- 2023

season_start_wk <- 40
rsv_season_start_wk <- 30

pandemic_seasons <- c("2020/2021", "2021/2022", "2022/2023")

indicators <- c("Influenza (A or B)", "RSV", "GPILI", "ARI", "NHS24") |>
  purrr::set_names()

# Set Theme
old <- theme_set(theme_bw())


# Format data -------------------------------------------------------------
scot_data <- tidy_data |>
  filter(indicator %in% indicators) |>
  mutate(week_date = as_date(report_week),
         season = case_when(indicator == "RSV" ~ threshtools::find_flu_season(week_date, start_week = rsv_season_start_wk),
                            TRUE ~ threshtools::find_flu_season(week_date, start_week = season_start_wk))) |>
  arrange(report_week) |>
  rename(organism = indicator,
         iso_week = week)

hb_data <- tidy_hb_data |>
  filter(indicator %in% indicators) |>
  mutate(week_date = as_date(report_week),
         season = case_when(indicator == "RSV" ~ threshtools::find_flu_season(week_date, start_week = rsv_season_start_wk),
                            TRUE ~ threshtools::find_flu_season(week_date, start_week = season_start_wk))) |>
  arrange(report_week) |>
  rename(organism = indicator,
         iso_week = week)

age_data <- tidy_age_data |>
  filter(indicator %in% indicators) |>
  mutate(week_date = as_date(report_week),
         season = case_when(indicator == "RSV" ~ threshtools::find_flu_season(week_date, start_week = rsv_season_start_wk),
                            TRUE ~ threshtools::find_flu_season(week_date, start_week = season_start_wk))) |>
  arrange(report_week) |>
  rename(organism = indicator,
         iso_week = week,
         report_age_band = age_band)


# Create sts object ------------------------------------------------------

# Vector of pathogen names
# pathogens <- unique(hb_data$organism)

# Create list of sts objects for each pathogen
sts_list <- map(indicators, hb_sts, data = hb_data)

# Create list of sts objects for each pathogen
sts_age_list <- map(indicators, age_sts, data = age_data)


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
                     populationOffset = FALSE,
                     powertrans = "2/3",
                     fitFun = "algo.farrington.fitGLM.flexible",
                     b = 2, w = 3,
                     weightsThreshold = 2.58, pastWeeksNotIncluded = 2,
                     pThresholdTrend = 1, thresholdMethod = "nbPlugin",
                     alpha = 0.05, limit54 = c(5, 4))


## Scotland ---------------------------------------------------------------

sts_scot <- map(sts_list, aggregate, by = "unit")

# Run ff model with same parameters

scot_pc.noufaily <- map(sts_scot, farringtonFlexible, con.noufaily)

temp <- farringtonFlexible(
  sts_scot[[1]],
  control = con.noufaily
)

rlang::last_trace()

str(sts_scot[[3]])
str(con.noufaily)

# Tidy Outputs ------------------------------------------------------------

output_scot <- map(scot_pc.noufaily, tidy_outputs)

gg_outbreak(tidy_output = output_scot$`Influenza (A or B)`)

gg_outbreak(tidy_output = output_scot$RSV)

osd_start_week <- output_scot |>
  imap(\(x, idx){
    x |>
      mutate(season = case_when(idx == "RSV" ~ threshtools::find_flu_season(week_date, start_week = rsv_season_start_wk),
                                TRUE ~ threshtools::find_flu_season(week_date, start_week = season_start_wk)),
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

mem_data <- scot_data |>
  split(~organism) |>
  map(\(x){
    seasons |>
      map(\(y){
        x |>
          threshtools::data_to_mem(seasons = y, season_col = season) |>
          mem::memmodel() |>
          threshtools::tidy_mem()
      }) |>
      bind_rows(.id = "season")
  }) |>
  bind_rows(.id = "organism")

mem_start_week <- scot_data |>
  left_join(
    mem_data |>
      select(organism, season, low_threshold),
    join_by(organism, season)
  ) |>
  group_by(organism, season) |>
  filter(rate >= low_threshold) |>
  arrange(week_date) |>
  slice_head() |>
  rename(week = iso_week)

mem_start_week |>
  filter(organism == "Influenza (A or B)")


# Compare OSD to MEM ------------------------------------------------------

mem_start_week |>
  select(organism, season, week, week_date) |>
  left_join(
    osd_start_week |>
    select(organism, season, week, week_date),
    join_by(organism, season),
    suffix = c(".mem", ".osd")
  ) |>
  mutate(week_diff = interval(week_date.osd, week_date.mem) / weeks(1)) |>
  select(!starts_with("week_date")) |>
  split(~organism)


# Detect out of season alarms ---------------------------------------------

rsv_season = c(30:53, 1:10)
flu_season = c(40:53, 1:20)

output_scot |>
  imap(\(x, idx){
    x |>
      mutate(season = case_when(idx == "RSV" ~ threshtools::find_flu_season(week_date, start_week = rsv_season_start_wk),
                                TRUE ~ threshtools::find_flu_season(week_date, start_week = season_start_wk)),
             week = lubridate::isoweek(week_date),
             within_season = case_when(idx == "RSV" ~ week %in% rsv_season,
                                       TRUE ~ week %in% flu_season)) |>
      group_by(season) |>
      filter(alarm == TRUE,
             within_season == FALSE,
             !season %in% pandemic_seasons)
   }) |>
   bind_rows(.id = "organism")

