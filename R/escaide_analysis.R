
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
               glue, gt, grates, threshtools, patchwork)


# Set parameters ----------------------------------------------------------

start_year <- 2015

season_start_wk <- 40
rsv_season_start_wk <- 30

pandemic_seasons <- c("2020/2021", "2021/2022")

pathogens <- c("Influenza (A or B)", "RSV") |>
  purrr::set_names()

# Set Theme
old <- theme_set(theme_bw())


# Format data -------------------------------------------------------------
scot_data <- Aggregate_Scot |>
  clean_names() |>
  filter(organism %in% pathogens) |>
  rename(iso_week = is_oweek) |>
  mutate(week_date = as_date(grates::isoweek(year = year, week = iso_week)),
         season = case_when(organism == "RSV" ~ threshtools::find_flu_season(week_date, start_week = rsv_season_start_wk),
                            TRUE ~ threshtools::find_flu_season(week_date, start_week = season_start_wk))) |>
  arrange(week_date)

hb_data <- Aggregate_HB |>
  clean_names() |>
  filter(organism %in% pathogens) |>
  rename(iso_week = is_oweek) |>
  mutate(week_date = as_date(grates::isoweek(year = year, week = iso_week)),
         season = case_when(organism == "RSV" ~ threshtools::find_flu_season(week_date, start_week = rsv_season_start_wk),
                            TRUE ~ threshtools::find_flu_season(week_date, start_week = season_start_wk))) |>
  arrange(week_date)

# Create sts object ------------------------------------------------------

# Create list of sts objects for each pathogen
sts_list <- map(pathogens, hb_sts, data = hb_data)

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
                    # b = 1, w = 3,
                     weightsThreshold = 2.58, pastWeeksNotIncluded = 2,
                     pThresholdTrend = 1, thresholdMethod = "nbPlugin",
                     alpha = 0.05, limit54 = c(5, 4))


## Scotland ---------------------------------------------------------------

sts_scot <- map(sts_list, aggregate, by = "unit")

# Run ff model with same parameters

scot_pc.noufaily <- map(sts_scot, farringtonFlexible, con.noufaily)



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
  "2017/2018" = c("2011/2012", "2013/2014",
    "2014/2015", "2015/2016", "2016/2017"),
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

comparison_tbl <- mem_start_week |>
  select(organism, season, week, week_date) |>
  left_join(
    osd_start_week |>
    select(organism, season, week, week_date),
    join_by(organism, season),
    suffix = c(".mem", ".osd")
  ) |>
  mutate(week_diff = interval(week_date.osd, week_date.mem) / weeks(1))

comparison_tbl |>
  select(!starts_with("week_date")) |>
  split(~organism)


# Detect out of season alarms ---------------------------------------------

rsv_season = c(30:53, 1:10)
flu_season = c(40:53, 1:20)

output_scot |>
  bind_rows(.id = "organism") |>
  mutate(season = case_when(organism == "RSV" ~ threshtools::find_flu_season(week_date, start_week = rsv_season_start_wk),
                            TRUE ~ threshtools::find_flu_season(week_date, start_week = season_start_wk)),
         week = lubridate::isoweek(week_date),
         within_season = case_when(organism == "RSV" ~ week %in% rsv_season,
                                   TRUE ~ week %in% flu_season)) |>
  group_by(season) |>
  filter(alarm == TRUE,
         within_season == FALSE,
         !season %in% pandemic_seasons)

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



# Poster plot -------------------------------------------------------------

plot_dates = list(
  start = as_date(grates::isoweek(2025, 30)),
  end = as_date(grates::isoweek(2026, 20))
)

breaks <- seq(
  plot_dates$start,
  plot_dates$end,
  by = "2 weeks"
)


plot_colours = list(
  success = "#67a9cf",
  warning = "#ef8a62"
)

plot_data <- output_scot|>
  bind_rows(.id = "organism") |>
  mutate(season = case_when(organism == "RSV" ~ threshtools::find_flu_season(week_date, start_week = rsv_season_start_wk),
                            TRUE ~ threshtools::find_flu_season(week_date, start_week = season_start_wk)),
         week = lubridate::isoweek(week_date),
         within_season = case_when(organism == "RSV" ~ week %in% rsv_season,
                                   TRUE ~ week %in% flu_season)) |>
  left_join(
    mem_data |>
      select(organism, season, low_threshold),
    join_by(organism, season)
  ) |>
  split(~organism)

gg_escaide_outbreak <- function(group = "overall", tidy_output){

  alarm_weeks <- tidy_output |>
    filter(unit == group,
           alarm == TRUE)

  plot <- tidy_output |>
    left_join(mem_data |>
                select(organism, season, low_threshold),
              join_by(organism, season)) |>
    filter(unit == group)  |>
    ggplot(aes(x = week_date)) +
    geom_col(aes(y = observed), alpha = 0.8) +
    geom_line(aes(y = upperbound, colour = "ODA alarm threshold"), linetype = "dashed", linewidth = 1.1, alpha = 0.85) +
    geom_line(aes(y = expected, colour = "ODA expected count"), linetype = "dashed", linewidth = 1.1, alpha = 0.85) +
    scale_color_manual(values = c("ODA alarm raised" = plot_colours$warning, "ODA alarm threshold" = plot_colours$warning, "ODA expected count" = plot_colours$success)) +
    geom_point(data = alarm_weeks, aes(x = week_date, y = -50, colour = "ODA alarm raised"), size = 2.5) +
    scale_x_date(
      breaks = breaks,
      date_labels = "%Y-%V"
    ) +
    labs(x = "ISO Week",
         y = "Confirmed\nCases") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.title.y = element_text(angle = 0, vjust = 0.5))

  return(plot)
}

osd_plot <- gg_escaide_outbreak(
  tidy_output = plot_data$`Influenza (A or B)` |>
    filter(week_date >= plot_dates$start,
           week_date <= plot_dates$end)
)


mem_plot_data <- scot_data |>
  filter(week_date >= plot_dates$start,
         week_date <= plot_dates$end) |>
  left_join(
    mem_data |>
      select(organism, season, low_threshold),
    join_by(organism, season)
  )

flu_start_weeks <- mem_start_week |>
  filter(organism == "Influenza (A or B)",
         week_date >= plot_dates$start,
         week_date <= plot_dates$end)


mem_plot <- mem_plot_data |>
  filter(organism == "Influenza (A or B)") |>
  ggplot(aes(x = week_date, y = rate)) +
  geom_ribbon(aes(ymin = 0, ymax = low_threshold, fill = "MEM baseline"), alpha = 0.5) +
  geom_ribbon(aes(ymin = Inf, ymax = low_threshold, fill = "Above MEM baseline"), alpha = 0.5) +
  geom_col(alpha = 0.9) +
  geom_point(data = flu_start_weeks, aes(x = week_date, y = -1.5, colour = "First week above MEM baseline"), shape = 17, size = 3) +
  scale_x_date(
    breaks = breaks,
    date_labels = "%Y-%V"
  ) +
  scale_fill_manual(values = c("MEM baseline" = plot_colours$success, "Above MEM baseline" = plot_colours$warning)) +
  scale_color_manual(values = c("First week above MEM baseline" = plot_colours$warning)) +
  labs(x = "ISO Week",
       y = "Rate per\n100,000\npopulation") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(angle = 0, vjust = 0.5))


poster_plot <- mem_plot / osd_plot +
  plot_layout(axis_titles = "collect_x")

poster_plot


# Plot table --------------------------------------------------------------

poster_tbl <- comparison_tbl |>
  ungroup() |>
  filter(organism == "Influenza (A or B)") |>
  select(season, week.mem, week.osd, week_diff) |>
  gt() |>
  cols_label(
    season = md("**Season**"),
    week.mem = md("***MEM***"),
    week.osd = md("***OSD***"),
    week_diff = md("**Time difference (weeks)**")
  ) |>
  tab_spanner(label = md("**First exceedance week**"), columns = starts_with("week."))

poster_tbl



# Save outputs ------------------------------------------------------------

ggsave(filename = "outputs/escaide_plot.svg", plot = poster_plot, dpi = 300, width = 7, height = 10)

gt::gtsave(data = poster_tbl, filename = "outputs/poster_tbl.html")

