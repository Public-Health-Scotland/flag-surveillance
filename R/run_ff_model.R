
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

#load("data/model_data.rds")

# Keep only Aggregated data objects

rm(list=setdiff(ls(), c("Aggregate_Scot", "Aggregate_HB", "Aggregate_AgeGp")))


# Packages ----------------------------------------------------------------

pacman::p_load(tidyverse, lubridate, ISOweek, surveillance, purrr, janitor, glue, gt)

source("R/functions.R")


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
  mutate(week_date = case_when(iso_week < 10 ~ paste0(year, "-W0", iso_week, "-", 1, sep = ""),
                               TRUE ~ paste0(year, "-W", iso_week, "-", 1, sep = "")),
         week_date = ISOweek::ISOweek2date(week_date)) |>
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



# Create plots ------------------------------------------------------------

# Vector of hb names to filter list

hb_vec <- unique(output_list$Adenovirus$unit)
names(hb_vec) <- hb_vec

# Individual plots for each pathogen in each health board

plot_list <- map(output_list, \(x) map(hb_vec, gg_outbreak, tidy_output = x))



# Plots for each pathogen faceted by Health board

plot_list_faceted <- map2(output_list, names(output_list), gg_outbreak_facet)



# Summary Table -----------------------------------------------------------

season_summary <- output_list |>
  map(\(x)
      x |>
        group_by(unit) |>
        summarise(total_count = sum(observed),
                  total_alarm = sum(alarm))
  )

last_5_weeks <- output_list |>
  map(\(x)
      x |>
        group_by(unit) |>
        slice_max(week_date, n = 5)
  )

week_summary <- output_list |>
  map(\(x)
      x |>
        group_by(unit) |>
        slice_max(week_date, n = 1)
  )

alarm_weeks <- week_summary |>
  map(\(x)
      x |>
        filter(alarm == TRUE)
  ) |>
  list_rbind(names_to = "organism") |>
  mutate(rate = round_half_up(observed/population * 100000, 2))

alarms_this_week <- week_summary |>
  map(\(x)
      x |>
        slice_max(week_date) |>
        filter(alarm == TRUE)
  ) |>
  list_rbind(names_to = "organism") |>
  mutate(rate = round_half_up(observed/population * 100000, 2))





# Save Data ---------------------------------------------------------------

# Save plots

if (!dir.exists("outputs/plots")) {
  dir.create("outputs/plots")
}

for (i in 1:length(plot_list_faceted)) {

  ggsave(
    plot_list_faceted[[i]],
    filename = glue("outputs/plots/FF_plot_{names(plot_list_faceted[i])}.png"),
    width = 12,
    height = 8
  )

}


# plot_list |>
#   imap(\(x, idx){
#     x |>
#       imap(\(y, idy){
#
#         # Add title to plots
#         plot <- y +
#           labs(title = idx,
#                subtitle = idy)
#
#         # Save out plots
#         ggsave(
#           plot,
#           filename = glue("outputs/plots/FF_plot_{idx}_{idy}.png"),
#           width = 12,
#           height = 6
#         )
#       })
#   })


# Save summary of pathogens/hb combos above alarm threshold for this week
if (!dir.exists("outputs/tables")) {
  dir.create("outputs/tables")
}

write_csv(alarms_this_week, file = glue("outputs/tables/Alarm_pathogens_{today()}.csv"))


# Save objects for report
if (!dir.exists("data")) {
  dir.create("data")
}

save.image("data/model_data.rds")



# Render weekly report ----------------------------------------------------

quarto::quarto_render()
