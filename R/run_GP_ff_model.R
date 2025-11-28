# Set-up ------------------------------------------------------------------
library(quarto)

source("R/Read_in_tidy_GP_data.R")
source("R/GP_functions.R")

# Set parameters ----------------------------------------------------------

start_year <- 2024

# Set Theme
old <- theme_set(theme_bw())

# Create sts object ------------------------------------------------------

# Vector of consultation names
consultations <- unique(all_ILI_ARI_data$consultation_type) # replace with list of consultation types that we report on
names(consultations) <- consultations

# Create list of sts objects for each consultation type
GP_sts_list <- map(consultations, GP_hb_sts, data = GP_hb_data)

# Run Farrington Flexible model -------------------------------------------

## Health Boards ----------------------------------------------------------

# Calculate range to plot (from the start of the season)
GP_seapoch_list <- map(GP_sts_list, ~ season_epoch(.x, alarm_year = 2024, alarm_week = 40))

# Set model parameters
GP_con.noufaily <- list(
  range = GP_seapoch_list[[1]],  # dynamically calculated
  noPeriods = 10,
  b = 1,
  w = 3,
  reweight = TRUE,
  trend = TRUE,
  populationOffset = TRUE,
  powertrans = "2/3",
  fitFun = "algo.farrington.fitGLM.flexible",
  weightsThreshold = 2.58,
  pastWeeksNotIncluded = 2,
  pThresholdTrend = 1,
  thresholdMethod = "nbPlugin",
  alpha = 0.05,
  limit54 = c(5, 4)
)


# Run model (Farrington flexible with noufaily adaptation)
GP_hb_pc.noufaily <- map(GP_sts_list, farringtonFlexible, GP_con.noufaily)

## Scotland ---------------------------------------------------------------

GP_sts_scot <- map(GP_sts_list, aggregate, by = "unit")

# Run ff model with same parameters

GP_scot_pc.noufaily <- map(GP_sts_scot, farringtonFlexible, GP_con.noufaily)

# Tidy Outputs ------------------------------------------------------------

GP_output_scot <- map(GP_scot_pc.noufaily, tidy_outputs)

GP_output_hb <- map(GP_hb_pc.noufaily, tidy_outputs)

GP_output_list <- map2(GP_output_scot, GP_output_hb, bind_rows)

# Create plots ------------------------------------------------------------

# Vector of hb names to filter list
GP_hb_vec <- unique(GP_output_list$ILI$unit)
names(GP_hb_vec) <- GP_hb_vec

# Individual plots for each consultation type in each health board
GP_plot_list <- map(GP_output_list, \(x) map(GP_hb_vec, GP_gg_outbreak, tidy_output = x))

# Plots for each consultation type faceted by Health board
GP_plot_list_faceted <- map2(GP_output_list, names(GP_output_list), GP_gg_outbreak_facet)

# Summary Table -----------------------------------------------------------

GP_season_summary <- GP_output_list |>
  map(\(x)
      x |>
        group_by(unit) |>
        summarise(total_count = sum(observed),
                  total_alarm = sum(alarm))
  )

GP_last_5_weeks <- GP_output_list |>
  map(\(x)
      x |>
        group_by(unit) |>
        slice_max(week_date, n = 5)
  )

GP_week_summary <- GP_output_list |>
  map(\(x)
      x |>
        group_by(unit) |>
        slice_max(week_date, n = 1)
  )

GP_alarm_weeks <- GP_week_summary |>
  map(\(x)
      x |>
        filter(alarm == TRUE)
  ) |>
  list_rbind(names_to = "consultation_type") |>
  mutate(rate = round_half_up(observed/population * 100000, 2))

GP_alarms_this_week <- GP_week_summary |>
  map(\(x)
      x |>
        slice_max(week_date) |>
        filter(alarm == TRUE)
  ) |>
  list_rbind(names_to = "consultation_type") |>
  mutate(rate = round_half_up(observed/population * 100000, 2))

GP_historic_alarms <- GP_output_list |>
  bind_rows(.id = "consultation_type") |>
  filter(alarm == TRUE) |>
  mutate(week_date = grates::as_isoweek(week_date),
         rate = round_half_up(observed/population * 100000, 2)) |>
  arrange(week_date)

# GP HSCP and postcode analysis -------------------------------------------

source("R/Explore_GP_consultations_by_postcode_HSCP.R")

# Save Data ---------------------------------------------------------------
# Save plots

if (!dir.exists("outputs/plots")) {
  dir.create("outputs/plots")
}

for (i in 1:length(GP_plot_list_faceted)) {
  ggsave(
    GP_plot_list_faceted[[i]],
    filename = glue("outputs/plots/FF_plot_{names(GP_plot_list_faceted[i])}.png"),
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

# Save summary of Consultation type/hb combos above alarm threshold for this week
if (!dir.exists("outputs/tables")) {
  dir.create("outputs/tables")
}

write_csv(GP_alarms_this_week, file = glue("outputs/tables/GP_Alarm_{today()}.csv"))
write_csv(GP_historic_alarms, file = glue("outputs/tables/GP_historic_alarms.csv"))

# Save objects for report
if (!dir.exists("data")) {
  dir.create("data")
}

rm(all_ILI_ARI_data, Date_Reference)

save.image("data/GP_model_data.rds")

# Render weekly report ----------------------------------------------------

quarto_render("Flag_GP_weekly_report.qmd")


#To-do:

# - Encorporate into r markdown
# - may need to move all ARI ILI data processing script to rStudio
# - Figure out where to put postcode and hscp flags


