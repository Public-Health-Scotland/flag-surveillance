#' Title: Create interactive plots from FLAG outputs
#' Author: Matthew Hoyle
#' Date: 01/04/2025
#'
#' Notes:
#' This script runs utilises the {ggiraph} package to create interactive plots
#' from the model outputs



# Load packages -----------------------------------------------------------

pacman::p_load(tidyverse, ggplot2, ggiraph, glue, lubridate)


# Set defaults ----------------------------------------------------------

old <- theme_set(theme_bw(base_family = "Arial"))


# Create static plots ------------------------------------------------------------

# Vector of hb names to filter list
hb_vec <- unique(output_list$Adenovirus$unit) |>
  set_names()

# Individual plots for each pathogen in each health board
plot_list <- map(output_list, \(x) map(hb_vec, gg_outbreak, tidy_output = x))

# Plots for each pathogen faceted by Health board
plot_list_faceted <- map2(output_list, names(output_list), gg_outbreak_facet)


# Create Interactive plots ------------------------------------------------

iplot_list <- map(output_list, \(x) map(hb_vec, ggiraph_outbreak, tidy_output = x))


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
  left_join(hb_names, join_by(unit == health_board)) |>
  mutate(unit = factor(unit, levels = hb_vec)) |>
  arrange(unit) |>
  mutate(rate = round_half_up(observed/population * 100000, 2))

historic_alarms <- output_list |>
  bind_rows(.id = "organism") |>
  filter(alarm == TRUE) |>
  mutate(week_date = grates::as_isoweek(week_date),
         rate = round_half_up(observed/population * 100000, 2)) |>
  arrange(week_date)



# Save outputs ------------------------------------------------------------

# Save plots

if (!dir.exists(here::here("outputs/plots"))) {
  dir.create(here::here("outputs/plots"))
}

for (i in 1:length(plot_list_faceted)) {

  ggsave(
    plot_list_faceted[[i]],
    filename = here::here(glue("outputs/plots/FF_plot_{names(plot_list_faceted[i])}.png")),
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
if (!dir.exists(here::here("outputs/tables"))) {
  dir.create(here::here("outputs/tables"))
}

write_csv(alarms_this_week, file = here::here(glue("outputs/tables/Alarm_pathogens_{today()}.csv")))

write_csv(historic_alarms, file = here::here(glue("outputs/tables/historic_alarms.csv")))


