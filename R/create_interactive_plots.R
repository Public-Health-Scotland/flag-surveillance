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

tooltip_css <- "background-color:#3F3685;font-family:Arial;color:white;padding:5px;border-radius:3px;"


# Format data -------------------------------------------------------------

# Function to format labels

make_nice_label <- function(week_date, observed, expected, upperbound, alarm_text) {
  wk_label <- htmltools::span(
    glue('Week {lubridate::isoweek(week_date)}:'),
    style = htmltools::css(
      font_weight = 600,
      font_size = '20px'
    )
  )
  count_label <- htmltools::span(
    glue('Count: {observed}'),
    style = htmltools::css(
      font_size = '16px'
    )
  )
  expected_label <- htmltools::span(
    glue('Expected Count: {expected}'),
    style = htmltools::css(
      font_size = '16px'
    )
  )
  threshold_label <- htmltools::span(
    glue('Alarm Threshold: {upperbound}'),
    style = htmltools::css(
      font_size = '16px'
    )
  )
  alarm_label <- htmltools::span(
    glue('Alarm raised: {alarm_text}'),
    style = htmltools::css(
      font_size = '16px'
    )
  )
  glue::glue('{wk_label}<br>{count_label}<br>{expected_label}<br>{threshold_label}<br>{alarm_label}')
}


# Plotting function

ggiraph_outbreak <- function(group = "overall", tidy_output){

  plot_data <- tidy_output |>
    filter(unit == group)

  alarm_weeks <- plot_data |>
    filter(alarm == TRUE)

  # Apply function to format labels
  plot_data_formatted <- plot_data |>
    mutate(
      alarm_text = case_when(alarm == FALSE ~ "No",
                             TRUE ~ "Yes 🚩"),
      nice_label = pmap_chr(
        list(
          week_date,
          observed,
          expected,
          upperbound,
          alarm_text
        ),
        make_nice_label
      )
    )

  plot <- plot_data_formatted  |>
    ggplot(aes(x = week_date)) +
    geom_col_interactive(aes(y = observed, tooltip = nice_label, data_id = observed)) +
    geom_line(aes(y = upperbound, colour = "Alarm Threshold"), linetype = "dashed") +
    geom_line(aes(y = expected, colour = "Expected Count"), linetype = "dashed") +
    scale_color_manual(values = c("Alarm Threshold" = "#F8766D", "Expected Count" = "#00BA38", "Alarm Raised" = "#F8766D")) +
    geom_point(data = alarm_weeks, aes(x = week_date, y = 0, colour = "Alarm Raised"), size = 2) +
    scale_x_date(date_breaks = "month", date_labels = "%Y-%V") +
    labs(x = "ISO Week",
         y = "No. Positive Samples",
         caption  = "Source: ECOSS") +
    theme(legend.position = "bottom", # Corrected legend position
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}


# Create plots ------------------------------------------------------------

iplot_list <- map(output_list, \(x) map(hb_vec, ggiraph_outbreak, tidy_output = x))


ggiraph::girafe(ggobj = iplot_list$Adenovirus$overall,
                options = list(
                  opts_tooltip(css = tooltip_css, offx = 15, offy = 15, opacity = 1),
                  opts_sizing(width = .8),
                  opts_hover(css = ''),
                  opts_hover_inv(css ='opacity:0.8;fill:lightgrey;')
                )
)
