
#' Create `sts` object with heath board regions as units and population matrix
#'
#' @param pathogen Character string of pathogen name
#' @param data Aggregated `data.frame` of weekly counts by health board
#'
#' @return `sts` object with heath board regions as units and population matrix
#' @export
#'
#' @examples

hb_sts <- function(pathogen, data){

  # filter the chosen disease
  pathogen_data <- data |>
    filter(organism == pathogen)

  # Create matrix of counts by hb
  count_matrix <- pathogen_data |>
    select(health_board, count, week_date) |>
    pivot_wider(id_cols = week_date, names_from = health_board, values_from = count) |>
    select(!week_date) |>
    as.matrix()

  # Create matrix of population by hb for each week
  pop_matrix <- pathogen_data |>
    filter(organism == pathogen) |>
    select(year, iso_week, health_board, pop) |>
    pivot_wider(names_from = health_board, values_from = pop) |>
    select(!c(year, iso_week)) |>
    as.matrix()


  # create the sts object
  disease_sts <- surveillance::sts(observed = count_matrix, # weekly number of cases
                                   start = c(min(pathogen_data$year), 01), # first week of the time series
                                   frequency = 52, # weekly data
                                   epochAsDate = TRUE, # we do have dates, not only index
                                   epoch = as.numeric(unique(pathogen_data$week_date)), # here are the dates
                                   population = pop_matrix
  )

  return(disease_sts)

}


#' Create `sts` object with age band as units and population matrix
#'
#' @param pathogen Character string of pathogen name
#' @param data Aggregated `data.frame` of weekly counts by age band
#'
#' @return
#' @export
#'
#' @examples
#'

agegp_sts <- function(pathogen, data){

  # filter the chosen disease
  pathogen_data <- data |>
    filter(organism == pathogen)

  # Create matrix of counts by hb
  count_matrix <- pathogen_data |>
    select(report_age_band, count, week_date) |>
    pivot_wider(id_cols = week_date, names_from = report_age_band, values_from = count) |>
    select(!week_date) |>
    as.matrix()

  # Create matrix of population by hb for each week
  pop_matrix <- pathogen_data |>
    filter(organism == pathogen) |>
    select(year, iso_week, report_age_band, pop) |>
    pivot_wider(names_from = report_age_band, values_from = pop) |>
    select(!c(year, iso_week)) |>
    as.matrix()


  # create the sts object
  disease_sts <- surveillance::sts(observed = count_matrix, # weekly number of cases
                                   start = c(min(pathogen_data$year), 01), # first week of the time series
                                   frequency = 52, # weekly data
                                   epochAsDate = TRUE, # we do have dates, not only index
                                   epoch = as.numeric(unique(pathogen_data$week_date)), # here are the dates
                                   population = pop_matrix
  )

  return(disease_sts)

}





#' Convert model outputs to tidy format
#'
#' @param surveillance_model Output object from `farringtonFlexible()`
#'
#' @return `tbl` of results in tidy format
#' @export
#'
#' @examples
#'

tidy_outputs <- function(surveillance_model){

  unit_names <- colnames(observed(surveillance_model))

  output_tbl <- observed(surveillance_model) |>
    as_tibble() |>
    mutate(week_date = epoch(surveillance_model)) |>
    pivot_longer(cols = !week_date, names_to = "unit", values_to = "observed") |>

    left_join(

      upperbound(surveillance_model) |>
        as_tibble() |>
        mutate(week_date = epoch(surveillance_model)) |>
        pivot_longer(cols = !week_date, names_to = "unit", values_to = "upperbound"),
      join_by(week_date, unit)

    ) |>

    left_join(

      alarms(surveillance_model) |>
        as_tibble() |>
        mutate(week_date = epoch(surveillance_model)) |>
        pivot_longer(cols = !week_date, names_to = "unit", values_to = "alarm"),
      join_by(week_date, unit)

    ) |>

    left_join(

      population(surveillance_model) |>
        as_tibble() |>
        mutate(week_date = epoch(surveillance_model)) |>
        pivot_longer(cols = !week_date, names_to = "unit", values_to = "population"),
      join_by(week_date, unit)

    ) |>

    left_join(

      surveillance_model@control$expected |>
        as_tibble() |>
        set_names(unit_names) |>
        mutate(week_date = epoch(surveillance_model)) |>
        pivot_longer(cols = !week_date, names_to = "unit", values_to = "expected") |>
        mutate(expected = round_half_up(expected, 2)),
      join_by(week_date, unit)

    )
}





#' Plot model outputs using {ggplot2}
#'
#' @param group Character string with name of group to plot
#' @param tidy_output Model outputs which have been tidied using `tidy_outputs()`
#'
#' @return `ggplot` object with bars for weekly counts and lines for expected
#'  and alarm thresholds
#' @export
#'
#' @examples
#'
#'

gg_outbreak <- function(group = "overall", tidy_output){

  alarm_weeks <- tidy_output |>
    filter(unit == group,
           alarm == TRUE)

  plot <- tidy_output |>
    filter(unit == group)  |>
    ggplot(aes(x = week_date)) +
    geom_col(aes(y = observed)) +
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

  return(plot)
}



#' Plot model outputs using {ggplot2} faceted by group
#'
#' @param plot_name Character string with title of plot
#' @param tidy_output Model outputs which have been tidied using `tidy_outputs()`
#'
#' @return `ggplot` object with bars for weekly counts and lines for expected
#'  and alarm thresholds
#' @export
#'
#' @examples
#'

gg_outbreak_facet <- function(tidy_output, plot_name = NULL){

  alarm_weeks <- tidy_output |>
    filter(alarm == TRUE)

  plot <- tidy_output |>
    ggplot(aes(x = week_date)) +
    geom_col(aes(y = observed), alpha = 0.8) +
    geom_line(aes(y = upperbound, colour = "Alarm Threshold"), linetype = "dashed") +
    geom_line(aes(y = expected, colour = "Expected Count"), linetype = "dashed") +
    geom_point(data = alarm_weeks, aes(x = week_date, y = 0, colour = "Alarm Raised")) +
    scale_x_date(date_breaks = "month", date_labels = "%Y-%V") +
    scale_color_manual(values = c("Alarm Threshold" = "#F8766D", "Expected Count" = "#00BA38", "Alarm Raised" = "#F8766D")) +
    facet_wrap(~unit, scales = "free_y", ncol = 4) +
    labs(x = "ISO Week",
         y = "No. Positive Samples",
         title = "Farrington Flexible Abberation Detection Model",
         subtitle = plot_name,
         caption  = "Source: ECOSS") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

  return(plot)
}




