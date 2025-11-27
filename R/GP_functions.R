#############################################
#
# FLAG GP FUNCTIONS
#
#############################################

GP_hb_sts <- function(consultation, data) {

  # Filter the chosen consultation type
  consultation_data <- data |>
    filter(consultation_type == consultation)

  # Create a complete set of weeks for alignment
  all_weeks <- sort(unique(consultation_data$week_date))

  # ---- Counts Matrix ----
  count_df <- consultation_data |>
    select(week_date, health_board, count) |>
    group_by(week_date, health_board) |>
    summarise(count = sum(count), .groups = "drop") |>
    pivot_wider(
      id_cols = week_date,
      names_from = health_board,
      values_from = count,
      values_fill = 0
    ) |>
    complete(week_date = all_weeks, fill = list(count = 0)) # fill missing weeks

  count_matrix <- count_df |> select(-week_date) |> as.matrix()

  # ---- Population Matrix ----
  pop_df <- consultation_data |>
    select(week_date, health_board, pop) |>  # use week_date for alignment
    group_by(week_date, health_board) |>
    summarise(pop = mean(pop), .groups = "drop") |>
    pivot_wider(
      id_cols = week_date,
      names_from = health_board,
      values_from = pop,
      values_fill = 0
    ) |>
    complete(week_date = all_weeks, fill = list(pop = 0)) # align weeks

  pop_matrix <- pop_df |> select(-week_date) |> as.matrix()

  # ---- Diagnostics ----
  message("Dimensions of count_matrix: ", paste(dim(count_matrix), collapse = " x "))
  message("Dimensions of pop_matrix: ", paste(dim(pop_matrix), collapse = " x "))

  if (!all(dim(count_matrix) == dim(pop_matrix))) {
    stop("Mismatch: count_matrix and pop_matrix dimensions differ")
  }

  # ---- Create sts object ----
  consultation_sts <- surveillance::sts(
    observed = count_matrix,
    start = c(min(consultation_data$year), 1),
    frequency = 52,
    epochAsDate = TRUE,
    epoch = as.numeric(all_weeks), # aligned weeks
    population = pop_matrix
  )

  return(consultation_sts)
}

####################################################################################################

GP_hscp_sts <- function(consultation, data) {

  # Filter the chosen consultation type
  consultation_data <- data |>
    filter(consultation_type == consultation)

  # Create a complete set of weeks for alignment
  all_weeks <- sort(unique(consultation_data$week_date))

  # ---- Counts Matrix ----
  count_df <- consultation_data |>
    select(week_date, HSCP_Name, count) |>
    group_by(week_date, HSCP_Name) |>
    summarise(count = sum(count), .groups = "drop") |>
    pivot_wider(
      id_cols = week_date,
      names_from = HSCP_Name,
      values_from = count,
      values_fill = 0
    ) |>
    complete(week_date = all_weeks, fill = list(count = 0)) # fill missing weeks

  count_matrix <- count_df |> select(-week_date) |> as.matrix()

  # ---- Population Matrix ----
  pop_df <- consultation_data |>
    select(week_date, HSCP_Name, pop) |>  # use week_date for alignment
    group_by(week_date, HSCP_Name) |>
    summarise(pop = mean(pop), .groups = "drop") |>
    pivot_wider(
      id_cols = week_date,
      names_from = HSCP_Name,
      values_from = pop,
      values_fill = 0
    ) |>
    complete(week_date = all_weeks, fill = list(pop = 0)) # align weeks

  pop_matrix <- pop_df |> select(-week_date) |> as.matrix()

  # ---- Diagnostics ----
  message("Dimensions of count_matrix: ", paste(dim(count_matrix), collapse = " x "))
  message("Dimensions of pop_matrix: ", paste(dim(pop_matrix), collapse = " x "))

  if (!all(dim(count_matrix) == dim(pop_matrix))) {
    stop("Mismatch: count_matrix and pop_matrix dimensions differ")
  }

  # ---- Create sts object ----
  consultation_sts <- surveillance::sts(
    observed = count_matrix,
    start = c(min(consultation_data$year), 1),
    frequency = 52,
    epochAsDate = TRUE,
    epoch = as.numeric(all_weeks), # aligned weeks
    population = pop_matrix
  )

  return(consultation_sts)
}
####################################################################################################

GP_postcode_sts <- function(consultation, data) {

  # Filter the chosen consultation type
  consultation_data <- data |>
    filter(consultation_type == consultation)

  # Create a complete set of weeks for alignment
  all_weeks <- sort(unique(consultation_data$week_date))

  # ---- Counts Matrix ----
  count_df <- consultation_data |>
    select(week_date, Postcode, count) |>
    group_by(week_date, Postcode) |>
    summarise(count = sum(count), .groups = "drop") |>
    pivot_wider(
      id_cols = week_date,
      names_from = Postcode,
      values_from = count,
      values_fill = 0
    ) |>
    complete(week_date = all_weeks, fill = list(count = 0)) # fill missing weeks

  count_matrix <- count_df |> select(-week_date) |> as.matrix()

  # ---- Population Matrix ----
  pop_df <- consultation_data |>
    select(week_date, Postcode, pop) |>  # use week_date for alignment
    group_by(week_date, Postcode) |>
    summarise(pop = mean(pop), .groups = "drop") |>
    pivot_wider(
      id_cols = week_date,
      names_from = Postcode,
      values_from = pop,
      values_fill = 0
    ) |>
    complete(week_date = all_weeks, fill = list(pop = 0)) # align weeks

  pop_matrix <- pop_df |> select(-week_date) |> as.matrix()

  # ---- Diagnostics ----
  message("Dimensions of count_matrix: ", paste(dim(count_matrix), collapse = " x "))
  message("Dimensions of pop_matrix: ", paste(dim(pop_matrix), collapse = " x "))

  if (!all(dim(count_matrix) == dim(pop_matrix))) {
    stop("Mismatch: count_matrix and pop_matrix dimensions differ")
  }

  # ---- Create sts object ----
  consultation_sts <- surveillance::sts(
    observed = count_matrix,
    start = c(min(consultation_data$year), 1),
    frequency = 52,
    epochAsDate = TRUE,
    epoch = as.numeric(all_weeks), # aligned weeks
    population = pop_matrix
  )

  return(consultation_sts)
}

####################################################################################################

season_epoch <- function(sts, alarm_year = 2024, alarm_week = 40) {
  iso_info <- isoWeekYear(epoch(sts))

  # Select weeks for alarm detection starting at ISO week 40 of 2024
  which(iso_info$ISOYear > alarm_year |
          (iso_info$ISOYear == alarm_year & iso_info$ISOWeek >= alarm_week))
}

####################################################################################################

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

####################################################################################################

GP_gg_outbreak <- function(group = "overall", tidy_output){

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
         y = "No. Consultations",
         caption  = "Source: PHS") +
    theme(legend.position = "bottom", # Corrected legend position
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

  return(plot)
}

####################################################################################################

GP_gg_outbreak_facet <- function(tidy_output, plot_name = NULL){

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
         y = "No. Consultations",
         title = "Farrington Flexible Abberation Detection Model",
         subtitle = plot_name,
         caption  = "Source: PHS") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

  return(plot)
}



