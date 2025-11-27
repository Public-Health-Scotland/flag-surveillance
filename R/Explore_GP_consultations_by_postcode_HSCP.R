#isolate which healthboards to explore
alarms_to_explore <- GP_alarms_this_week %>%
  filter(unit != "overall") %>%
  mutate(health_board = unit) %>%
  select(consultation_type, health_board)

# Check if both are empty
if (nrow(alarms_to_explore) == 0) {
  message("No ILI or ARI Health Board Alarms in the previous week.")} else {

# Continue with rest of script
cat("At least one of ILI or ARI has Health Board alarms. Continuing...\n")

#isolate HSCP and Postcodes to match with alarm HBs
HB_HSCPs_Postcodes <- all_ILI_ARI_data %>%
  select(Postcode, HSCP_Name, health_board) %>%
  distinct()

#Get all alarms to explore
alarms_to_explore <- alarms_to_explore %>%
  left_join(HB_HSCPs_Postcodes, by = "health_board")

#HSCP alarms to explore
HSCP_alarms_to_explore <- alarms_to_explore %>%
  select(consultation_type, HSCP_Name) %>%
  distinct()

#Postcode alarms to explore
Postcode_alarms_to_explore <- alarms_to_explore %>%
  select(consultation_type, Postcode) %>%
  distinct()

### Explore HSCP alarm data -------------------------------------------------
### -------------------------------------------------------------------------

# Filter all_hscp_data to only include rows that match
# consultation_type and hscp to explore this week
HSCP_data_to_explore <- GP_hscp_data %>%
  semi_join(HSCP_alarms_to_explore, by = c("consultation_type", "HSCP_Name"))

# Vector of consultation names to explore by hscp
consultations_hscp <- unique(HSCP_data_to_explore$consultation_type) # replace with list of consultation types that we report on
names(consultations_hscp) <- consultations_hscp

# Create list of sts objects for each consultation type
GP_hscp_sts_list <- map(consultations_hscp, GP_hscp_sts, data = HSCP_data_to_explore)

# Run Farrington Flexible model -------------------------------------------
## HSCPs ------------------------------------------------------------------

# Calculate range to plot (from the start of the season)
GP_hscp_seapoch_list <- map(GP_hscp_sts_list, ~ season_epoch(.x, alarm_year = 2024, alarm_week = 40))

# Run model (Farrington flexible with noufaily adaptation)
GP_hscp_pc.noufaily <- map(GP_hscp_sts_list, farringtonFlexible, GP_con.noufaily)

# Tidy HSCP Outputs --------------------------------------------------------

GP_output_hscp <- map(GP_hscp_pc.noufaily, tidy_outputs)

# HSCP Summary Tables ------------------------------------------------------

GP_hscp_season_summary <- GP_output_hscp |>
  map(\(x)
      x |>
        group_by(unit) |>
        summarise(total_count = sum(observed),
                  total_alarm = sum(alarm))
  )

GP_hscp_last_5_weeks <- GP_output_hscp |>
  map(\(x)
      x |>
        group_by(unit) |>
        slice_max(week_date, n = 5)
  )

GP_hscp_week_summary <- GP_output_hscp |>
  map(\(x)
      x |>
        group_by(unit) |>
        slice_max(week_date, n = 1)
  )

GP_hscp_alarm_weeks <- GP_hscp_week_summary |>
  map(\(x)
      x |>
        filter(alarm == TRUE)
  ) |>
  list_rbind(names_to = "consultation_type") |>
  mutate(rate = round_half_up(observed/population * 100000, 2))

GP_hscp_alarms_this_week <- GP_hscp_week_summary |>
  map(\(x)
      x |>
        slice_max(week_date) |>
        filter(alarm == TRUE)
  ) |>
  list_rbind(names_to = "consultation_type") |>
  mutate(rate = round_half_up(observed/population * 100000, 2))

GP_hscp_historic_alarms <- GP_output_hscp |>
  bind_rows(.id = "consultation_type") |>
  filter(alarm == TRUE) |>
  mutate(week_date = grates::as_isoweek(week_date),
         rate = round_half_up(observed/population * 100000, 2)) |>
  arrange(week_date)

### Explore Postcode alarm data (ARI only) ----------------------------------
### -------------------------------------------------------------------------

# Filter all_hscp_data to only include rows that match
# consultation_type and postcode to explore this week (ARI only)
Postcode_data_to_explore <- GP_postcode_data %>%
  semi_join(Postcode_alarms_to_explore, by = c("consultation_type", "Postcode")) %>%
  filter(consultation_type == "ARI")

# Check if both are empty
if (nrow(Postcode_data_to_explore) == 0) {
  message("No ARI Health Board Alarms in the previous week. Skipping postcode analysis")} else {

# Vector of consultation names to explore by postcode
consultations_postcode <- unique(Postcode_data_to_explore$consultation_type) # replace with list of consultation types that we report on
names(consultations_postcode) <- consultations_postcode

# Create list of sts objects for each consultation type
GP_postcode_sts_list <- map(consultations_postcode, GP_postcode_sts, data = Postcode_data_to_explore)

# Run Farrington Flexible model --------------------------------------------
## Postcodes ---------------------------------------------------------------

# Calculate range to plot (from the start of the season)
GP_postcode_seapoch_list <- map(GP_postcode_sts_list, ~ season_epoch(.x, alarm_year = 2024, alarm_week = 40))

# Run model (Farrington flexible with noufaily adaptation)
GP_postcode_pc.noufaily <- map(GP_postcode_sts_list, farringtonFlexible, GP_con.noufaily)

# Tidy Postcode Outputs --------------------------------------------------------

GP_output_postcode <- map(GP_postcode_pc.noufaily, tidy_outputs)

# Postcode Summary Tables ------------------------------------------------------

GP_postcode_season_summary <- GP_output_postcode |>
  map(\(x)
      x |>
        group_by(unit) |>
        summarise(total_count = sum(observed),
                  total_alarm = sum(alarm))
  )

GP_postcode_last_5_weeks <- GP_output_postcode |>
  map(\(x)
      x |>
        group_by(unit) |>
        slice_max(week_date, n = 5)
  )

GP_postcode_week_summary <- GP_output_postcode |>
  map(\(x)
      x |>
        group_by(unit) |>
        slice_max(week_date, n = 1)
  )

GP_postcode_alarm_weeks <- GP_postcode_week_summary |>
  map(\(x)
      x |>
        filter(alarm == TRUE)
  ) |>
  list_rbind(names_to = "consultation_type") |>
  mutate(rate = round_half_up(observed/population * 100000, 2))

GP_postcode_alarms_this_week <- GP_postcode_week_summary |>
  map(\(x)
      x |>
        slice_max(week_date) |>
        filter(alarm == TRUE)
  ) |>
  list_rbind(names_to = "consultation_type") |>
  mutate(rate = round_half_up(observed/population * 100000, 2))

GP_postcode_historic_alarms <- GP_output_postcode |>
  bind_rows(.id = "consultation_type") |>
  filter(alarm == TRUE) |>
  mutate(week_date = grates::as_isoweek(week_date),
         rate = round_half_up(observed/population * 100000, 2)) |>
  arrange(week_date)

  } # end to else option which runs Postcode analysis if any ARI HB alarms in past week


  } # end to else option which runs whole script if any ILI or ARI HB alarms in past week


