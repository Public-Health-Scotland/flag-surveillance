
#isolate which healthboards to explore
alarms_to_explore <- GP_alarms_this_week %>%
  filter(unit != "overall") %>%
  mutate(health_board = unit) %>%
  select(consultation_type, health_board)

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






