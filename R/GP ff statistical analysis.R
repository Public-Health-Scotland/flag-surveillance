#### GP data statistical analysis to explore validity
#### and Positive Predictive Value of FF model








## Cohen’s Kappa analysis
## Create a data frame with two columns:
# weeks, HB (inc. national), +
# MEM_status (0 = below baseline, 1 = above baseline)
# FF_alarm (0 = no alarm, 1 = alarm)

#install.packages(c("irr", "psych"))
library(irr)
library(psych)

# Get all HB and national data for ILI and ARI (inc. alarms status)
GP_ILI_Nat_HB_FF_data <- GP_output_list[[1]]
GP_ARI_Nat_HB_FF_data <- GP_output_list[[2]]

# Get weekly FF alarm status for all ILI HB and national data
GP_ILI_Nat_HB_FF_alarms <- GP_ILI_Nat_HB_FF_data %>%
  select(week_date, unit, alarm)
GP_ILI_Nat_HB_FF_alarms$alarm <- as.numeric(GP_ILI_Nat_HB_FF_alarms$alarm)
# Get weekly FF alarm status for all ARI HB and national data
GP_ARI_Nat_HB_FF_alarms <- GP_ARI_Nat_HB_FF_data %>%
  select(week_date, unit, alarm)
GP_ARI_Nat_HB_FF_alarms$alarm <- as.numeric(GP_ARI_Nat_HB_FF_alarms$alarm)

## To-do:
# -- Combine with data for if MEM above baseline for ILI or ARI on that week

# Inner join FF alarms and MEM 'above baseline' flags
ILI_alarms_and_flags <- ili_hb_above_baseline %>%
  inner_join(GP_ILI_Nat_HB_FF_alarms, by = c("week_date", "unit"))
ARI_alarms_and_flags <- ari_hb_above_baseline %>%
  inner_join(GP_ARI_Nat_HB_FF_alarms, by = c("week_date", "unit"))


# Cohen’s Kappa checking how well signaling above/below baseline vs
# alarm/no alarm agree for the same week
# This is looking at comparing the same week
ili_results <- ILI_alarms_and_flags %>%
  group_by(unit) %>%
  summarise(
    kappa = kappa2(cbind(activity_flag, alarm))$value,
    n_weeks = n(),
    .groups = "drop"
  )
print(ili_results)
ari_results <- ARI_alarms_and_flags %>%
  group_by(unit) %>%
  summarise(
    kappa = kappa2(cbind(activity_flag, alarm))$value,
    n_weeks = n(),
    .groups = "drop"
  )
print(ari_results)




# Cohen’s Kappa checking how well signaling above/below baseline vs
# alarm/no alarm agree for a -/+ 1 week period
# This is looking at comparing week whether there had been an alarm over
# a three week period (was there an alarm -/+ 1 week from row being analysed?)
ARI_alarms_and_flags_weekblend <- ARI_alarms_and_flags %>%
  group_by(unit) %>%
  arrange(week_date) %>%
  mutate(agreement_window = if_else(alarm == 1 | lead(alarm) == 1 | lag(alarm) == 1,
    1, 0
  ))

ari_weekblend_results <- ARI_alarms_and_flags_weekblend %>%
  group_by(unit) %>%
  summarise(
    kappa = kappa2(cbind(activity_flag, agreement_window))$value,
    n_weeks = n(),
    .groups = "drop"
  )
print(ari_weekblend_results)

ILI_alarms_and_flags_weekblend <- ILI_alarms_and_flags %>%
  group_by(unit) %>%
  arrange(week_date) %>%
  mutate(agreement_window = if_else(alarm == 1 | lead(alarm) == 1 | lag(alarm) == 1,
                                    1, 0
  ))

ili_weekblend_results <- ILI_alarms_and_flags_weekblend %>%
  group_by(unit) %>%
  summarise(
    kappa = kappa2(cbind(activity_flag, agreement_window))$value,
    n_weeks = n(),
    .groups = "drop"
  )
print(ili_weekblend_results)








