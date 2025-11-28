#### GP data statistical analysis to explore validity
#### and Positive Predictive Value of FF model








## Cohen’s Kappa analysis
## Create a data frame with two columns:
# weeks, HB (inc. national), +
# MEM_status (0 = below baseline, 1 = above baseline)
# FF_alarm (0 = no alarm, 1 = alarm)

install.packages(c("irr", "psych"))
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




boards <- split(data, data$BoardID)
lapply(boards, function(df) kappa2(df[, c("MEM_status", "FF_alarm")]))












