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
library(epitools)


### --- Create dataframes ---

## Create dataframe of FF+MEM flags for each HB each week

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

# Inner join FF alarms and MEM 'above baseline' flags
ILI_alarms_and_flags <- ili_hb_above_baseline %>%
  inner_join(GP_ILI_Nat_HB_FF_alarms, by = c("week_date", "unit"))
ARI_alarms_and_flags <- ari_hb_above_baseline %>%
  inner_join(GP_ARI_Nat_HB_FF_alarms, by = c("week_date", "unit"))

































## Create HSCP and matching HB FF ILI+ARI alarms dataframe for each week

    # Filter all_hscp_data to only include rows that match
    # consultation_type and hscp to explore this week
    HSCP_data_to_explore <- GP_hscp_data

    # Vector of consultation names to explore by hscp
    consultations_hscp <- unique(HSCP_data_to_explore$consultation_type) # replace with list of consultation types that we report on
    names(consultations_hscp) <- consultations_hscp

    # Create list of sts objects for each consultation type
    GP_hscp_sts_list <- map(consultations_hscp, GP_hscp_sts, data = HSCP_data_to_explore)

    # Run Farrington Flexible model -------------------------------------------
    # Calculate range to plot (from the start of the season)
    GP_hscp_seapoch_list <- map(GP_hscp_sts_list, ~ season_epoch(.x, alarm_year = 2024, alarm_week = 40))

    # Run model (Farrington flexible with noufaily adaptation)
    GP_hscp_pc.noufaily <- map(GP_hscp_sts_list, farringtonFlexible, GP_con.noufaily)

    # Tidy HSCP Outputs --------------------------------------------------------
    GP_output_hscp <- map(GP_hscp_pc.noufaily, tidy_outputs)





    # Get all hscp data for ILI and ARI (inc. alarms status)
    GP_ARI_hscp_FF_data <- GP_output_hscp[[1]]
    GP_ILI_hscp_FF_data <- GP_output_hscp[[2]]

    # Get weekly FF alarm status for all ILI hscp data
    GP_ILI_hscp_FF_alarms <- GP_ILI_hscp_FF_data %>%
      select(week_date, unit, alarm)
    GP_ILI_hscp_FF_alarms$alarm <- as.numeric(GP_ILI_hscp_FF_alarms$alarm)
    # Get weekly FF alarm status for all ARI hscp data
    GP_ARI_hscp_FF_alarms <- GP_ARI_hscp_FF_data %>%
      select(week_date, unit, alarm)
    GP_ARI_hscp_FF_alarms$alarm <- as.numeric(GP_ARI_hscp_FF_alarms$alarm)

        # Add HB to hscp alarms
    hscp_HBs <- HB_HSCPs_Postcodes %>%
          select(-Postcode) %>%
          distinct()
        GP_ARI_hscp_FF_alarms <- GP_ARI_hscp_FF_alarms %>%
          left_join(hscp_HBs, by = c("unit" = "HSCP_Name"))
        GP_ILI_hscp_FF_alarms <- GP_ILI_hscp_FF_alarms %>%
          left_join(hscp_HBs, by = c("unit" = "HSCP_Name"))

        # Add HB alarms to hscp alarms to compare
        #remove overall from ARI national and HB alarms
        GP_ARI_HB_FF_alarms <- GP_ARI_Nat_HB_FF_alarms %>%
          filter(unit != "overall") %>%
          mutate(health_board = unit) %>%
          mutate(health_board_alarm = alarm) %>%
          select(-unit, -alarm)
        #remove overall from ILI national and HB alarms
        GP_ILI_HB_FF_alarms <- GP_ILI_Nat_HB_FF_alarms %>%
          filter(unit != "overall") %>%
          mutate(health_board = unit) %>%
          mutate(health_board_alarm = alarm) %>%
          select(-unit, -alarm)

        #add weekly HB alarms to hscp
        GP_ARI_hscp_FF_alarms <- GP_ARI_hscp_FF_alarms %>%
          left_join(GP_ARI_HB_FF_alarms, by = c("week_date", "health_board"))
        GP_ILI_hscp_FF_alarms <- GP_ILI_hscp_FF_alarms %>%
          left_join(GP_ILI_HB_FF_alarms, by = c("week_date", "health_board"))



## Calcultate Odds Ratio of a postcode alarm occouring within a healthboard with an alarm vs a postcode alarm occouring within a healthboard without an alarm

        #Create tables of data
        ARI_postcode_tab <- table(GP_ARI_Postcode_FF_alarms$alarm, GP_ARI_Postcode_FF_alarms$health_board_alarm)
        ARI_postcode_tab

        #Chi squared test
        chisq.test(ARI_postcode_tab)

        #calculate odds ratio
        oddsratio(ARI_postcode_tab)





## Create postcode and matching HB FF ARI alarms dataframe for each week

    # Filter all_hscp_data to only include rows that match
    # consultation_type and postcode to explore this week (ARI only)
    Postcode_data_to_explore <- GP_postcode_data %>%
      filter(consultation_type == "ARI")

        # Vector of consultation names to explore by postcode
        consultations_postcode <- unique(Postcode_data_to_explore$consultation_type) # replace with list of consultation types that we report on
        names(consultations_postcode) <- consultations_postcode

        # Create list of sts objects for each consultation type
        GP_postcode_sts_list <- map(consultations_postcode, GP_postcode_sts, data = Postcode_data_to_explore)

        #Make sure theres enough rows for each postcode to do analysis
        GP_postcode_sts_list <- Filter(function(x) nrow(x@observed) >= 20, GP_postcode_sts_list)

        # Run Farrington Flexible model --------------------------------------------
        # Calculate range to plot (from the start of the season)
        GP_postcode_seapoch_list <- map(GP_postcode_sts_list, ~ season_epoch(.x, alarm_year = 2024, alarm_week = 40))

        # Run model (Farrington flexible with noufaily adaptation)
        GP_postcode_pc.noufaily <- map(GP_postcode_sts_list, farringtonFlexible, GP_postcode_con.noufaily)

        # Tidy Postcode Outputs --------------------------------------------------------
        GP_output_postcode <- map(GP_postcode_pc.noufaily, tidy_outputs)

        # Get all Postcode data for ARI (inc. alarms status)
        GP_ARI_Postcode_FF_data <- GP_output_postcode[[1]]
        # Get weekly FF alarm status for all ARI postcode data
        GP_ARI_Postcode_FF_alarms <- GP_ARI_Postcode_FF_data %>%
          select(week_date, unit, alarm)
        GP_ARI_Postcode_FF_alarms$alarm <- as.numeric(GP_ARI_Postcode_FF_alarms$alarm)

        # Add HB to postcode alarms - DD10, G33, G69, G83  all need 2 rows per week as they are within 2 healthboards
        postcode_HBs <- HB_HSCPs_Postcodes %>%
          select(-HSCP_Name) %>%
          distinct()
        GP_ARI_Postcode_FF_alarms <- GP_ARI_Postcode_FF_alarms %>%
          left_join(postcode_HBs, by = c("unit" = "Postcode"))

        # Add HB alarms to postcode alarms to compare
        #remove overall from ARI national and HB alarms
        GP_ARI_HB_FF_alarms <- GP_ARI_Nat_HB_FF_alarms %>%
          filter(unit != "overall") %>%
          mutate(health_board = unit) %>%
          mutate(health_board_alarm = alarm) %>%
          select(-unit, -alarm)

        #add weekly HB alarms to postcode
        GP_ARI_Postcode_FF_alarms <- GP_ARI_Postcode_FF_alarms %>%
          left_join(GP_ARI_HB_FF_alarms, by = c("week_date", "health_board"))








### --- Run analysis ---

## Cohen’s Kappa checking how well signaling above/below baseline vs alarm/no alarm agree for the same week (Data already 2024/2025 season)
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




## Cohen’s Kappa checking how well signaling above/below baseline vs alarm/no alarm agree for a -/+ 1 week period (Data already 2024/2025 season)
# This is looking at comparing week whether there had been an alarm over a three week period (was there an alarm -/+ 1 week from row being analysed?)
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






## Calculate Odds Ratio of an ARI postcode alarm occurring within a healthboard with an ARI alarm vs an ARI postcode alarm occurring within a healthboard without an ARI alarm

# Make Data 2024/2025 season
start_date <- as.Date("2024-09-30")
end_date   <- as.Date("2025-09-28")

GP_ARI_Postcode_FF_alarms <- GP_ARI_Postcode_FF_alarms %>%
  filter(week_date >= start_date & week_date <= end_date)

#Create tables of data
ARI_postcode_tab <- table(GP_ARI_Postcode_FF_alarms$alarm, GP_ARI_Postcode_FF_alarms$health_board_alarm)
ARI_postcode_tab

#Chi squared test
chisq.test(ARI_postcode_tab)

#calculate odds ratio
oddsratio(ARI_postcode_tab)

## Calculate Odds Ratio of an ARI hscp alarm occurring within a healthboard with an ARI alarm vs an ARI hscp alarm occurring within a healthboard without an ARI alarm

# Make Data 2024/2025 season
start_date <- as.Date("2024-09-30")
end_date   <- as.Date("2025-09-28")

GP_ARI_hscp_FF_alarms <- GP_ARI_hscp_FF_alarms %>%
  filter(week_date >= start_date & week_date <= end_date)

#Create tables of data
ARI_hscp_tab <- table(GP_ARI_hscp_FF_alarms$alarm, GP_ARI_hscp_FF_alarms$health_board_alarm)
ARI_hscp_tab

#Chi squared test
chisq.test(ARI_hscp_tab)

#calculate odds ratio
oddsratio(ARI_hscp_tab)

## Calculate Odds Ratio of an ILI hscp alarm occurring within a healthboard with an ILI alarm vs an ILI hscp alarm occurring within a healthboard without an ILI alarm

# Make Data 2024/2025 season
start_date <- as.Date("2024-09-30")
end_date   <- as.Date("2025-09-28")

GP_ILI_hscp_FF_alarms <- GP_ILI_hscp_FF_alarms %>%
  filter(week_date >= start_date & week_date <= end_date)

#Create tables of data
ILI_hscp_tab <- table(GP_ILI_hscp_FF_alarms$alarm, GP_ILI_hscp_FF_alarms$health_board_alarm)
ILI_hscp_tab

#Chi squared test
chisq.test(ILI_hscp_tab)

#calculate odds ratio
oddsratio(ILI_hscp_tab)










# R code example for a mixed-effects logistic regression model

# Install and load required packages
install.packages(c("lme4", "broom.mixed"))
library(lme4)
library(broom.mixed)

# Example: Your dataframe should have columns:
# HSCP_alarm (binary: 0/1)
# HealthBoard_alarm (binary: 0/1)
# HealthBoard (factor identifying the Health Board)
# HSCP (optional if you want random intercepts for HSCP too)

# Fit mixed-effects logistic regression
model <- glmer(
  HSCP_alarm ~ HealthBoard_alarm + (1 | HealthBoard),
  data = df,
  family = binomial(link = "logit")
)

# View summary
summary(model)

# Extract odds ratio and confidence intervals
model_results <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
print(model_results)

# Optional: Add HSCP random effect if needed
# model2 <- glmer(
#   HSCP_alarm ~ HealthBoard_alarm + (1 | HealthBoard) + (1 | HSCP),
#   data = df,
#   family = binomial(link = "logit")
# )

# Check model fit
glance(model)

# Predict probabilities (optional)













