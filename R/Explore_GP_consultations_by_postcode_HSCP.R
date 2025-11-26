
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

###...Explore HSCP alarm data...................................................

# Filter all_hscp_data to only include rows that match
# consultation_type and hscp to explore this week
HSCP_data_to_explore <- GP_hscp_data %>%
  semi_join(HSCP_alarms_to_explore, by = c("consultation_type", "HSCP_Name"))








