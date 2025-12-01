library(dplyr)
library(forcats)
library(ggplot2)
library(glue)
library(lubridate)
library(mem)
library(openxlsx)
library(purrr)
library(stringr)
library(tidyr)
library(readr)



#### Read in ILI data

season_linetypes <- c(5:1 ,1)

season_thickness <- c(0.5, 0.5, 0.5, 0.5, 0.5, 1.1)

source("//PHI_conf/Respiratory_Surveillance_Viral/GP L8/04 Analysis/Season 2025-26/ILI/R/functions.R")

### Source file locations ###
# location of current source files
current_data_loc <-
  '//PHI_conf/Respiratory_Surveillance_Viral/GP L8/03 Data/Season 2025-26/'

# location of historic Scotland level data
hist_scot_data_loc <-
  '//PHI_conf/Respiratory_Surveillance_Viral/GP L8/03 Data/ILI_master_files/masterfile_HPA_towk342017.xlsx'

# location of historic Health Board level data
hist_hb_data_loc <-
  '//PHI_conf/Respiratory_Surveillance_Viral/GP L8/03 Data/ILI_master_files/masterfile_ILI_HB_towk342017.xlsx'

# Calculate new thresholds?

new_thresholds <- FALSE

### MEM Threshold Calculation constants ###

# Seasons to use in calculation of MEM thresholds
threshold_seasons <- c("2017/2018",
                       "2018/2019",
                       "2022/2023",
                       "2023/2024",
                       "2024/2025")

# Pandemic Season Start (isoweek)
pandemic_start_week <- 40

# Pandemic Season End (isoweek)
pandemic_end_week <- 20

### Common output constants

# Start all plots at week (isoweek)
plot_from_week <- 40

# End all plots at week (isoweek)
plot_to_week = lubridate::isoweek(Sys.Date()) - 1

# Plot week 53
include_week_53 = FALSE

### Frequently used Line Chart MEM constants ###

# Which seasons to plot
line_seasons <- c('2020/2021',
                  '2021/2022',
                  '2022/2023',
                  '2023/2024',
                  '2024/2025',
                  '2025/2026')

### Frequently used multiple variable Heat Chart MEM constants ###

# Which season to plot
heat1_season = c("2024/2025", "2025/2026")

### Frequently used single variable Heat Chart MEM constants ###

# Which seasons to place on y-axis
heat2_seasons = c('2011/2012',
                  '2012/2013',
                  '2013/2014',
                  '2014/2015',
                  '2015/2016',
                  '2016/2017',
                  '2017/2018',
                  '2018/2019',
                  '2019/2020',
                  '2020/2021',
                  '2021/2022',
                  '2022/2023',
                  '2023/2024',
                  '2024/2025',
                  '2025/2026')

# Figure captions
national_figure_caption <-
  paste("\n\nSource: PHS\nData up to week", caption_date)
age_HB_figure_caption <-
  paste(
    "<br><br>
  Source: PHS
  <br> Data up to week", caption_date)
### Processing ###

source("//PHI_conf/Respiratory_Surveillance_Viral/GP L8/04 Analysis/Season 2025-26/ILI/R/import.R")
source("//PHI_conf/Respiratory_Surveillance_Viral/GP L8/04 Analysis/Season 2025-26/ILI/R/tidy_data.R")
source("//PHI_conf/Respiratory_Surveillance_Viral/GP L8/04 Analysis/Season 2025-26/ILI/R/mem_models.R")


### Create Outputs ###

if(isFALSE(all(names(scot_tidy_list) == names(mem_models)))) {
  stop("scot_tidy_list & mem_models mismatch")
}

# Number of seasons to plot
if (length(line_seasons) == length(season_linetypes)) {
  n_seasons_to_plot <- rep(c(1), each = length(line_seasons))
} else {
  stop ("Cannot plot line graph. The number of seasons specified does not match the number of season linetypes. Please check the variables 'line_seasons' and 'line_colours'.")
}

chart_inputs <- purrr::map2 (
  .x = scot_tidy_list,
  .y = mem_models,
  .f = ~
    CreateChartInput (
      .x,
      .y,
      i.season_start_week = plot_from_week
    )
)

temp_ili_hb <- list("overall" = chart_inputs$Rate.ILI.Tot,
                    "AA" = chart_inputs$AA,
                    "BR" = chart_inputs$BR,
                    "DG" = chart_inputs$DG,
                    "FF" = chart_inputs$FF,
                    "FV" = chart_inputs$FV,
                    "GC" = chart_inputs$GGC,
                    "GR" = chart_inputs$GR,
                    "HG" = chart_inputs$HG,
                    "LN" = chart_inputs$LN,
                    "LO" = chart_inputs$LO,
                    "OR" = chart_inputs$OR,
                    "SH" = chart_inputs$SH,
                    "TY" = chart_inputs$TY,
                    "WI" = chart_inputs$WI)

temp_ili_hb <- purrr::map_df(temp_ili_hb, ~as.data.frame(.x), .id="id")

temp_ili_hb$description <- temp_ili_hb$id

ili_hb_above_baseline <- temp_ili_hb %>%
  filter(season == "2024/2025") %>%
  mutate(activity_flag = if_else(activity_level == "Baseline", 0, 1)) %>%
  rename(
    ISOyear = isoyear,
    ISOweek = isoweek) %>%
  left_join(date_reference, by = c("ISOyear", "ISOweek")) %>%
  select(ISOweek_beginning, ISOyear, ISOweek, description, activity_flag) %>%
  distinct() %>%
  rename(week_date = ISOweek_beginning, unit = description)







#### Read in ARI data



















# Load functions
source("//PHI_conf/Respiratory_Surveillance_Viral/GP L8/04 Analysis/Season 2025-26/ARI/R/functions.R")

### Source file locations ###
# location of current source files
current_data_loc <-
  '//PHI_conf/Respiratory_Surveillance_Viral/GP L8/03 Data/Season 2025-26/'

# location of historic Scotland level data
hist_scot_data_loc <-
  '//PHI_conf/Respiratory_Surveillance_Viral/GP L8/03 Data/ILI_master_files/masterfile_HPA_towk342017.xlsx'

# location of historic Health Board level data
hist_hb_data_loc <-
  '//PHI_conf/Respiratory_Surveillance_Viral/GP L8/03 Data/ILI_master_files/masterfile_ARI_HB_towk342017.xlsx'

# Calculate new thresholds?

new_thresholds <- FALSE

### MEM Threshold Calculation constants ###

# Seasons to use in calculation of MEM thresholds
threshold_seasons <- c('2017/2018',
                       '2018/2019',
                       '2022/2023',
                       '2023/2024',
                       '2024/2025')

# Threshold method type
threshold_method = 1 # Arithmetic mean and mean confidence interval

use_t_value = TRUE # Use the t-value over z-value for confidence intervals that rely on t/z-values

# Pandemic Season Start (isoweek)
pandemic_start_week <- 40

# Pandemic Season End (isoweek)
pandemic_end_week <- 20

### Common output constants

# Start all plots at week (isoweek)
plot_from_week <- 40

# End all plots at week (isoweek)
plot_to_week = lubridate::isoweek(Sys.Date()) - 1

# Plot week 53
include_week_53 = FALSE

### Frequently used Line Chart MEM constants ###

# Which seasons to plot
line_seasons <- c('2020/2021','2021/2022', '2022/2023', '2023/2024', '2024/2025', '2025/2026')

season_linetypes <- c(5:1 ,1)

season_thickness <- c(0.5, 0.5, 0.5, 0.5, 0.5, 1.1)

### Frequently used multiple variable Heat Chart MEM constants ###

# Which season to plot
heat1_season = c('2024/2025', '2025/2026')

### Frequently used single variable Heat Chart MEM constants ###

# Which seasons to place on y-axis
heat2_seasons = c('2011/2012',
                  '2012/2013',
                  '2013/2014',
                  '2014/2015',
                  '2015/2016',
                  '2016/2017',
                  '2017/2018',
                  '2018/2019',
                  '2019/2020',
                  '2020/2021',
                  '2021/2022',
                  '2022/2023',
                  '2023/2024',
                  '2024/2025',
                  '2025/2026')

# Figure captions
national_figure_caption <- "\n\nSource: PHS\nData up to week"
age_HB_figure_caption <- "<br><br>Source: PHS<br>Data up to week"

### Processing ###

source("//PHI_conf/Respiratory_Surveillance_Viral/GP L8/04 Analysis/Season 2025-26/ARI/R/import.R")
source("//PHI_conf/Respiratory_Surveillance_Viral/GP L8/04 Analysis/Season 2025-26/ARI/R/tidy_data.R")
source("//PHI_conf/Respiratory_Surveillance_Viral/GP L8/04 Analysis/Season 2025-26/ARI/R/mem_models.R")

### Create Outputs ###

if(isFALSE(all(names(scot_tidy_list) == names(mem_models)))) {
  stop("scot_tidy_list & mem_models mismatch")
}

# Number of seasons to plot
if (length(line_seasons) == length(season_linetypes)) {
  n_seasons_to_plot <- rep(c(1), each = length(season_linetypes))
} else {
  stop ("Cannot plot line graph. The number of seasons specified does not match the number of line colours. Please check the variables 'line_seasons' and 'line_colours'.")
}

chart_inputs <- purrr::map2 (
  .x = scot_tidy_list,
  .y = mem_models,
  .f = ~
    CreateChartInput (
      .x,
      .y,
      i.season_start_week = plot_from_week
    )
)

temp_ari_hb <- list("overall" = chart_inputs$Rate.ARI.Tot,
                    "AA" = chart_inputs$AA,
                    "BR" = chart_inputs$BR,
                    "DG" = chart_inputs$DG,
                    "FF" = chart_inputs$FF,
                    "FV" = chart_inputs$FV,
                    "GC" = chart_inputs$GGC,
                    "GR" = chart_inputs$GR,
                    "HG" = chart_inputs$HG,
                    "LN" = chart_inputs$LN,
                    "LO" = chart_inputs$LO,
                    "OR" = chart_inputs$OR,
                    "SH" = chart_inputs$SH,
                    "TY" = chart_inputs$TY,
                    "WI" = chart_inputs$WI)

temp_ari_hb <- purrr::map_df(temp_ari_hb, ~as.data.frame(.x), .id="id")

temp_ari_hb$description <- temp_ari_hb$id

ari_hb_above_baseline <- temp_ari_hb %>%
  filter(season == "2024/2025") %>%
  mutate(activity_flag = if_else(activity_level == "Baseline", 0, 1)) %>%
  rename(
    ISOyear = isoyear,
    ISOweek = isoweek) %>%
  left_join(date_reference, by = c("ISOyear", "ISOweek")) %>%
  select(ISOweek_beginning, ISOyear, ISOweek, description, activity_flag) %>%
  distinct() %>%
  rename(week_date = ISOweek_beginning, unit = description)


