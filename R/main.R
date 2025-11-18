#' Title: Main Script for running FLAG analysis and producing outputs
#' Author: Matthew Hoyle
#' Date: 14/11/2025
#'
#' Notes:
#' This script runs a series of subscripts which collectively run a farrington
#' flexible aberration detection model and renders an interactive report using
#' quarto.

# Load Data ---------------------------------------------------------------

source("/PHI_conf/Respiratory_Surveillance_General/Matthew_Hoyle/get_ecoss_data.R")

# Keep only Aggregated data objects

rm(list=setdiff(ls(), c("Aggregate_Scot", "Aggregate_HB", "Aggregate_AgeGp")))


# Load functions ----------------------------------------------------------

source(here::here("R/functions.R"))


# Run model ---------------------------------------------------------------

source(here::here("R/run_ff_model.R"))


# Create plots ------------------------------------------------------------

source(here::here("R/create_outputs.R"))


# Save objects for quarto report ------------------------------------------

if (!dir.exists(here::here("data"))) {
  dir.create(here::here("data"))
}

# Only keep Objects used in the report

rm(list=setdiff(ls(),
                c("alarm_weeks", "alarms_this_week", "hb_names", "hb_vec", "pathogens", "hb_pc.noufaily",
                  "scot_pc.noufaily", "historic_alarms", "iplot_list", "output_list",
                  "plot_list", "plot_list_faceted")))


save.image(here::here("data/model_data.rds"))


# Render quarto report ----------------------------------------------------

quarto::quarto_render()
