#' Title: Main Script for running FLAG analysis and producing outputs
#' Author: Matthew Hoyle
#' Date: 14/11/2025
#'
#' Notes:
#' This script runs a series of subscripts which collectively run a farrington
#' flexible aberration detection model and renders an interactive report using
#' quarto.


# Load functions ----------------------------------------------------------

source(here::here("R/functions.R"))


# Run model ---------------------------------------------------------------

source(here::here("R/run_FF_model.R"))


# Create plots ------------------------------------------------------------

source(here::here("R/create_plots.R"))


# Save objects for quarto report ------------------------------------------

if (!dir.exists(here::here("data"))) {
  dir.create(here::here("data"))
}

save.image(here::here("data/model_data.rds"))


# Render quarto report ----------------------------------------------------

quarto::quarto_render()
