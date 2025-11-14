#' Title: Create interactive plots from FLAG outputs
#' Author: Matthew Hoyle
#' Date: 01/04/2025
#'
#' Notes:
#' This script runs utilises the {ggiraph} package to create interactive plots
#' from the model outputs



# Load packages -----------------------------------------------------------

pacman::p_load(tidyverse, ggplot2, ggiraph, glue, lubridate)



# Set defaults ----------------------------------------------------------

old <- theme_set(theme_bw(base_family = "Arial"))

tooltip_css <- "background-color:#3F3685;font-family:Arial;color:white;padding:5px;border-radius:3px;"


# Format data -------------------------------------------------------------




# Create plots ------------------------------------------------------------

iplot_list <- map(output_list, \(x) map(hb_vec, ggiraph_outbreak, tidy_output = x))


ggiraph::girafe(ggobj = iplot_list$Adenovirus$overall,
                options = list(
                  opts_tooltip(css = tooltip_css, offx = 15, offy = 15, opacity = 1),
                  opts_sizing(width = .8),
                  opts_hover(css = ''),
                  opts_hover_inv(css ='opacity:0.8;fill:lightgrey;')
                )
)
