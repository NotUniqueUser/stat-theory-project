library(MASS)
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(scales)
library(ggspatial)
library(here)
library(broom)
library(pROC)

pipeline_scripts <- c(
  "001_import.R",
  "002_hypothesis_tests.R",
  "003_plot_map.R",
  "004_plot_tests.R"
)

purrr::walk(pipeline_scripts, \(script) source(here("scripts", script)))