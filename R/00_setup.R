# 00_setup.R
# Project setup: packages, paths, shared constants
# Source this at the start of any interactive session, or let each script source it.

library(here)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(jsonlite)
library(httr2)
library(ggplot2)
library(scales)
library(patchwork)
library(svglite)
library(knitr)
library(kableExtra)
library(tidytext)
library(forcats)
library(glue)

source(here("R", "functions.R"))

# Paths
RAW <- here("data", "raw")
PROC <- here("data", "processed")
DER <- here("data", "derived")
FIG <- here("figures")

# Study period
STUDY_START <- 1980L
STUDY_END <- 2025L

# Snapshot dates
IBRD_SNAPSHOT <- as.Date("2026-09-30")

cat("Setup complete. Study period:", STUDY_START, "-", STUDY_END, "\n")
