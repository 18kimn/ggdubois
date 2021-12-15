library(tidyverse)
source("data/utils.R")
process_file <- function(filename) {
  read_csv(
    filename,
    progress = FALSE, show_col_types = FALSE
  ) %>%
    row_as_names() %>%
    return()
}

list.files(
  "data/raw/nhgis0008_csv",
  full.names = T, pattern = "\\.csv"
) %>%
  map_dfr(process_file) %>%
  mutate(fips = paste0(`County Code`, `State Code`)) %>%
  rename(
    year = `Data File Year`,
    march_employees = `Total mid-March employees`,
    payroll = `Total annual payroll`,
    establishments = `Total number of establishments`,
  ) %>%
  group_by(fips, year) %>%
  # These columns have some character values in them,
  #   which should be coerced to numeric for data analysis
  mutate(across(c(march_employees:establishments), as.numeric)) %>%
  # Many counties are included multiple times as separate geometries
  # I just add them together
  summarize(
    across(c(march_employees:establishments), ~ sum(., na.rm = T)),
    .groups = "drop"
  ) %>%
  write_csv("data/clean/employees_timeseries.csv")