local({
  library(tidyverse)
  row_as_names <- source("data-raw/cleaners/utils.R")$value$row_as_names
  process_file <- function(filename) {
    read_csv(
      filename,
      progress = FALSE, show_col_types = FALSE
    ) %>%
      row_as_names() %>%
      return()
  }

  clean_employment <- function() {
    list.files(
      "data-raw/raw/nhgis0008_csv",
      full.names = TRUE, pattern = "\\.csv"
    ) %>%
      map_dfr(process_file) %>%
      mutate(fips = paste0(`State Code`, `County Code`)) %>%
      rename(
        year = `Data File Year`,
        march_employees = `Total mid-March employees`,
        payroll = `Total annual payroll`,
        establishments = `Total number of establishments`,
      ) %>%
      group_by(`GIS Join Match Code`, fips, year) %>%
      # These columns have some character values in them,
      #   which should be coerced to numeric for data analysis
      mutate(across(c(march_employees:establishments), as.numeric)) %>%
      # Many counties are included multiple times as separate geometries
      # I just add them together
      summarize(
        across(c(march_employees:establishments), ~ sum(., na.rm = TRUE)),
        .groups = "drop"
      )
  }

  clean_employment
})