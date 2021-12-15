library(tidyverse)
source("data/utils.R")

dta <- read_csv(
  "data/raw/nhgis0007_csv/nhgis0007_ts_nominal_county.csv",
  show_col_types = FALSE
) %>%
  row_as_names() %>%
  drop_na(`FIPS State Code`, `FIPS County Code`)
mutate(fips = paste0(`FIPS State Code`, `FIPS County Code`)) %>%
  pivot_longer(cols = `1790: Persons: Total`:last_col(1)) %>%
  select(fips, name, value)


dta <- dta %>%
  separate(name,
    into = c("year", "measure", "subgroup"),
    sep = ": ", extra = "merge"
  ) %>%
  filter(year != "2008-2012", year != "Margin of error")