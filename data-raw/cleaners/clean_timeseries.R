local({
  library(tidyverse)
  row_as_names <- source("data-raw/cleaners/utils.R")$value$row_as_names

  clean_timeseries <- function() {
    dta_long <- read_csv(
      "data-raw/raw/nhgis0007_csv/nhgis0007_ts_nominal_county.csv",
      show_col_types = FALSE
    ) %>%
      row_as_names() %>%
      unite(fips, `FIPS State Code`, `FIPS County Code`, sep = "") %>%
      pivot_longer(`1790: Persons: Total`:last_col(1))

    dta_long %>%
      separate(name,
        into = c("year", "measure", "subgroup"),
        sep = ": ", extra = "merge"
      ) %>%
      filter(
        year != "2008-2012",
        year != "Margin of error",
        year >= "1970",
        !is.na(value)
      ) %>%
      select(fips, year, subgroup, value) %>%
      mutate(value = as.numeric(value)) %>%
      pivot_wider(names_from = "subgroup") %>%
      mutate(
        median_income = Households,
        income_group = ntile(median_income, 3),
        income_group = c("low", "medium", "high")[income_group],
        black = (Total - `White (single race)`) / Total,
        edu = (Total - `25 years and over ~ 4 or more years of college (until 1980) or bachelor's degree or higher (since 1990)`) / Total, # nolint
        owner = `Owner occupied` / (`Owner occupied` + `Renter occupied`),
        workforce = `16 years and over ~ Male ~ In labor force` +
          `16 years and over ~ Female ~ In labor force`,
        unemp = `16 years and over ~ Male ~ In labor force--Civilian--Unemployed` + # nolint
          `16 years and over ~ Female ~ In labor force--Civilian--Unemployed`,
        unemp = unemp / workforce
      ) %>%
      select(
        fips, year, median_income, income_group,
        black, edu, owner, unemp
      )
  }

  clean_timeseries
})