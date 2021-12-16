local({
  library(tidyverse)
  fips_convert <- source("data-raw/cleaners/utils.R")$value$fips_convert
  get_unemployment <- function() {
    temp <- paste0(tempfile(), ".xlsx")
    download.file(
      "https://www.bls.gov/lau/laucnty18.xlsx",
      temp,
      quiet = TRUE
    )
    readxl::read_excel(temp, skip = 5, col_names = c(
      "x", "fips1", "fips2", "name", "year",
      "na", "na2", "na3", "na4", "unemp"
    )) %>%
      drop_na(fips1, fips2) %>%
      mutate(fips = paste0(fips1, fips2)) %>%
      select(fips, unemp)
  }

  get_mhi <- function() {
    temp <- paste0(tempfile(), ".xls")
    download.file(
      "https://www2.census.gov/programs-surveys/saipe/datasets/2017/2017-state-and-county/est17all.xls", # nolint
      temp,
      quiet = TRUE
    )
    readxl::read_excel(temp, skip = 3) %>%
      select(
        sfips = starts_with("State FIPS"),
        cfips = starts_with("County FIPS"),
        mhi = `Median Household Income`
      ) %>%
      transmute(
        fips = paste0(sfips, cfips),
        mhi = as.numeric(mhi)
      )
  }

  get_chr <- function() {
    read_csv("data-raw/raw/CHR_TRENDS_CSV_2019.csv",
      col_types = cols(.default = "c")
    ) %>%
      filter(measurename %in%
        c("Air pollution - particulate matter", "Children in poverty")) %>%
      unite("fips", statecode, countycode, sep = "") %>%
      filter(yearspan == 2014) %>%
      select(fips, measurename, rawvalue) %>%
      pivot_wider(names_from = measurename, values_from = rawvalue) %>%
      rename(
        pm25 = `Air pollution - particulate matter`,
        child_poverty = `Children in poverty`
      )
  }

  get_rentburden <- function() {
    read_csv(
      "https://eviction-lab-data-downloads.s3.amazonaws.com/validated/counties/all-counties.csv", # nolint
      col_types = cols(.default = "c", year = "d", rent.burden = "d")
    ) %>%
      filter(year == max(year)) %>%
      select(fips = GEOID, rent_burden = rent.burden)
  }

  get_prescriptions <- function() {
    "https://www.cdc.gov/drugoverdose/rxrate-maps/county2015.html" %>%
      rvest::read_html() %>%
      rvest::html_node("#acc-panel-1 > div > table") %>% # nolint
      rvest::html_table() %>%
      mutate(
        fips = as.character(`County FIPS Code`),
        opioids = as.numeric(`Opioid Dispensing Rate per 100`)
      ) %>%
      fips_convert() %>%
      select(fips, opioids)
  }

  get_chr <- function() {
    "data-raw/raw/2010 County Health Rankings National Data.xlsx" %>%
      readxl::read_excel(sheet = 2, skip = 1, col_types = "text") %>%
      select(
        fips = FIPS, gini = GINI,
        singpar = `% Single-Parent Households`,
        college2000 = `% College`
      ) %>%
      mutate(across(gini:college2000, as.numeric))
  }

  get_census <- function() {
    census_vars <- c(
      "Gini" = "B19083_001",
      "total_pop" = "B01001_001",
      "white" = "B01001A_001"
    )
    tidycensus::get_acs("county", census_vars, year = 2019) %>%
      select(fips = GEOID, variable, estimate) %>%
      pivot_wider(names_from = variable, values_from = estimate)
  }

  get_demographics <- function() {
    list(
      get_census,
      get_chr,
      get_mhi,
      get_rentburden,
      get_prescriptions,
      get_unemployment
    ) %>%
      map(~ suppressMessages(suppressWarnings(.()))) %>%
      reduce(full_join, by = "fips")
  }

  get_demographics
})
