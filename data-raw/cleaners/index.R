
local({
  library(tidyverse)
  library(sf)
  get_demographics <- source("data-raw/cleaners/scrape_demographics.R")$value
  demographics <- get_demographics()

  clean_employment <- source("data-raw/cleaners/clean_employment.R")$value
  clean_timeseries <- source("data-raw/cleaners/clean_timeseries.R")$value

  ipums_timeseries <- clean_timeseries()
  ipums_employment <- clean_employment()

  georgia_shps <- read_sf("data-raw/raw/nhgis009_shape") %>%
    filter(str_sub(NHGISST, 1, 2) == "13") %>%
    mutate(fips = paste0(
      str_sub(NHGISST, 1, 2), str_sub(NHGISCTY, 1, 3)
    )) %>%
    select(fips, GISJOIN)
  georgia <- ipums_employment %>%
    filter(
      as.numeric(year) %% 10 == 0,
      str_sub(fips, 1, 2) == "13"
    ) %>%
    select(-fips) %>%
    full_join(georgia_shps, by = c(`GIS Join Match Code` = "GISJOIN")) %>%
    left_join(ipums_timeseries, by = c("fips", "year")) %>%
    filter(!st_is_empty(geometry), ) %>%
    sf::st_as_sf()

  usethis::use_data(demographics, georgia, overwrite = TRUE)
})