
local({
  library(tidyverse)
  library(sf)
  get_demographics <- source("data-raw/cleaners/scrape_demographics.R")$value
  demographics <- get_demographics()

  clean_employment <- source("data-raw/cleaners/clean_employment.R")$value
  clean_timeseries <- source("data-raw/cleaners/clean_timeseries.R")$value

  ipums_timeseries <- clean_timeseries()
  ipums_employment <- clean_employment()

  georgia_shps <- tigris::counties("13") %>%
    select(fips = GEOID)
  georgia <- ipums_employment %>%
    filter(
      as.numeric(year) %% 10 == 0,
      str_sub(fips, 1, 2) == "13"
    ) %>%
    full_join(ipums_timeseries, by = c("fips", "year")) %>%
    full_join(georgia_shps, by = "fips") %>%
    sf::st_as_sf()

  usethis::use_data(demographics, georgia, overwrite = TRUE)
})