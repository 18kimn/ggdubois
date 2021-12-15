row_as_names <- function(dta, n = 1) {
  # descriptive names are in the nth row
  new_names <- unlist(dta[n, ])

  # they should be substituted in for the existing names
  dta %>%
    rename_with(~ new_names[.]) %>%
    slice(-n) %>%
    return()
}

fips_convert <- function(dta) {
  dta %>% mutate(fips = ifelse(
    nchar(fips) == 4,
    paste0("0", fips),
    as.character(fips)
  ))
}