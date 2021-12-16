local({
  row_as_names <- function(dta, n = 1, exclude = c()) {
    dta_keep <- select(dta, {{ exclude }}) %>% slice(-1)
    dta <- select(dta, -{{ exclude }})

    # descriptive names are in the nth row
    new_names <- dta %>%
      slice(n) %>%
      unlist()

    # they should be substituted in for the existing names
    # and potentially bound to dta_keep
    dta %>%
      rename_with(~ new_names[.]) %>%
      slice(-n) %>%
      bind_cols(dta_keep, .)
  }

  fips_convert <- function(dta) {
    dta %>% mutate(fips = ifelse(
      nchar(fips) == 4,
      paste0("0", fips),
      as.character(fips)
    ))
  }

  list(
    row_as_names = row_as_names,
    fips_convert = fips_convert
  )
})