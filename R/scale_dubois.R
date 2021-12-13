scale_dubois <- function(aesthetics, type = "divergent", ...) {
  if (!(length(type) == 1 && type %in% c("sequential", "divergent"))) {
    stop("Type should be just one of 'sequential' or 'divergent'.")
  }
  if (type == "continuous") {
    return(continuous_scale(
      aesthetics, "dubois_c",
      gradient_n_pal(dubois_sequential(6, ...))
    ))
  }
  return(discrete_scale(aesthetics, "dubois_d", dubois_pal(), ...))
}

scale_color_dubois <- function(...) {
  scale_dubois(aesthetics = "color", ...)
}

scale_fill_dubois <- function(...) {
  scale_dubois(aesthetics = "fill", ...)
}


ggplot(georgia, aes(x = median_income, y = high_school_graduates)) +
  geom_point() +
  scale_color_gradientn(colors = dubois_pal(10))


# or the slightly more conveient and extensible syntax of:
ggplot(georgia, aes(x = median_income, y = high_school_graduates)) +
  geom_point() +
  scale_color_dubois()