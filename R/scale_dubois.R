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
  return(discrete_scale(aesthetics, "dubois_d", \(n){
    unname(dubois_pal(n))
  }, ...))
}

scale_color_dubois <- function(...) {
  scale_dubois(aesthetics = "color", ...)
}

scale_fill_dubois <- function(...) {
  scale_dubois(aesthetics = "fill", ...)
}