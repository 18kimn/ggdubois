
dubois_pal <- function(n = 8, type = "divergent") {
  if (!(length(type) == 1 && type %in% c("divergent", "sequential"))) {
    stop("`type` should be just one of divergent and sequential")
  }
  divergent_pal <- c(
    "black" = "#000000",
    "red" = "#dc143c",
    "green" = "#00aa00",
    "brown" = "#654321",
    "tan" = "#d2b48c",
    "gold" = "#ffd700",
    "pink" = "#ffc0cb",
    "blue" = "#4682b4"
  )
  sequential_pal <- c(
    "#131514", "#31241B", "#59371C", "#8F623B",
    "#FDC111", "#F9E3AA", "#F9EFE3"
  )
  sequential_generator <- scales:::gradient_n_pal(sequential_pal)

  if (type == "divergent") {
    return(divergent_pal[1:n])
  }
  return(sequential_generator(seq(0, 1, length.out = n)))
}


# by default, returns a divergent color scale with the specified amount of colors
dubois_pal(4)
# but can return a sequential color scale
dubois_pal(10, type = "sequential")