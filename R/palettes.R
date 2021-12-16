# much of the syntax and signature is taken from viridis, viridisLite,
# and RColorBrewer
dubois_sequential <- function(n, begin = 0, end = 1, direction = 1) {
  if (begin < 0 | begin > 1 | end < 0 | end > 1) {
    stop("begin and end must be in [0,1]")
  }
  if (abs(direction) != 1) {
    stop("direction must be 1 or -1")
  }
  if (n == 0) {
    return(character(0))
  }
  if (direction == -1) {
    tmp <- begin
    begin <- end
    end <- tmp
  }
  sequential_pal <- c(
    "#131514", "#31241B", "#59371C", "#8F623B",
    "#FDC111", "#F9E3AA", "#F9EFE3"
  )
  sequential_generator <- scales:::gradient_n_pal(sequential_pal)
  sequential_generator(seq(begin, end, length.out = n))
}

dubois_divergent <- c(
  "black" = "#000000",
  "red" = "#dc143c",
  "green" = "#00aa00",
  "brown" = "#654321",
  "tan" = "#d2b48c",
  "gold" = "#ffd700",
  "pink" = "#ffc0cb",
  "blue" = "#4682b4",
  "background" = "#e4d2c1"
)


dubois_pal <- function(n = length(dubois_divergent), type = "divergent", ...) {
  if (!(length(type) == 1 && type %in% c("divergent", "sequential"))) {
    stop("`type` should be just one of divergent and sequential")
  }
  if (type == "divergent" && length(list(...)) >= 1) {
    warning(
      "Extra parameters supplied in '...' have no effect ",
      "if type == 'divergent'."
    )
  }
  if (n > length(dubois_divergent) && type == "divergent") {
    warning(
      "The ggdubois divergent palette can only supply up to 8 colors; ",
      "you asked for ", n, ". NA values will be included in the palette."
    )
  }

  if (type == "divergent") {
    return(dubois_divergent[1:n])
  }
  return(dubois_sequential(n, ...))
}