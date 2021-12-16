#' @import ggplot2

spike_maker <- function(xmin, xmax, ymin, ymax) {
  slope <- (ymax - ymin) / ((xmax - xmin) / 2)
  x_trans <- (xmax + xmin) / 2
  calc_point <- function(x) {
    return(
      (slope * (abs(x - x_trans))) + ymin +
        rnorm(1, 0, (ymax - ymin) / 50)
    )
  }
  map_dfr(1:100, function(t) {
    true_t <- xmin + ((xmax - xmin) / 100) * t
    tibble(x = true_t, y = calc_point(true_t))
  })
}


trace_rect <- function(xmin, xmax, ymin, ymax) {
  jitterer <- function() {
    rnorm(100, 0, (ymax - ymin) / 250)
  }
  # go up
  up <- map_dfr(1:100, function(i) {
    tibble(x = xmin, y = (ymax - ymin) * i / 100 + ymin)
  }) + jitterer()
  # go right
  right <- map_dfr(1:100, function(i) {
    tibble(x = (xmax - xmin) * i / 100 + xmin, y = ymax)
  }) + jitterer()

  # go down
  down <- map_dfr(1:100, function(i) {
    tibble(x = xmax, y = (ymin - ymax) * i / 100 + ymax)
  }) + jitterer()

  # go left
  left <- map_dfr(1:100, function(i) {
    tibble(x = (xmin - xmax) * i / 100 + xmax, y = ymin)
  }) + jitterer()

  bind_rows(up, right, down, left)
}



# the circles are made as width-1 cols stacked on top of each other
# spikes are made as width-0.1 cols that stretch from


# -------
# Plate 22 in the Data Portraits text

StatSpike <- ggproto("StatSpike", Stat,
  required_aes = c("x", "y"),
  compute_panel = function(self, data, scales, ...) {
    print(head(data))

    ggproto_parent(Stat, self)$compute_panel(data, scales, ...)
  },
  compute_group = function(self, data, scales) {
    return(data)
  }
)

geom_spike <- function(mapping = NULL, data = NULL,
                       na.rm = FALSE, show.legend = NA,
                       stat = "identity",
                       position = "identity",
                       inherit.aes = TRUE, ...) {
  layer(
    geom = GeomSegment, mapping = mapping, data = data,
    stat = StatSpike, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...)
  )
}