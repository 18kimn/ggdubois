library(tidyverse)

dta <- tibble(
  id = factor(1:5),
  dep_var = rnorm(5, 50, 20),
  heights = cumsum(dep_var)
)

spike <- tibble(
  x = c(5, 6, 7),
  y = c(dta$heights[3], dta$heights[1], dta$heights[3])
)

ggplot() +
  geom_col(
    data = dta,
    aes(0, dep_var, fill = id),
    position = position_stack(reverse = T)
  ) +
  geom_col(
    data = spike,
    width = 0.1,
    position = "dodge",
    aes(0, y), fill = "black"
  ) +
  coord_polar()

# Summary
# the circles are made as width-1 cols stacked on top of each other
# spikes are made as width-0.1 cols that stretch from


# -------
# Plate 22 in the Data Portraits text

StatSpike <- ggproto("StatSpike", Stat,
  required_aes = "x", compute_group = function(data, scales) {

    # Data-wise this is pretty easy, because we just
  }
)

geom_spike <- function(mapping = NULL, data = NULL,
                       na.rm = FALSE, show.legend = NA,
                       stat = "identity",
                       position = "identity",
                       inherit.aes = TRUE, ...) {
  layer(
    geom = GeomSegment, mapping = mapping, data = data, stat = StatSpike,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}