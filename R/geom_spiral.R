library(tidyverse)

dta <- tibble(
  x = 1:5
)

StatSpiral <- ggproto("StatSpiral", Stat,
  required_aes = "x|y",
  compute_group = function(data, scales) {
    # Slope is needed to make the spiral curve inward instead of
    #   connecting to itself in a circle
    slope <- -0.5
    max_x <- max(data$x) / 2

    # four stages of loop
    # first curve start coordinates
    # first curve end coordinates
    # second curve start coordinates
    # second curve end coordinates
    y <- seq(1, to = 0.6, length.out = nrow(data))
    first <- data.frame(
      x = 0,
      xend = unlist(lapply(data$x, min, max_x)),
      y = y,
      yend = slope * unlist(lapply(data$x, min, max_x)) + y,
      PANEL = data$PANEL,
      group = data$group
    )
    xend <- ifelse(data$x < max_x, NA_real_, data$x - max_x)
    second <- data.frame(
      x = 0,
      xend = xend,
      y = first$yend,
      yend = slope * xend + first$yend,
      PANEL = data$PANEL,
      group = data$group
    )
    # SECOND SET
    # x0 <- 0
    # x1 <- ifelse(coords$x < max_x, NA_real_, coords$x - max_x)
    # y0 <- y1
    # y1 <- slope * x1 + y0
    # second_curve <- segmentsGrob(x0, x1, y0, y1)
    return(rbind(first, second))
  }
)

geom_spiral <- function(mapping = NULL, data = NULL,
                        na.rm = FALSE, show.legend = NA,
                        stat = "identity",
                        position = "identity",
                        inherit.aes = TRUE, ...) {
  layer(
    geom = GeomSegment, mapping = mapping, data = data, stat = StatSpiral,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


# cxc <- ggplot(dta, aes(x = 0, xend = x, y = 1, yend = 0.8)) +
#   geom_segment(aes(x = 0, xend = x, y = x, yend = 1)) +
#   geom_segment(aes(x = 0, xend = x, y = 0, yend = 0)) +
#   geom_segment(colour = "black") +
#   coord_polar()
# cxc
library(grid)
# expected usage
# single bar
test <- ggplot(dta, aes(x = x)) +
  geom_spiral() +
  coord_polar(clip = "off")
test

# should take stat="identity" stat="bin"
# want to pass a "maximum" that will be used as 2pi
# i.e. if the maximum is 6 then a bar of 4 will only go 4/6ths of
# the way around the plot