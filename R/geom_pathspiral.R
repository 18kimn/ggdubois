#' @import ggplot2

StatPathSpiral <- ggproto("StatPathSpiral", Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales) {
    # Slope is needed to make the spiral curve inward instead of
    #   connecting to itself in a circle
    slope <- -0.2
    max_x <- max(data$y) / 2
    # four stages of loop
    # first curve start coordinates
    # first curve end coordinates
    # second curve start coordinates
    # second curve end coordinates
    y <- seq(5, by = -1.5, length.out = nrow(data))
    first <- data.frame(
      x = 0,
      xend = pmin(data$y, max_x),
      y = y,
      yend = slope * pmin(data$y, max_x) + y,
      PANEL = data$PANEL,
      group = data$group
    )
    xend <- ifelse(data$y < max_x, NA_real_, data$y - max_x)
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
    current <<- rbind(first, second)
    return(rbind(first, second))
  }
)

geom_pathspiral <- function(mapping = NULL, data = NULL,
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
