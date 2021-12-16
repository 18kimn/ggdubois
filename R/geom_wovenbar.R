library(ggplot2)
# plan:
# receive a parameter (xmax or maxwidth or something)
# and two aesthetics (x and y)

StatWrappedBar <- ggproto("StatWrappedBar", Stat,
  compute_group = function(self, data, scales) {
    print(data)

    return(data)
  }
)
# add
# modify the rectangle to wrap around (y %/% maxwidth) times
# the remainder is y %% maxwidth
# the overhang will be rendered as a circle grob that connects the rectangles
# use rectGrob to render rectangles as appropriate
#
GeomWrappedBar <- ggproto("GeomWrappedBar", GeomBar,
  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)

    # add on extra rows to the data:
    # should be
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    data <- transform(data,
      ymin = pmin(y, 0), ymax = pmax(y, 0),
      xmin = x - width / 2, xmax = x + width / 2,
      width = NULL
    )
    flip_data(data, params$flipped_aes)
  },
  draw_panel = function(self, data, panel_params, coord, max_height) {
    geom_rect_grob <- function(data, panel_params, coord) {
      ggproto_parent(GeomRect, self)$draw_panel(
        data,
        panel_params,
        coord,
        lineend = "butt",
        linejoin = "mitre"
      )
    }
    coords <- coord$transform(data, panel_params)

    # if max_height not specified, calculate it as the second-largest
    #   item in the given data
    if (is.null(max_height)) {
      max_height <- 1.1 * sort(data$y, T)[2]
    }
    # produce # times to wrap and the leftover segment
    times_to_wrap <- max(data$y) %/% max_height
    leftover <- max(data$y) %% max_height
    going_right <- TRUE
    grobs <- list()
    for (i in 1:times_to_wrap) {
      grobs[[i]] <- rectGrob(
        coords$xmin, ifelse(going_right, coords$xmax),
      )
      #  rectGrob(
      #         coords$xmin, coords$ymax,
      #         width = coords$xmax - coords$xmin,
      #         height = coords$ymax - coords$ymin,
      #         default.units = "native",
      #         just = c("left", "top"),
      #         gp = gpar(
      #           col = coords$colour,
      #           fill = alpha(coords$fill, coords$alpha),
      #           lwd = coords$size * .pt,
      #           lty = coords$linetype,
      #           linejoin = linejoin,
      #           lineend = lineend
      #         )
      #       ))
      going_right <- !going_right
    }

    # produce circle

    # potentially break

    # produce bar going left

    # produce circle

    # potentially break
    zeroGrob()
  }

  # produce last bar

  # return glist
)

geom_wrappedbar <- function(mapping = NULL, data = NULL,
                            ...,
                            max_height = NULL,
                            width = NULL,
                            na.rm = FALSE,
                            orientation = NA,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatWrappedBar,
    geom = GeomWrappedBar,
    position = "dodge",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      max_height = max_height,
      ...
    )
  )
}

