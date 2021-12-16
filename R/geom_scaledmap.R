StatScaledMap <- ggproto("StatScaledMap", StatSf,
  compute_layer = function(self, data, params, layout) {
    rescaler <- function(x, newMin = 0.4) {
      (1 - newMin) / (max(x) - min(x)) * (x - min(x)) + newMin
    }
    data$x <- rescaler(data$x)
    if (length(data$PANEL) != 1) {
      data$x <- sort(data$x, decreasing = TRUE)
    }

    data_list <- lapply(
      seq_len(length(data$x)),
      function(i) {
        sub_dta <- data[i, ]
        scale_factor <- data$x[i]
        centroid <- sf::st_centroid(sub_dta$geometry)

        sub_dta$geometry <- (sub_dta$geometry - centroid) *
          scale_factor + centroid
        return(sub_dta)
      }
    )
    data <- do.call(rbind, data_list)

    # Hacky: In order to make facet_wrap() work with this
    # I need to delete "x" as an aesthetic so that the plot doesnt
    # try to scale to x
    # This means we can't use Stat or StatSf's `compute_layer`
    # since it checks for the presence of aesthetics
    data$x <- NULL

    params$coord <- layout$coord
    # Trim off extra parameters
    params <- params[intersect(names(params), self$parameters())]

    args <- c(list(data = quote(data), scales = quote(scales)), params)
    ggplot2:::dapply(data, "PANEL", function(data) {
      scales <- layout$get_scales(data$PANEL[1])
      tryCatch(do.call(self$compute_panel, args))
    })
  },
  required_aes = c("geometry", "x", "group")
)

geom_scaledmap <- function(mapping = NULL, data = NULL,
                           na.rm = FALSE, show.legend = NA,
                           position = "identity",
                           inherit.aes = TRUE, ...) {
  c(
    layer_sf(
      geom = GeomSf, mapping = mapping, data = data,
      stat = StatScaledMap, position = position,
      show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ), coord_sf(default = TRUE)
  )
}