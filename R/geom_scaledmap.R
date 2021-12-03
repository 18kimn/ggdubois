library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)

StatScaledMap <- ggproto("StatScaledMap", StatSf,
  compute_layer = function(self, data, params, layout) {
    print(params)
    # add coord to the params, so it can be forwarded to compute_group()
    params$coord <- layout$coord
    ggproto_parent(Stat, self)$compute_layer(data, params, layout)
  },
  compute_group = function(self, data, scales, coord) {
    # if (!(class(data$x) %in% c("numeric", "factor"))) {
    #   stop("Only numeric or factor variables allowed for x")
    # }
    # print(data)
    data_list <- lapply(seq(1, 0, by = -0.25), function(scale_factor) {
      newdata <- data
      newdata$geometry <- (
        newdata$geometry - st_centroid(newdata$geometry)
      ) * scale_factor
      newdata$factor <- scale_factor
      return(newdata)
    })
    data <- do.call(rbind, data_list)
    print(data)
    ggproto_parent(StatSf, self)$compute_group(data, scales, coord)
  },
  required_aes = c("geometry", "x")
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

# testing
tester <- function() {
  ct <- tigris::states() %>%
    filter(NAME == "Connecticut")
  map(1:4, function(i) {
    mutate(ct,
      pop = runif(1, 0, 2) * ALAND,
      group = i
    )
  }) %>%
    do.call(rbind, .) %>%
    mutate(group = as.factor(group)) %>%
    ggplot() +
    geom_scaledmap(aes(x = pop))
}