GeomPolygon <- ggproto("GeomPolygon", Geom,
  draw_panel = function(data, panel_params, coord, rule = "evenodd") {
    print(as_tibble(data))
    n <- nrow(data)
    if (n == 1) {
      return(zeroGrob())
    }

    munched <- coord_munch(coord, data, panel_params)

    if (is.null(munched$subgroup)) {
      # Sort by group to make sure that colors, fill, etc. come in same order
      munched <- munched[order(munched$group), ]

      # For gpar(), there is one entry per polygon (not one entry per point).
      # We'll pull the first value from each group, and assume all these values
      # are the same within each group.
      first_idx <- !duplicated(munched$group)
      first_rows <- munched[first_idx, ]

      ggplot2:::ggname(
        "geom_polygon",
        polygonGrob(
          munched$x, munched$y,
          default.units = "native",
          id = munched$group,
          gp = gpar(
            col = first_rows$colour,
            fill = alpha(first_rows$fill, first_rows$alpha),
            lwd = first_rows$size * .pt,
            lty = first_rows$linetype
          )
        )
      )
    } else {
      if (utils::packageVersion("grid") < "3.6") {
        abort("Polygons with holes requires R 3.6 or above")
      }
      # Sort by group to make sure that colors, fill, etc. come in same order
      munched <- munched[order(munched$group, munched$subgroup), ]
      id <- match(munched$subgroup, unique(munched$subgroup))

      # For gpar(), there is one entry per polygon (not one entry per point).
      # We'll pull the first value from each group, and assume all these values
      # are the same within each group.
      first_idx <- !duplicated(munched$group)
      first_rows <- munched[first_idx, ]

      ggname(
        "geom_polygon",
        pathGrob(
          munched$x, munched$y,
          default.units = "native",
          id = id, pathId = munched$group,
          rule = rule,
          gp = gpar(
            col = first_rows$colour,
            fill = alpha(first_rows$fill, first_rows$alpha),
            lwd = first_rows$size * .pt,
            lty = first_rows$linetype
          )
        )
      )
    }
  },
  default_aes = aes(
    colour = NA, fill = "grey20", size = 0.5, linetype = 1,
    alpha = NA, subgroup = NULL
  ),
  handle_na = function(data, params) {
    data
  },
  required_aes = c("x", "y"),
  draw_key = draw_key_polygon
)