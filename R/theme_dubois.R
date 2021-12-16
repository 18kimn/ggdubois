library(ggplot2)

theme_dubois <- function(base_size = 12, base_family = "Montserrat",
                         show_grid = FALSE, show_border = FALSE,
                         style = "canvas") {
  colors <- c(dubois_pal(), "background" = "#F5E3CB", "text" = "#2A1206")

  # Fonts can be a pain, so some checking is done to help users out
  available_fonts <- c(names(pdfFonts()), names(postscriptFonts()))
  if (length(find.package("sysfonts", quiet = T)) > 0) {
    available_fonts <- c(
      available_fonts,
      sysfonts::font_families()
    )
  }
  if (!(base_family %in% available_fonts)) {
    # using commas as non-formatted line breaks lol
    warning(
      "Font family ", base_family, " was not found by `ggdubois`.",
      "You can try to install Montserrat (this theme's default) ",
      "with sysfonts::font_add_google('Montserrat') and make it ",
      "usable with showtext::showtext_auto(), ",
      "or supply a different font to `theme_dubois`.",
      " Falling back now to your system's default sans font."
    )
    base_family <- "sans"
  }

  # slightly obtuse syntax is needed for conditional grid/border
  panel_grid <- ifelse(show_grid,
    function(x) element_line(color = "#dda58c"),
    element_blank
  )
  border <- ifelse(show_border,
    function(x) element_rect(size = 0.5, fill = NA, color = colors["text"]),
    element_blank
  )
  if (style == "regular") {
    t <- theme_bw(base_size = base_size, base_family = base_family) +
      theme(
        text = element_text(color = colors["text"]),
        line = element_line(color = colors["text"], ),
        title = element_text(color = colors["text"], face = "bold"),
        rect = element_rect(
          fill = colors["background"],
          color = colors["text"]
        ),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.subtitle = element_text(face = "plain"),
        panel.grid.major = panel_grid(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(
          fill = colors["background"],
          color = if (!show_border) NA
        ),
        panel.background = element_rect(
          fill = colors["background"],
          color = NA,
        ),
        panel.border = border(),
        legend.key = element_rect(fill = colors["background"])
      )
  } else if (style == "canvas") {
    t <- theme_void(base_size, base_family = base_family) +
      theme(
        plot.title = element_text(
          face = "bold",
          size = rel(1.5), margin = margin(5, 5, 5, 5)
        ),
        plot.subtitle = element_text(
          size = rel(1.2),
          margin = margin(5, 5, 5, 5)
        ),
        plot.background = element_rect(
          fill = colors["background"], color = NA,
        ),
        plot.margin = margin(10, 10, 10, 10),
        panel.background = element_rect(fill = NA, color = NA),
        # strips are still useful so inherit that from theme_grey
        # (ggplot2 defaul theme)
        strip.background = element_rect(fill = "grey85", size = rel(0.8)),
        strip.text = element_text(margin = margin(4.4, 4.4, 4.4, 4.4))
      )
  }
  return(t)
}