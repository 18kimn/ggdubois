library(ggplot2)
library(ggthemes)
library(showtext)

theme_dubois <- function(base_size = 12, base_family = "Montserrat",
                         show_grid = F, show_border = F) {
  colors <- c(dubois_pal(), "background" = "#EDDCCC", "text" = "#2A1206")

  # Fonts can be a pain, so some checking is done to help users out
  available_fonts <- c(names(pdfFonts()), names(postscriptFonts()))
  if ("showtext" %in% names(sessionInfo()$otherPkgs)) {
    available_fonts <- c(
      available_fonts,
      font_families()
    )
  }
  if (!(base_family %in% available_fonts)) {
    # using commas as non-formatted line breaks lol
    warning(
      "Font family ", base_family, " was not found by `ggdubois`.",
      "You can try to install Montserrat (this theme's default) ",
      "with sysfonts:::font_add_google('Montserrat') and make it ",
      "usable with showtext::showtext_auto().",
      " Falling back now to your system's default sans font."
    )
    base_family <- "sans"
  }

  # slightly obtuse syntax is needed for conditional grid/border
  panel_grid <- ifelse(show_grid,
    function(x) element_line(color = "#dda58c"),
    element_blank
  )
  panel_border <- ifelse(show_border,
    function(x) element_rect(size = 0.5, fill = NA, color = colors["text"]),
    element_blank
  )

  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(color = colors["text"]),
      line = element_line(color = colors["text"], ),
      title = element_text(color = colors["text"], face = "bold"),
      rect = element_rect(fill = colors["background"], color = colors["text"]),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      plot.subtitle = element_text(face = "plain"),
      panel.grid.major = panel_grid(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(
        fill = colors["background"],
      ),
      panel.border = panel_border(),
      legend.key = element_rect(fill = colors["background"])
    )
}