# ---------------------------------------------------------------------------
# SA in Numbers house style
# Colours follow a colour-blind-validated categorical order. Use at most three
# coloured series per chart; everything else (normals, bands, context) is grey.
# ---------------------------------------------------------------------------

SAIN <- list(
  surface  = "#fcfcfb",
  ink      = "#0b0b0b",
  ink2     = "#52514e",
  muted    = "#898781",
  grid     = "#e1e0d9",
  axis     = "#c3c2b7",
  # categorical, in fixed order - never cycle or reorder
  s1 = "#2a78d6",   # blue   - always "this season / now"
  s2 = "#eb6834",   # orange - first comparison
  s3 = "#1baf7a",   # aqua   - second comparison
  # uncertainty / normal-range bands (neutral, so they never read as a series)
  band_outer = "#ecebe6",
  band_inner = "#d9d8d1",
  brand = "SA in Numbers"
)

theme_sain <- function(base_size = 15) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = SAIN$surface, colour = NA),
      panel.background = ggplot2::element_rect(fill = SAIN$surface, colour = NA),
      panel.grid.major.y = ggplot2::element_line(colour = SAIN$grid, linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = SAIN$axis, linewidth = 0.4),
      axis.text  = ggplot2::element_text(colour = SAIN$muted, size = ggplot2::rel(0.8)),
      axis.title = ggplot2::element_text(colour = SAIN$ink2, size = ggplot2::rel(0.8)),
      plot.title = ggplot2::element_text(colour = SAIN$ink, face = "bold", size = ggplot2::rel(1.35),
                                         margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(colour = SAIN$ink2, size = ggplot2::rel(0.85),
                                            lineheight = 1.15, margin = ggplot2::margin(b = 12)),
      plot.caption = ggplot2::element_text(colour = SAIN$muted, size = ggplot2::rel(0.65),
                                           hjust = 0, lineheight = 1.15, margin = ggplot2::margin(t = 12)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = SAIN$ink2, size = ggplot2::rel(0.8)),
      plot.margin = ggplot2::margin(24, 28, 18, 24)
    )
}

sain_caption <- function(source, note = NULL) {
  paste0(if (!is.null(note)) paste0(note, "\n") else "",
         "Source: ", source, "\n", SAIN$brand, " \u00b7 open data and code")
}

# Export one chart in the three social formats plus a web version.
# Sizes in pixels; text scales with the device so it stays legible on phones.
SOCIAL_FORMATS <- list(
  story  = c(w = 1080, h = 1920),   # TikTok, Reels, Shorts, Stories
  square = c(w = 1080, h = 1080),   # Instagram / Facebook feed
  wide   = c(w = 1920, h = 1080),   # YouTube, X, LinkedIn, website hero
  web    = c(w = 1400, h = 900)
)

# Re-wrap title and subtitle to fit narrow formats (story, square)
wrap_labels <- function(plot, title_width, subtitle_width) {
  rewrap <- function(x, w) {
    if (is.null(x) || !is.character(x)) return(x)
    paste(strwrap(gsub("\\s*\n\\s*", " ", x), width = w), collapse = "\n")
  }
  plot$labels$title <- rewrap(plot$labels$title, title_width)
  plot$labels$subtitle <- rewrap(plot$labels$subtitle, subtitle_width)
  plot
}

save_social <- function(plot, stem, out_dir = "outputs/social",
                        formats = names(SOCIAL_FORMATS), dpi = 150) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  paths <- character()
  for (f in formats) {
    sz <- SOCIAL_FORMATS[[f]]
    path <- file.path(out_dir, sprintf("%s_%s.png", stem, f))
    p <- if (sz[["w"]] <= 1080) wrap_labels(plot, 40, 64) else plot
    ggplot2::ggsave(path, p, device = ragg::agg_png,
                    width = sz[["w"]] / dpi, height = sz[["h"]] / dpi, dpi = dpi,
                    bg = SAIN$surface)
    paths <- c(paths, path)
  }
  invisible(paths)
}
