theme_prism_bw <- function(base_size = 12, base_family = "",
                           base_fontface = "bold",
                           base_line_size = base_size / 22,
                           base_rect_size = base_size / 22,
                           axis_text_angle = 0) {
  # ensure x axis text is at a sensible angle
  angle <- axis_text_angle[1]
  if(!angle %in% c(0, 45, 90, 270))
    stop(sprintf("'axis_text_angle' must be one of [%s]",
                 paste(c(0, 45, 90, 270), collapse=", ")),
         ".\nFor other angles, use the guide_axis() function in ggplot2 instead.",
         call. = FALSE)

  # The half-line (base-fontsize / 2) sets up the basic vertical
  # rhythm of the theme. Most margins will be set to this value.
  # However, when we work with relative sizes, we may want to multiply
  # `half_line` with the appropriate relative size. This applies in
  # particular for axis tick sizes. And also, for axis ticks and
  # axis titles, `half_size` is too large a distance, and we use `half_size/2`
  # instead.
  half_line <- base_size / 2

  # Throughout the theme, we use three font sizes, `base_size` (`rel(1)`)
  # for normal, `rel(0.8)` for small, and `rel(1.2)` for large.

  # Fontface is bold by default. Use plain for normal font.

  # Starts with theme_grey and then modify some parts
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # make line endings 'square' instead of 'butt'
      line              = element_line(lineend = "square"),
      # change plot title
      plot.title        = element_text(face = base_fontface,
                                       size = rel(1.2), hjust = 0.5,
                                       margin = margin(b = base_size, unit = "pt")),
      # change axes titles
      axis.title        = element_text(face = base_fontface, size = base_size),
      axis.title.x      = element_text(margin = margin(t = rel(7.5))),
      axis.title.y      = element_text(angle = 90, margin = margin(r = rel(10))),
      # change axis text
      axis.text         = element_text(face = base_fontface, size = rel(0.95), colour = "black"),
      axis.text.x       = element_text(
        margin = margin(t = rel(2)),
        angle = axis_text_angle,
        hjust = ifelse(axis_text_angle %in% c(45, 90, 270), 1, 0.5),
        vjust = ifelse(axis_text_angle %in% c(0, 90, 270), 0.5, 1)
        ),
      axis.text.y       = element_text(margin = margin(r = rel(1.25)), hjust = 1),
      # change axis ticks
      axis.ticks.length = unit(half_line / 1.1, "pt"),
      axis.ticks        = element_line(colour = "black", size = half_line / 10),
      # show axes
      axis.line         = element_line(colour = "black", size = half_line / 10),
      # white background and no border
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_blank(),
      # remove gridlines
      panel.grid = element_blank(),
      # change strip elements (facet labels)
      strip.background  = element_blank(),
      strip.text.x = element_text(face = base_fontface, margin = margin(b = half_line / 1.5)),
      strip.text.y = element_text(face = base_fontface, margin = margin(l = half_line / 1.5)),
      # change legend elements
      legend.key        = element_blank(),
      legend.title      = element_blank(),
      legend.key.width  = unit(base_size * 1.8, "pt"),

      # change custom prism theme elements
      prism.ticks.length = unit(half_line / 2.5, "pt"),

      complete = TRUE
    )
}

