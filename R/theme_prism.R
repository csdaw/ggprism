theme_prism <- function(palette = "black_and_white", base_size = 12,
                        base_family = "", base_fontface = "bold",
                        base_line_size = base_size / 22,
                        base_rect_size = base_size / 22,
                        axis_text_angle = 0) {
  # Ensure x axis text is at a sensible angle
  angle <- axis_text_angle[1]
  if(!angle %in% c(0, 45, 90, 270))
    stop(sprintf("'axis_text_angle' must be one of [%s]",
                 paste(c(0, 45, 90, 270), collapse=", ")),
         ".\nFor other angles, use the guide_axis() function in ggplot2 instead.",
         call. = FALSE)

  # Get element colours from palette
  colors <- deframe(ggprism::ggprism_data$themes[[palette]])

  # Define half_line for correct relative size of elements
  half_line <- base_size / 2

  t <- theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line =               element_line(
      colour = "black", size = base_line_size,
      linetype = 1, lineend = "square"
    ),
    rect =               element_rect(
      fill = "white", colour = "black",
      size = base_rect_size, linetype = 1
    ),
    text =               element_text(
      family = base_family, face = base_fontface,
      colour = "black", size = base_size,
      lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
      margin = margin(), debug = FALSE
    ),

    axis.line =          element_blank(),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(size = rel(0.8), colour = "grey30"),
    axis.text.x =        element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top =    element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks =         element_line(colour = "grey20"),
    axis.ticks.length =  unit(half_line / 2, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title.x =       element_text(
      margin = margin(t = half_line / 2),
      vjust = 1
    ),
    axis.title.x.top =   element_text(
      margin = margin(b = half_line / 2),
      vjust = 0
    ),
    axis.title.y =       element_text(
      angle = 90,
      margin = margin(r = half_line / 2),
      vjust = 1
    ),
    axis.title.y.right = element_text(
      angle = -90,
      margin = margin(l = half_line / 2),
      vjust = 0
    ),

    legend.background =  element_rect(colour = NA),
    legend.spacing =     unit(2 * half_line, "pt"),
    legend.spacing.x =    NULL,
    legend.spacing.y =    NULL,
    legend.margin =      margin(half_line, half_line, half_line, half_line),
    legend.key =         element_rect(fill = "grey95", colour = NA),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    legend.box.margin =  margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(2 * half_line, "pt"),

    panel.background =   element_rect(fill = "grey92", colour = NA),
    panel.border =       element_blank(),
    panel.grid =         element_line(colour = "white"),
    panel.grid.minor =   element_line(size = rel(0.5)),
    panel.spacing =      unit(half_line, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,

    strip.background =   element_rect(fill = "grey85", colour = NA),
    strip.text =         element_text(
      colour = "grey10",
      size = rel(0.8),
      margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
    ),
    strip.text.x =       NULL,
    strip.text.y =       element_text(angle = -90),
    strip.text.y.left =  element_text(angle = 90),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = unit(half_line / 2, "pt"),

    plot.background =    element_rect(colour = "white"),
    plot.title =         element_text( # font size "large"
      size = rel(1.2),
      hjust = 0, vjust = 1,
      margin = margin(b = half_line)
    ),
    plot.title.position = "panel",
    plot.subtitle =      element_text( # font size "regular"
      hjust = 0, vjust = 1,
      margin = margin(b = half_line)
    ),
    plot.caption =       element_text( # font size "small"
      size = rel(0.8),
      hjust = 1, vjust = 1,
      margin = margin(t = half_line)
    ),
    plot.caption.position = "panel",
    plot.tag =           element_text(
      size = rel(1.2),
      hjust = 0.5, vjust = 0.5
    ),
    plot.tag.position =  'topleft',
    plot.margin =        margin(half_line, half_line, half_line, half_line),

    complete = TRUE
  )

  # make sure all elements are set to NULL if not explicitly defined
  ggplot2:::ggplot_global$theme_all_null %+replace% t
}
