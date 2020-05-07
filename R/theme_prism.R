#' Title
#'
#' Description.
#'
#' @param palette Description.
#' @param base_size Description.
#' @param base_family Description.
#' @param base_fontface Description.
#' @param base_line_size Description.
#' @param base_rect_size Description.
#' @param axis_text_angle Description.
#' @param frame Description.
#'
#' @return
#' @export
#'
#' @example inst/examples/ex-theme_prism.R
theme_prism <- function(palette = "black_and_white", base_size = 12,
                        base_family = "", base_fontface = "bold",
                        base_line_size = base_size / 22,
                        base_rect_size = base_size / 22,
                        axis_text_angle = 0,
                        frame = FALSE) {
  # Ensure x axis text is at a sensible angle
  angle <- axis_text_angle[1]
  if(!angle %in% c(0, 45, 90, 270))
    stop(sprintf("'axis_text_angle' must be one of [%s]",
                 paste(c(0, 45, 90, 270), collapse=", ")),
         ".\nFor other angles, use the guide_axis() function in ggplot2 instead.",
         call. = FALSE)

  # Get element colours from palette
  if (!palette %in% names(ggprism::ggprism_data$themes)) {
    stop("The palette ", paste(palette), " does not exist.
         See names(ggprism_data$themes) for valid palette names.")
  }
  colours <- deframe(ggprism::ggprism_data$themes[[palette]])

  # Define half_line for correct relative size of elements
  half_line <- base_size / 2

  t <- theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line =               element_line(
      colour = colours["axisColor"], size = base_line_size,
      linetype = 1, lineend = "square"
    ),
    rect =               element_rect(
      fill = "white", colour = colours["axisColor"],
      size = base_rect_size, linetype = 1
    ),
    text =               element_text(
      family = base_family, face = base_fontface,
      colour = colours["graphTitleColor"], size = base_size,
      lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
      margin = margin(), debug = FALSE
    ),

    # Prism custom theme elements
    prism.ticks.length = unit(half_line / 2.5, "pt"),

    # Normal ggplot2 theme elements
    axis.line =          element_line(size = half_line / 10, lineend = ifelse(frame, "butt", "square")),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(size = rel(0.95), colour = colours["axisLabelColor"]),
    axis.text.x =        element_text(
      margin = margin(t = 0.8 * half_line / 2),
      angle = axis_text_angle,
      hjust = ifelse(axis_text_angle %in% c(45, 90, 270), 1, 0.5),
      vjust = ifelse(axis_text_angle %in% c(0, 90, 270), 0.5, 1)
      ),
    axis.text.x.top =    element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = 0.5 * half_line / 2), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = 0.5 * half_line / 2), hjust = 0),
    axis.ticks =         element_line(size = half_line / 10),
    axis.ticks.length =  unit(half_line / 1.1, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title = element_text(size = base_size, face = base_fontface, colour = colours["axisTitleColor"]),
    axis.title.x =       element_text(
      margin = margin(t = half_line * 1.2),
      vjust = 1
    ),
    axis.title.x.top =   element_text(
      margin = margin(b = half_line * 1.2),
      vjust = 0
    ),
    axis.title.y =       element_text(
      angle = 90,
      margin = margin(r = half_line * 1.2),
      vjust = 1
    ),
    axis.title.y.right = element_text(
      angle = -90,
      margin = margin(l = half_line * 1.2),
      vjust = 0
    ),

    legend.background =  element_blank(),
    legend.spacing =     unit(2 * half_line, "pt"),
    legend.spacing.x =    NULL,
    legend.spacing.y =    NULL,
    legend.margin =      margin(half_line, half_line, half_line, half_line),
    legend.key =         element_blank(),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   unit(base_size * 1.8, "pt"),
    legend.text =        element_text(size = rel(0.8), face = "plain"),
    legend.text.align =  NULL,
    legend.title =       element_blank(),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    legend.box.margin =  margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(2 * half_line, "pt"),

    panel.background = element_rect(fill = ifelse(palette == "office", colours["plottingAreaColor"], NA),
                                    colour = NA),
    panel.border =       element_rect(fill = NA,
                                      colour = ifelse(frame, colours["axisColor"], NA),
                                      size = half_line / 10),
    panel.grid =         element_blank(),
    panel.grid.minor =   element_line(size = rel(0.5)),
    panel.spacing =      unit(half_line, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,

    strip.background =   element_blank(),
    strip.text =         element_text(
      colour = "grey10",
      size = rel(0.8),
      margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
    ),
    strip.text.x =       element_text(margin = margin(b = half_line / 1.5)),
    strip.text.y =       element_text(angle = -90, margin = margin(l = half_line / 1.5)),
    strip.text.y.left =  element_text(angle = 90),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = unit(half_line / 2, "pt"),

    plot.background =    element_rect(fill = colours["pageBackgroundColor"], colour = NA),
    plot.title =         element_text( # font size "large"
      size = rel(1.2),
      hjust = 0.5, vjust = 1,
      margin = margin(b = base_size)
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
  ggprism::ggprism_data$themes[["all_null"]] %+replace% t
}
