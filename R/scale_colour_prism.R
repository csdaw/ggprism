#' Prism colour scales (discrete)
#'
#' A collection of discrete colour scales that use palettes which mirror the
#' colour schemes available in GraphPad Prism.
#'
#' @param palette `string`. Palette name, use
#' `lengths(ggprism_data$colour_palettes)` to show all valid palette names
#' and their number of values each palette supports.
#' @inheritDotParams ggplot2::discrete_scale
#'
#' @example inst/examples/ex-scale_colour_prism.R
#'
#' @export
scale_colour_prism <- function(palette = "colors", ...) {
  discrete_scale("colour", "prism", prism_colour_pal(palette = palette), ...)
}

#' @rdname scale_colour_prism
#' @export
scale_color_prism <- scale_colour_prism
