#' Prism fill scales (discrete)
#'
#' A collection of discrete fill scales that use palettes which mirror the
#' colour schemes available in GraphPad Prism.
#'
#' @param palette `string`. Palette name, see
#' `lengths(ggprism_data$fill_palettes)`
#' for valid palette names.
#' @inheritDotParams ggplot2::discrete_scale -aesthetics -scale_name
#'
#' @example inst/examples/ex-scale_fill_prism.R
#'
#' @export
scale_fill_prism <- function(palette = "colors", ...) {
  discrete_scale("fill", "prism", prism_fill_pal(palette = palette), ...)
}
