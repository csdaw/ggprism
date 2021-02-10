#' Prism shape scales (discrete)
#'
#' Shape scales that approximate those used in GraphPad Prism.
#' No unicode characters are used, only the default symbols available in R.
#'
#' The `default` palette supports up to 9 values. It does not use
#' any symbols with a fill.
#'
#' The `filled` palette supports up to 10 values. The first 5 symbols
#' have a fill.
#'
#' The `complete` palette supports up to 14 values. Symbols 5 to 9
#' have a fill.
#'
#' @param palette `string`. Palette name, one of: `default`,
#' `filled`, or `complete`.
#' @inheritDotParams ggplot2::discrete_scale -aesthetics -scale_name
#'
#' @return Returns a ggproto object of class _ScaleDiscrete_ which works with
#' _shape_ aesthetics.
#'
#' @example inst/examples/ex-scale_shape_prism.R
#'
#' @export
scale_shape_prism <- function(palette = "default", ...) {
  discrete_scale("shape", "prism", prism_shape_pal(palette = palette), ...)
}
