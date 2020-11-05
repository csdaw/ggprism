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
#' @inheritDotParams ggplot2::discrete_scale
#'
#' @example inst/examples/ex-scale_shape_prism.R
#'
#' @export
scale_shape_prism <- function(palette = "default", ...) {
  discrete_scale("shape", "prism", prism_shape_pal(palette = palette), ...)
}

#' @rdname scale_shape_prism
#' @inheritParams scale_shape_prism
#' @example inst/examples/ex-prism_shape_pal.R
#' @export
prism_shape_pal <- function(palette = c("default", "filled", "complete")) {
  palette <- match.arg(palette)
  shapes <- ggprism::ggprism_data$shape_palettes[[palette]]

  out <- manual_pal(shapes[["pch"]])
  attr(out, "max_n") <- nrow(shapes)
  out
}
