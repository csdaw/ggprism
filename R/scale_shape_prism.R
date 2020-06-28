#' Prism shape scales (discrete)
#'
#' Shape scales that approximate those used in GraphPad Prism.
#' No unicode characters are used, only the default symbols available in R.
#'
#' The \code{default} palette supports up to 9 values. It does not use
#' any symbols with a fill.
#'
#' The \code{filled} palette supports up to 10 values. The first 5 symbols
#' have a fill.
#'
#' The \code{complete} palette supports up to 14 values. Symbols 5 to 9
#' have a fill.
#'
#' @param palette \code{string}. Palette name, one of: \code{default},
#' \code{filled}, or \code{complete}.
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
