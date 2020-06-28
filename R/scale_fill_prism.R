#' Prism fill scales (discrete)
#'
#' A collection of discrete fill scales that use palettes which mirror the
#' colour schemes available in GraphPad Prism.
#'
#' @param palette \code{string}. Palette name, see
#' \code{lengths(ggprism_data$fill_palettes)}
#' for valid palette names.
#' @inheritDotParams ggplot2::discrete_scale
#'
#' @example inst/examples/ex-scale_fill_prism.R
#'
#' @export
scale_fill_prism <- function(palette = "colors", ...) {
  discrete_scale("fill", "prism", prism_fill_pal(palette = palette), ...)
}

#' @rdname scale_fill_prism
#' @inheritParams scale_fill_prism
#' @example inst/examples/ex-prism_fill_pal.R
#' @export
prism_fill_pal <- function(palette = "colors") {
  allfills <- ggprism::ggprism_data$fill_palettes
  if (!palette %in% names(allfills)) {
    stop("The palette ", paste(palette), " does not exist.
         See lengths(ggprism_data$fill_palettes) for valid palette names.")
  }
  values <- unname(allfills[[palette]])
  f <- manual_pal(values)
  attr(f, "max_n") <- length(values)
  f
}
