#' Prism colour scales (discrete)
#'
#' A collection of discrete colour scales that use palettes which mirror the
#' colour schemes available in GraphPad Prism.
#'
#' @param palette \code{string}. Palette name, use
#' \code{lengths(ggprism_data$colour_palettes)} to show all valid palette names
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

#' @rdname scale_colour_prism
#' @inheritParams scale_colour_prism
#' @example inst/examples/ex-prism_colour_pal.R
#' @export
prism_colour_pal <- function(palette = "colors") {
  allcols <- ggprism::ggprism_data$colour_palettes
  if (!palette %in% names(allcols)) {
    stop("The palette ", paste(palette), " does not exist.
         See lengths(ggprism_data$colour_palettes) for valid palette names.")
  }
  values <- unname(allcols[[palette]])
  f <- manual_pal(values)
  attr(f, "max_n") <- length(values)
  f
}

#' @rdname scale_colour_prism
#' @export
prism_color_pal <- prism_colour_pal
