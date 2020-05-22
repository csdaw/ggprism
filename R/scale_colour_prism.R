#' Prism colour palettes (discrete)
#'
#' Description.
#'
#' @param palette string. Palette name, see names(ggprism_data$colour_palettes)
#' for valid palette names.
#' @family colour prism
#' @export
#' @example inst/examples/ex-prism_colour_pal.R
prism_colour_pal <- function(palette = "colors") {
  allcols <- ggprism::ggprism_data$colour_palettes
  if (!palette %in% names(allcols)) {
    stop("The palette ", paste(palette), " does not exist.
         See names(ggprism_data$colour_palettes) for valid palette names.")
  }
  values <- unname(allcols[[palette]])
  f <- manual_pal(values)
  attr(f, "max_n") <- length(values)
  f
}

#' @export
#' @rdname prism_colour_pal
prism_color_pal <- prism_colour_pal

#' Prism colour scales (discrete)
#'
#' See \code{\link{prism_fill_pal}()} for details.
#'
#' @inheritParams prism_colour_pal
#' @inheritDotParams ggplot2::discrete_scale
#' @family colour prism
#' @example inst/examples/ex-scale_colour_prism.R
#' @export
scale_colour_prism <- function(palette = "colors", ...) {
  discrete_scale("colour", "prism", prism_colour_pal(palette = palette), ...)
}

#' @export
#' @rdname scale_colour_prism
scale_color_prism <- scale_colour_prism
