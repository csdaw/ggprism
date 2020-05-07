#' Prism fill palettes (discrete)
#'
#' Description.
#'
#' @param palette string. Palette name, see names(ggprism_data$fill_palettes)
#' for valid palette names.
#' @family fill
#' @export
#' @example inst/examples/ex-prism_fill_pal.R
prism_fill_pal <- function(palette = "colors") {
  allfills <- ggprism::ggprism_data$fill_palettes
  if (!palette %in% names(allfills)) {
    stop("The palette ", paste(palette), " does not exist.
         See names(ggprism_data$fill_palettes) for valid palette names.")
  }
  values <- unname(allfills[[palette]])
  f <- manual_pal(values)
  attr(f, "max_n") <- length(values)
  f
}

#' Prism fill scales (discrete)
#'
#' See \code{\link{prism_fill_pal}()} for details.
#'
#' @inheritParams prism_fill_pal
#' @inheritDotParams ggplot2::discrete_scale
#' @family fill prism
#' @example inst/examples/ex-scale_fill_prism.R
#' @export
scale_fill_prism <- function(palette = "colors", ...) {
  discrete_scale("fill", "prism", prism_fill_pal(palette = palette), ...)
}
