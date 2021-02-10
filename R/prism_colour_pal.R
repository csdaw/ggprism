#' Prism colour palettes
#'
#' A collection of colour palettes which mirror the
#' colour schemes available in GraphPad Prism.
#'
#' @inheritParams scale_colour_prism
#' @return Returns a function which takes a single integer as its only argument
#' and returns a character vector of hexadecimal colours.
#' See the examples below for usage.
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

#' @rdname prism_colour_pal
#' @export
prism_color_pal <- prism_colour_pal
