#' Prism fill palettes
#'
#' A collection of fill palettes which mirror the
#' colour schemes available in GraphPad Prism.
#'
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
