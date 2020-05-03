#' Prism shape palettes (discrete)
#'
#' Shape palettes used by
#' \href{https://www.graphpad.com}{GraphPad Prism}
#'
#' The complete palette is not exact but approximates
#' the shape palette from Prism using only the default symbols
#' available in R. No unicode characters are used.
#'
#' The default palette supports up to 9 values. It does not use
#' any symbols with a fill.
#'
#' The filled palette supports up to 10 values. The first 5 symbols
#' have a fill.
#'
#' The complete palette supports up to 14 values. Symbols 5 to 9
#' have a fill.
#'
#' @param palette string. Palette name, one of: default, filled,
#' or complete.
#' @family shape
#' @example inst/examples/ex-prism_shape_pal.R
#' @export
prism_shape_pal <- function(palette = c("default", "filled", "complete")) {
  palette <- match.arg(palette)
  shapes <- ggprism::ggprism_data$shape_palettes[[palette]]

  out <- manual_pal(shapes[["pch"]])
  attr(out, "max_n") <- nrow(shapes)
  out
}

#' Prism shape scales (discrete)
#'
#' See \code{\link{prism_shape_pal}()} for details.
#'
#' @inheritParams prism_shape_pal
#' @param ... Other arguments passed on to \funclink{discrete_scale}.
#' @family shape prism
#' @example inst/examples/ex-scale_shape_prism.R
#' @export
scale_shape_prism <- function(palette = "default", ...) {
  discrete_scale("shape", "prism", prism_shape_pal(palette = palette), ...)
}
