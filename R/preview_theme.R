#' Preview Prism themes
#'
#' Quickly generate a preview of a ggprism theme.
#' See `names(ggprism_data$themes)` for valid palette names.
#'
#' @param palette `string`. Palette name.
#'
#' @return Returns an object of class _ggplot_.
#'
#' @example inst/examples/ex-preview_theme.R
#'
#' @export
preview_theme <- function(palette) {
  if (!palette %in% names(ggprism::ggprism_data$themes)) {
    stop("The palette ", paste(palette), " does not exist.
         See names(ggprism_data$themes) for valid palette names")
  }
  if (palette == "all_null") {
    stop("Cannot preview an all NULL theme")
  }

  # get maximum length of colour palette
  max_n <- attr(prism_colour_pal(palette), "max_n")

  # generate some data
  df <- data.frame(
    x = rep(factor(1:20), 20),
    y = stats::rnorm(20^2)
  )

  # subset data depending on palette length
  df <- df[df$x %in% c(1:max_n), ]

  # make a boxplot which shows entire palette
  ggplot(df, aes(x = .data$x, y = .data$y, colour = .data$x, fill = .data$x)) +
    geom_boxplot() +
    theme_prism(palette = palette) +
    scale_colour_prism(palette = palette) +
    scale_fill_prism(palette = palette) +
    labs(title = paste0(palette, " (", max_n, ")"))
}
