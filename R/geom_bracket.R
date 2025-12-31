#' Square brackets
#'
#' @description Draw square brackets. Works similar to [ggplot2::geom_segment].
#'
#' @param tip.length `numeric`, length of bracket tips. Default is 0.03. Set to 0 to
#' remove bracket tips and just draw a straight line.
#' @param direction `string`, direction the bracket tips point by default,
#' either "up" or "down". Default is "down".
#' @inheritParams ggplot2::geom_segment
#'
#' @returns Returns a ggplot2 `Layer` object that can be added to a plot.
#'
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' ggplot(
#'   data.frame(x = 1, xend = 4, y = 1, yend = 1),
#'   aes(x = x, xend = xend, y = y, yend = yend)
#' ) +
#'   geom_bracket()
#'
geom_bracket <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         ...,
                         tip.length = 0.03,
                         direction = "down",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBracket,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      tip.length = tip.length,
      direction = direction,
      na.rm = na.rm,
      ...
    )
  )
}

#' @format NULL
#' @usage NULL
#' @export
GeomBracket <- ggproto("GeomBracket", Geom,
                       # Fields ------------------------------------------------

                       # Specify the default and required aesthetics
                       required_aes = c("x", "y", "xend", "yend"),
                       default_aes = aes(
                         colour = "black",
                         linewidth = 0.5,
                         linetype = 1L,
                         alpha = NA
                       ),
                       extra_params = c("direction", "na.rm"),

                       # Methods ------------------------------------------------

                       ## draw_geom ---------------------------------------------
                       draw_panel = function(data,
                                             panel_params,
                                             coord,
                                             tip.length = 0.03,
                                             direction = "down",
                                             na.rm = FALSE) {

                         # Remove missing data, returning early if all are missing
                         data <- ggplot2::remove_missing(
                           df = data,
                           na.rm = na.rm,
                           vars = c("x", "y", "xend", "yend", "linetype", "linewidth", "lineend"),
                           name = "geom_bracket"
                         )
                         if (is.null(data) || nrow(data) == 0) return(ggplot2::zeroGrob())

                         # CRITICAL STEP! Turn x/y/xend/yend values into npc (between 0 and 1)
                         coord <- coord$transform(data, panel_params)

                         # need tip.length to be numeric vector length 2
                         if (length(tip.length) == 1) {
                           tip.length <- rep(tip.length, 2)
                         }

                         bracketGrob(
                           coord$x,
                           coord$y,
                           coord$xend,
                           coord$yend,
                           default.units = "native",
                           tip.length = tip.length,
                           direction = direction,
                           gp = gpar(
                             col = alpha(coord$colour, coord$alpha),
                             lwd = coord$linewidth * .pt,
                             lty = coord$linetype,
                             lineend = coord$lineend
                           )
                         )
                       }
)

bracketGrob <- function(x0 = unit(0, "npc"),
                        y0 = unit(0, "npc"),
                        x1 = unit(1, "npc"),
                        y1 = unit(1, "npc"),
                        tip.length = 0.03,
                        direction = "down",
                        default.units = "npc",
                        name = NULL,
                        gp = gpar(),
                        vp = NULL) {

  # Use the default unit if the user does not specify one
  if (!grid::is.unit(x0)) x0 <- unit(x0, default.units)
  if (!grid::is.unit(x1)) x1 <- unit(x1, default.units)
  if (!grid::is.unit(y0)) y0 <- unit(y0, default.units)
  if (!grid::is.unit(y1)) y1 <- unit(y1, default.units)

  gTree(
    x0 = x0,
    y0 = y0,
    x1 = x1,
    y1 = y1,
    tip.length = tip.length,
    direction = direction,
    name = name,
    gp = gp,
    vp = vp,
    cl = "bracket"
  )
}

#' @exportS3Method ggplot2::makeContent
makeContent.bracket <- function(x) {

  # Convert position and diameter values absolute units.
  # These are all numeric vectors.
  x0 <- grid::convertX(x$x0, "mm", valueOnly = TRUE)
  x1 <- grid::convertX(x$x1, "mm", valueOnly = TRUE)
  y0 <- grid::convertY(x$y0, "mm", valueOnly = TRUE)
  y1 <- grid::convertY(x$y1, "mm", valueOnly = TRUE)

  # Leave tip.length and direction untouched
  tip.length <- x$tip.length
  direction <- x$direction

  # Transform the input data to a data frame containing bracket paths
  brackets <- lapply(seq_along(x0), function(i) {
    cbind(
      create_bracket(
        x = x0[i],
        y = y0[i],
        xend = x1[i],
        yend = y1[i],
        tip.length = tip.length * 100, # magic number for ggprism backwards compatibility
        direction = direction
      ),
      id = i
    )
  })

  # output: data.frame with columns x, y, id
  brackets <- do.call(rbind, brackets)

  bracket_paths <- grid::polylineGrob(
    x = brackets$x,
    y = brackets$y,
    id = brackets$id,
    default.units = "mm",
    gp = x$gp # gp which was defined back in draw_panel() is eventually passed through to here.
  )
  # x$children which was a gList of length 0, is now defined as gList of polyline grobs.
  grid::setChildren(x, grid::gList(bracket_paths))
}
