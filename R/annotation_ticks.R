#' Add ticks as ggplot annotation
#'
#' This is an annotation function to add tick marks (major, minor, or both) to
#' a ggplot. Clipping must be turned off if the ticks are to appear outside the
#' plotting area, for example with: `coord_cartesian(clip = "off")`.
#'
#' @source The code is a slightly modified version of the answer to this
#' [Stack Overflow](https://stackoverflow.com/questions/58485334) question,
#' which is itself a refactored version of this
#' [`annotation_ticks()`](https://github.com/hrbrmstr/ggalt/blob/master/R/annotation_ticks.r)
#' function.
#'
#' @param sides `string`. Indicates which sides of the plot should ticks
#' appear. Can be any of `"trbl"`, for top, right, bottom, left.
#' @param type `string`. Types of ticks that appear. One of
#' `"major"`, `"minor"`, or `"both"`. Control number of ticks
#' by controlling the `breaks` and `minor_breaks` arguments in the
#' various ggplot2 `scale_(x|y)_` functions.
#' @param outside `logical`. Should the ticks point outside of the plotting
#' area? If `TRUE` clipping must be turned off.
#' @param tick.length a \code{\link[grid]{unit}} object specifying the length
#' of major ticks.
#' @param minor.length a \code{\link[grid]{unit}} object specifying the length
#' of minor ticks.
#' @param linewidth `numeric`. Linewidth of ticks.
#' @param colour,color `string`. Colour of ticks.
#' @param linetype `string` or `numeric`. Linetype of tick marks.
#' @param lineend `string`. Lineend of ticks. One of `"square"`
#' (default), `"butt"`, or `"round"`.
#' @param alpha `numeric`. Transparency of ticks.
#' @param data `data.frame`. Use this argument to control the appearance of
#' ticks on different facets. Pass a data.frame containing the levels from the
#' faceting variable you want to annotate specifically.
#' See [here](https://stackoverflow.com/questions/20128582) for an example.
#'
#' @return Returns a _layer_ ggproto object with `geom = GeomTicks`.
#'
#' @example inst/examples/ex-annotation_ticks.R
#'
#' @export
annotation_ticks <- function(sides = "b",
                             type = "both",
                             outside = FALSE,
                             tick.length = unit(4.8, "pt"),
                             minor.length = unit(2.4, "pt"),
                             linewidth = 0.6,
                             colour = "black",
                             color = NULL,
                             linetype = 1,
                             lineend = "butt",
                             alpha = 1,
                             data = data.frame(x = NA)) {
  if (missing(colour)) {
    if (!is.null(color)) colour <- color
  } else {
    if(!missing(color)) warn("Use colour or color but not both.")
  }

  # check for invalid side
  if (grepl("[^btlr]", sides)) {
    stop(gsub("[btlr]", "", sides), " is not a valid side: b,t,l,r are valid")
  }

  # split sides to character vector
  sides <- strsplit(sides, "")[[1]]

  if (!type %in% c("both", "major", "minor")) {
    stop("Type must be one of: both, major, minor")
  }

  if (outside) {
    tick.length <- -1 * tick.length
    minor.length <- -1 * minor.length
  } else {
    tick.length <- tick.length
    minor.length <- minor.length
  }

  layer(
    data = data,
    mapping = NULL,
    stat = StatIdentity,
    geom = GeomTicks,
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(
      sides = sides,
      type = type,
      tick.length = tick.length,
      minor.length = minor.length,
      linewidth = linewidth,
      colour = colour,
      linetype = linetype,
      lineend = lineend,
      alpha = alpha
    )
  )
}


#' ggproto classes for ggprism
#'
#' @usage NULL
#' @format NULL
#' @rdname ggprism-ggproto
#' @keywords internal
#' @export
GeomTicks <- ggproto("GeomTicks", Geom, extra_params = "",
                     handle_na = function(data, params) {data},
                     draw_panel = function(data,
                                           panel_scales,
                                           coord,
                                           sides = c("b", "l"),
                                           type = c("both", "major", "minor"),
                                           tick.length = unit(4.8, "pt"),
                                           minor.length = unit(2.4, "pt")) {
                       ticks <- list()

                       for (s in 1:length(sides)) {
                         if (grepl("[b|t]", sides[s])) {

                           x_major_breaks <- panel_scales$x$break_positions()
                           x_minor_breaks <- setdiff(panel_scales$x$break_positions_minor(), x_major_breaks)

                           if (type == "both") {
                             xticks <- union(x_major_breaks, x_minor_breaks)
                             tick.lengths <- unit.c(rep(tick.length, length(x_major_breaks)),
                                                   rep(minor.length, length(x_minor_breaks)))
                           } else if (type == "major") {
                             xticks <- x_major_breaks
                             tick.lengths <- tick.length
                           } else if (type == "minor") {
                             xticks <- x_minor_breaks
                             tick.lengths <- minor.length
                           }


                           # Make the grobs
                           if (grepl("b", sides[s])) {
                             ticks$x_b <- with(
                               data,
                               segmentsGrob(
                                 x0 = unit(xticks, "npc"),
                                 x1 = unit(xticks, "npc"),
                                 y0 = unit(0, "npc"),
                                 y1 = tick.lengths,
                                 gp = gpar(
                                   col = alpha(colour, alpha),
                                   lty = linetype,
                                   lwd = linewidth * .pt,
                                   lineend = lineend
                                 )
                               )
                             )
                           }
                           if (grepl("t", sides[s])) {
                             ticks$x_t <- with(
                               data,
                               segmentsGrob(
                                 x0 = unit(xticks, "npc"),
                                 x1 = unit(xticks, "npc"),
                                 y0 = unit(1, "npc"),
                                 y1 = unit(1, "npc") - tick.lengths,
                                 gp = gpar(
                                   col = alpha(colour, alpha),
                                   lty = linetype,
                                   lwd = linewidth * .pt,
                                   lineend = lineend
                                 )
                               )
                             )
                           }
                         }

                         if (grepl("[l|r]", sides[s])) {

                           y_major_breaks <- panel_scales$y$break_positions()
                           y_minor_breaks <- setdiff(panel_scales$y$break_positions_minor(), y_major_breaks)

                           if (type == "both") {
                             yticks <- union(y_major_breaks, y_minor_breaks)
                             tick.lengths <- unit.c(rep(tick.length, length(y_major_breaks)),
                                                   rep(minor.length, length(y_minor_breaks)))
                           } else if (type == "major") {
                             yticks <- y_major_breaks
                             tick.lengths <- tick.length
                           } else if (type == "minor") {
                             yticks <- y_minor_breaks
                             tick.lengths <- minor.length
                           }

                           # Make the grobs
                           if (grepl("l", sides[s])) {
                             ticks$y_l <- with(
                               data,
                               segmentsGrob(
                                 y0 = unit(yticks, "npc"),
                                 y1 = unit(yticks, "npc"),
                                 x0 = unit(0, "npc"),
                                 x1 = tick.lengths,
                                 gp = gpar(
                                   col = alpha(colour, alpha),
                                   lty = linetype,
                                   lwd = linewidth * .pt,
                                   lineend = lineend
                                 )
                               )
                             )
                           }
                           if (grepl("r", sides[s])) {
                             ticks$y_r <- with(
                               data,
                               segmentsGrob(
                                 y0 = unit(yticks, "npc"),
                                 y1 = unit(yticks, "npc"),
                                 x0 = unit(1, "npc"),
                                 x1 = unit(1, "npc") - tick.lengths,
                                 gp = gpar(
                                   col = alpha(colour, alpha),
                                   lty = linetype,
                                   lwd = linewidth * .pt,
                                   lineend = lineend
                                 )
                               )
                             )
                           }
                         }
                       }
                       gTree(children = do.call("gList", ticks))
                     },
                     default_aes = aes(colour = "black", linewidth = 0.6,
                                       linetype = 1, lineend = "butt", alpha = 1)
)
