#' Add ticks as ggplot annotation
#'
#' This is an annotation function to add tick marks (major, minor, or both) to
#' a ggplot. Clipping must be turned off if the ticks are to appear outside the
#' plotting area, for example with: \code{coord_cartesian(clip = "off")}.
#'
#' @source The code is a slightly modified version of the answer to this
#' \href{https://stackoverflow.com/questions/58485334}{Stack Overflow} question,
#' which is itself a refactored version of this
#' \href{https://github.com/hrbrmstr/ggalt/blob/master/R/annotation_ticks.r}{\code{annotation_ticks}}
#' function.
#'
#' @param sides \code{string}. Indicates which sides of the plot should ticks
#' appear. Can be any of \code{"trbl"}, for top, right, bottom, left.
#' @param type \code{string}. Types of ticks that appear. One of
#' \code{"major"}, \code{"minor"}, or \code{"both"}. Control number of ticks
#' by controlling the \code{breaks} and \code{minor_breaks} arguments in the
#' various \code{scale_(x|y)_} functions.
#' @param scale \code{string} or \code{character vector}. Type of scale to be
#' used for each side, for example \code{"identity"} or \code{"log10"}. If
#' length is 1 then the scale will be used for all \code{sides}. If longer than
#' 1 the length must match the number of \code{sides} specified.
#' @param scaled \code{logical}. Is the data already scaled? Should be
#' \code{TRUE} (default) if data is already transformed with \code{log10()} or
#' when using \code{scale_y_log10()}. Should be \code{FALSE} when using
#' \code{coord_trans(y = "log10")}.
#' @param outside \code{logical}. Should the ticks point outside of the plotting
#' area? If \code{TRUE} clipping must be turned off.
#' @param tick.length a \code{\link[grid]{unit}} object specifying the length
#' of major ticks.
#' @param minor.length a \code{\link[grid]{unit}} object specifying the length
#' of mminor ticks.
#' @param size \code{numeric}. Linewidth of ticks.
#' @param colour,color \code{string}. Colour of ticks.
#' @param linetype \code{string} or \code{numeric}. Linetype of tick marks.
#' @param lineend \code{string}. Lineend of ticks. One of \code{"square"}
#' (default), \code{"butt"}, or \code{"round"}.
#' @param alpha \code{numeric}. Transparency of ticks.
#' @param data \code{data.frame}. Use this argument to control the appearance of
#' ticks on different facets. Pass a data.frame containing the levels from the
#' faceting variable you want to annotate specifically.
#' See \href{https://stackoverflow.com/questions/20128582}{here} for an example.
#'
#' @example inst/examples/ex-annotation_ticks.R
#'
#' @export
annotation_ticks <- function(sides = "b",
                             type = "both",
                             scale = "identity",
                             scaled = TRUE,
                             outside = FALSE,
                             tick.length = unit(4.8, "pt"),
                             minor.length = unit(2.4, "pt"),
                             size = 0.6,
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

  if (length(sides) != length(scale)) {
    if (length(scale) == 1) {
      scale <- rep(scale, length(sides))
    } else {
      stop("Number of scales does not match the number of sides")
    }
  }

  base <- sapply(scale,
                 function(x) switch(x, "identity" = 10,
                                    "log10" = 10, "log" = exp(1)),
                 USE.NAMES = FALSE)

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

  delog <- scale %in% "identity"

  layer(
    data = data,
    mapping = NULL,
    stat = StatIdentity,
    geom = GeomTicks,
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(
      base = base,
      sides = sides,
      type = type,
      scaled = scaled,
      tick.length = tick.length,
      minor.length = minor.length,
      size = size,
      colour = colour,
      linetype = linetype,
      lineend = lineend,
      alpha = alpha,
      delog = delog
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
                                           base = c(10, 10),
                                           sides = c("b", "l"),
                                           type = c("both", "major", "minor"),
                                           scaled = TRUE,
                                           tick.length = unit(4.8, "pt"),
                                           minor.length = unit(2.4, "pt"),
                                           delog = c(x = TRUE, y = TRUE)) {
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
                                   lwd = size * .pt,
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
                                   lwd = size * .pt,
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
                                   lwd = size * .pt,
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
                                   lwd = size * .pt,
                                   lineend = lineend
                                 )
                               )
                             )
                           }
                         }
                       }
                       gTree(children = do.call("gList", ticks))
                     },
                     default_aes = aes(colour = "black", size = 0.6,
                                       linetype = 1, lineend = "butt", alpha = 1)
)
