# code from teunbrand's answer to https://stackoverflow.com/questions/58485334
# which is itself a refactored version of hrbrmstr's annotation_ticks function.
# https://github.com/hrbrmstr/ggalt/blob/master/R/annotation_ticks.r
annotation_ticks <- function(sides = "b",
                             scale = "identity",
                             scaled = TRUE,
                             outside = FALSE,
                             ticklength = unit(0.15, "cm"),
                             colour = "black",
                             base_size = 12,
                             linetype = 1,
                             alpha = 1,
                             color = NULL,
                             ticks_per_base = NULL,
                             data = data.frame(x = NA),
                             ...) {
  if (!is.null(color)) {
    colour <- color
  }

  # check for numeric base_size
  if (!is.numeric(base_size)) {
    stop("base_size must be numeric")
  } else {size = (base_size / 2) / 10}

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

  base <- sapply(scale, function(x) switch(x, "identity" = 10, "log10" = 10, "log" = exp(1)), USE.NAMES = FALSE)

  if (missing(ticks_per_base)) {
    ticks_per_base <- base - 1
  } else {
    if ((length(sides) != length(ticks_per_base))) {
      if (length(ticks_per_base) == 1) {
        ticks_per_base <- rep(ticks_per_base, length(sides))
      } else {
        stop("Number of ticks_per_base does not match the number of sides")
      }
    }
  }

  if (outside) {ticklength <- -1 * ticklength} else {ticklength <- ticklength}

  delog <- scale %in% "identity"

  layer(
    data = data,
    mapping = NULL,
    stat = StatIdentity,
    geom = if(geom_old) {GeomTicksOld} else {GeomTicks},
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(
      base = base,
      sides = sides,
      scaled = scaled,
      ticklength = ticklength,
      colour = colour,
      size = size,
      linetype = linetype,
      alpha = alpha,
      ticks_per_base = ticks_per_base,
      delog = delog,
      ...
    )
  )
}

GeomTicks <- ggproto(
  "GeomTicks", Geom,
  extra_params = "",
  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data,
                        panel_scales,
                        coord,
                        base = c(10, 10),
                        sides = c("b", "l"),
                        scaled = TRUE,
                        ticklength = unit(0.1, "cm"),
                        ticks_per_base = base - 1,
                        delog = c(x = TRUE, y = TRUE)) {
    ticks <- list()

    for (s in 1:length(sides)) {
      if (grepl("[b|t]", sides[s])) {

        # for ggplot2 < 3.3.0 use: xticks <- panel_params$x.minor
        if (utils::packageVersion("ggplot2") >= "3.2.1.9000") {
          x_minor_breaks <- panel_scales$x$break_positions_minor()
          x_major_breaks <- panel_scales$x$break_positions()
        } else {
          x_minor_breaks <- panel_scales$x.minor
          x_major_breaks <- panel_scales$x.major
        }

        xticks <- setdiff(x_minor_breaks, x_major_breaks)

        # Make the grobs
        if (grepl("b", sides[s])) {
          ticks$x_b <- with(
            data,
            segmentsGrob(
              x0 = unit(xticks, "npc"),
              x1 = unit(xticks, "npc"),
              y0 = unit(0, "npc"),
              y1 = ticklength,
              gp = gpar(
                col = alpha(colour, alpha),
                lty = linetype,
                lwd = size * .pt,
                lineend = "butt"
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
              y1 = unit(1, "npc") - ticklength,
              gp = gpar(
                col = alpha(colour, alpha),
                lty = linetype,
                lwd = size * .pt,
                lineend = "butt"
              )
            )
          )
        }
      }


      if (grepl("[l|r]", sides[s])) {

        # for ggplot2 < 3.3.0 use: yticks <- panel_params$y.minor
        if (utils::packageVersion("ggplot2") >= "3.2.1.9000") {
          y_minor_breaks <- panel_scales$y$break_positions_minor()
          y_major_breaks <- panel_scales$y$break_positions()
        } else {
          y_minor_breaks <- panel_scales$y.minor
          y_major_breaks <- panel_scales$y.major
        }

        yticks <- setdiff(y_minor_breaks, y_major_breaks)

        # Make the grobs
        if (grepl("l", sides[s])) {
          ticks$y_l <- with(
            data,
            segmentsGrob(
              y0 = unit(yticks, "npc"),
              y1 = unit(yticks, "npc"),
              x0 = unit(0, "npc"),
              x1 = ticklength,
              gp = gpar(
                col = alpha(colour, alpha),
                lty = linetype,
                lwd = size * .pt,
                lineend = "butt"
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
              x1 = unit(1, "npc") - ticklength,
              gp = gpar(
                col = alpha(colour, alpha),
                lty = linetype,
                lwd = size * .pt,
                lineend = "butt"
              )
            )
          )
        }
      }
    }
    gTree(children = do.call("gList", ticks))
  },
  default_aes = aes(colour = "black", size = 6 / 10, linetype = 1, alpha = 1)
)
