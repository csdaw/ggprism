geom_pvalue <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            ...,
                            tip.length = 0.03,
                            bracket.shorten = 0,
                            bracket.nudge.y = 0,
                            bracket.colour = NULL,
                            bracket.color = NULL,
                            label.colour = NULL,
                            label.color = NULL,
                            step.group.by = NULL,
                            step.increase = 0,
                            remove.bracket = FALSE,
                            parse = FALSE,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {

  # Put backwards compatibility code here for now... but should go in add_pvalue
  aaa <- "banana"
  bbb <- mapping$label
  # browser()

  # if label is a glue package expression, parse it
  # bracket_labels <- data[[rlang::as_name(mapping$label)]]

  # TODO: make sure step.increase is a single number?

  if (!is.null(bracket.color)) {
    if (!is.null(bracket.colour)) {
      warning("Use bracket.colour or bracket.color but not both.")
    } else {
      bracket.colour <- bracket.color
    }
  }

  if (!is.null(label.color)) {
    if (!is.null(label.colour)) {
      warning("Use label.colour or label.color but not both.")
    } else {
      label.colour <- label.color
    }
  }

  if (length(mapping$label) == 1 && inherits(mapping$label, "character")) {
    data$label <- glue::glue_data(data, mapping$label)
    mapping$label <- quo(`label`)
  }

  if (remove.bracket & (is.null(mapping$xend) & is.null(mapping$yend))) {
    stop("If remove.bracket = TRUE, then the `xend` or `yend` aesthetic must be defined.")
  }
  else if (remove.bracket & !is.null(mapping$xend)) {
    mapping$x <- mapping$xend
    mapping$xend <- NULL
  }
  else if (remove.bracket & !is.null(mapping$yend)) {
    mapping$y <- mapping$yend
    mapping$yend <- NULL
  }

  # # check for remove.bracket
  # # should stay before (!is.null(x))
  # if (remove.bracket & !is.null(xmax)) {
  #   xmin.length <- length(unique(data[[xmin]]))
  #   xmax.length <- length(unique(data[[xmax]]))
  #   if (xmin.length == 1 & xmax.length >= 2) {
  #     xmin <- xmax
  #     xmax <- NULL
  #   }
  #   else if (xmin.length >= 2 & xmax.length == 1) {
  #     xmax <- NULL
  #   }
  # }


  layer(
    data = data,
    mapping = mapping,
    stat = StatPValue,
    geom = GeomPValue,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      tip.length = tip.length,
      bracket.shorten = bracket.shorten,
      bracket.nudge.y = bracket.nudge.y,
      bracket.colour = bracket.colour,
      label.colour = label.colour,
      step.group.by = step.group.by,
      step.increase = step.increase,
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

StatPValue <- ggproto("StatPValue", Stat,
                      setup_params = function(data, params) {
                        params$flipped_aes <- has_flipped_aes(
                          data,
                          params,
                          main_is_orthogonal = FALSE,
                          range_is_orthogonal = TRUE
                        )

                        params$n_comparisons <- nrow(data)

                        params
                      },
                      setup_data = function(data, params) {
                        # Move these from params to data so they are split into groups properly!
                        data$bracket.shorten <- params$bracket.shorten / 2
                        data$bracket.nudge.y <- params$bracket.nudge.y
                        data
                      },
                      compute_group = function(data, scales,
                                               flipped_aes = FALSE,
                                               n_comparisons = NULL,
                                               step.group.by = NULL,
                                               step.increase = 0) {
                        # browser()
                        aaa <- "aaa"
                        # browser()
                        data <- flip_data(data, flipped_aes)

                        if (flipped_aes) {
                          y.scale.range <- scales$x$range$range[2] - scales$x$range$range[1]
                        } else {
                          y.scale.range <- scales$y$range$range[2] - scales$y$range$range[1]
                        }

                        data$x <- data$x + data$bracket.shorten
                        data$y <- data$y + data$bracket.nudge.y

                        if (step.increase > 0) {
                          if (is.null(step.group.by)) {
                            data <- add_step_increase(data, n_comparisons = n_comparisons, y.scale.range * step.increase)
                          } else {
                            # data$supp <- rep(1:2, each = 3)
                            step.increase <- y.scale.range * step.increase
                            # step.group.by = "supp"
                            data <- data[order(data$group, data$y), ]

                            data <- by(
                              data,
                              INDICES = data$group,
                              FUN = function(x) {
                                x <- add_step_increase(x, n_comparisons = nrow(data), step.increase)
                                return(x)
                              }
                            )
                            data <- do.call(rbind, data)
                          }
                        }

                        if (!is.null(data$xend)) {
                          data$xend <- data$xend - data$bracket.shorten
                        } else {
                          # data2$x <- (data$xend + data$x) / 2
                          # data2$y <- data2$y + (y.range * 0.01)
                        }

                        aaa <- "banana"
                        flip_data(data, flipped_aes)
                      },
                      required_aes = c("x", "y", "label"),

                      optional_aes = c("xend", "yend"),

                      extra_params = c("bracket.shorten", "bracket.nudge.y", "step.group.by", "step.increase", "na.rm"),



)

GeomPValue <- ggproto("GeomPValue", Geom,

                      required_aes = c("x", "y", "label"),

                      optional_aes = c("xend", "yend"),

                      default_aes = aes(
                        colour = "black",
                        fontface = 1,
                        fontfamily = "sans",
                        linewidth = 0.6,
                        linetype = 1L,
                        lineend = "butt",
                        alpha = NA,
                        size = 3.2,
                        hjust = 0.5,
                        vjust = 0,
                        angle = 0
                      ),

                      extra_params = c("tip.length", "bracket.shorten", "bracket.nudge.y", "bracket.colour", "label.colour", "step.group.by", "step.increase", "parse", "orientation", "na.rm"),

                      setup_params = function(data, params) {
                        params$flipped_aes <- has_flipped_aes(data, params, main_is_continuous = FALSE)
                        # browser()
                        banana <- "aaa"
                        params
                      },

                      setup_data = function(data, params) {
                        aaa <- "banana"
                        # browser()
                        # data$vjust[data$label %in% c("*", "**", "***", "****")] <- 0.5
                        data
                      },

                      draw_panel = function(data, panel_params, coord, tip.length = 0.03,
                                            bracket.shorten = 0, bracket.nudge.y = 0,
                                            bracket.colour = NULL, label.colour = NULL,
                                            step.group.by = NULL, step.increase = 0,
                                            parse = FALSE,
                                            flipped_aes = FALSE,
                                            na.rm = FALSE) {
                        # browser()
                        coord_flipped <- inherits(coord, "CoordFlip")

                        data <- flip_data(data, flipped_aes)
                        # browser()

                        if (flipped_aes | coord_flipped) {
                          y.range <- panel_params$x.range[2] - panel_params$x.range[1]
                          y.scale.range <- panel_params$x$scale$get_limits()[2] - panel_params$x$scale$get_limits()[1]
                          direction <- "up"
                          data$angle[data$angle == 0] <- 270
                        } else {
                          y.range <- panel_params$y.range[2] - panel_params$y.range[1]
                          y.scale.range <- panel_params$y$scale$get_limits()[2] - panel_params$y$scale$get_limits()[1]
                          direction <- "down"
                        }

                        aaa <- "banana"
                        # browser()


                        # bracket.shorten <- bracket.shorten / 2
                        # data$x <- data$x + bracket.shorten
                        # if (!is.null(data$xend)) data$xend <- data$xend - bracket.shorten
                        # browser()



                        # browser()

                        # data$y <- data$y + bracket.nudge.y

                        # if (is.null(step.group.by)) {
                        #   data <- add_step_increase(data, y.scale.range * step.increase)
                        # } else {
                        #   # data$supp <- rep(1:2, each = 3)
                        #   step.increase <- y.scale.range * step.increase
                        #   # step.group.by = "supp"
                        #   data <- data[order(data$group, data$y), ]
                        #
                        #   data <- by(
                        #     data,
                        #     INDICES = data$group,
                        #     FUN = function(x) {
                        #       x <- add_step_increase(x, step.increase)
                        #       return(x)
                        #     }
                        #   )
                        #   data <- do.call(rbind, data)
                        # }








                        data$vjust[data$label %in% c("*", "**", "***", "****")] <- 0.5

                        data2 <- data
                        if (!is.null(data2$fontfamily)) {
                          data2$family <- data2$fontfamily
                        }
                        # data2$angle <- 0

                        if (!is.null(label.colour)) {
                          data2$colour <- label.colour
                        }

                        if (!is.null(data2$xend)) {
                          data2$x <- (data$xend + data$x) / 2
                          data2$y <- data2$y + (y.range * 0.01)
                          data2 <- flip_data(data2, flipped_aes)
                          text_grob <- GeomText$draw_panel(
                            data2, panel_params, coord,
                            parse = parse, na.rm = na.rm
                          )
                        } else {
                          data2$vjust[data$vjust == 0] <- 0.5
                          data2 <- flip_data(data2, flipped_aes)
                          text_grob <- GeomText$draw_panel(
                            data2, panel_params, coord,
                            parse = parse, na.rm = na.rm
                          )
                          return(text_grob)
                        }

                        data3 <- data


                        # browser()

                        if (!is.null(bracket.colour)) {
                          data3$colour <- bracket.colour
                        }




                        data3$yend <- data3$y

                        data3 <- flip_data(data3, flipped_aes)

                        bracket_grob <- GeomBracket$draw_panel(
                          data3, panel_params, coord,
                          tip.length = tip.length,
                          direction = direction,
                          na.rm = FALSE
                        )

                        # if (is.null(data[[flipped_names(flipped_aes)$y]]))
                        #   return(line_grob)

                        ggplot2:::ggname("geom_pvalue",
                                         gTree(children = gList(
                                           text_grob,
                                           bracket_grob
                                         ))
                        )
                      }
)
