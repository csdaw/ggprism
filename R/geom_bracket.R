geom_bracket <- function(mapping = NULL, data = NULL, stat = "bracket",
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE,
                         label = NULL, type = c("text", "expression"),
                         xmin = NULL, xmax = NULL, y.position = NULL,
                         label.size = 3.2, tip.length = 0.03, bracket.size = 0.6,
                         bracket.colour = NULL, bracket.shorten = 0,
                         bracket.nudge.y = 0, step.increase = 0,
                         step.group.by = NULL, coord.flip = FALSE, ...) {
  type <- match.arg(type)

  build_signif_data <- function(data = NULL,
                                label = NULL, xmin = NULL, xmax = NULL,
                                y.position = NULL, bracket.colour = NULL,
                                bracket.shorten = 0, bracket.nudge.y = 0,
                                step.increase = 0, step.group.by = NULL) {

    if (is.null(data)) {
      data <- data.frame(
        label = label, xmin = xmin, xmax = xmax,
        y.position = y.position
      )
    } else {
      if (!is.null(label)) data$label <- label
      if (!is.null(y.position)) data$y.position <- y.position
      if (!is.null(xmin)) data$xmin <- xmin
      if (!is.null(xmax)) data$xmax <- xmax
      if (!is.null(bracket.colour)) data$bracket.colour <- bracket.colour
    }

    # add columns if they don't exist
    if (!("bracket.nudge.y" %in% colnames(data))) data$bracket.nudge.y <- bracket.nudge.y
    if (!("bracket.shorten" %in% colnames(data))) data$bracket.shorten <- bracket.shorten
    if (!("bracket.colour" %in% colnames(data))) data$bracket.colour <- NA

    if (is.null(step.group.by)) {
      data <- add_step_increase(data, step.increase)
    } else {
      data <- data[order(data[[step.group.by]], data[["y.position"]]), ]

      data <- by(
        data,
        INDICES = data[[step.group.by]],
        FUN = function(x) {
          x <- add_step_increase(x, step.increase)
          return(x)
        }
      )
      data <- do.call(rbind, data)
    }
    data
  }

  data <- build_signif_data(
    data = data, label = label, xmin = xmin, xmax = xmax,
    y.position = y.position, bracket.colour = bracket.colour,
    bracket.shorten = bracket.shorten, bracket.nudge.y = bracket.nudge.y,
    step.increase = step.increase, step.group.by = step.group.by
  )

  build_signif_mapping <- function(mapping, data) {
    if (is.null(mapping)) {
      # Check if required variables are present in data
      required.vars <- c("xmin", "xmax", "y.position")
      missing.required.vars <- setdiff(required.vars, colnames(data))
      if (length(missing.required.vars) > 0) {
        stop(
          "Required variables are missing in the data: ",
          paste(missing.required.vars, collapse = ", ")
        )
      }
      mapping <- aes()
    }

    if (is.null(mapping$label)) {
      label.col <- guess_signif_label_column(data)
      data$label <- data[[label.col]]
      mapping$label <- data$label
    }

    if (is.null(mapping$xmin)) mapping$xmin <- data$xmin
    if (is.null(mapping$xmax)) mapping$xmax <- data$xmax
    if (is.null(mapping$y.position)) mapping$y.position <- data$y.position
    if (is.null(mapping$group)) mapping$group <- 1:nrow(data)
    if (is.null(mapping$step.increase)) mapping$step.increase <- data$step.increase
    if (is.null(mapping$bracket.nudge.y)) mapping$bracket.nudge.y <- data$bracket.nudge.y
    if (is.null(mapping$bracket.colour)) mapping$bracket.colour <- data$bracket.colour
    if (is.null(mapping$bracket.shorten)) mapping$bracket.shorten <- data$bracket.shorten
    if (!"x" %in% names(mapping)) {
      mapping$x <- mapping$xmin
    }
    if (!"y" %in% names(mapping)) {
      mapping$y <- mapping$y.position
    }
    mapping
  }

  mapping <- build_signif_mapping(mapping, data)

  layer(
    stat = stat, geom = GeomBracket, mapping = mapping,  data = data,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      type = type, label.size = label.size,
      tip.length = tip.length, bracket.size = bracket.size,
      na.rm = na.rm, coord.flip = coord.flip, ...
    )
  )
}

StatBracket <- ggproto("StatBracket", Stat,
                       required_aes = c("x", "y", "group"),
                       setup_params = function(data, params) {
                         if (length(params$tip.length) == 1) {
                           params$tip.length <- rep(
                             params$tip.length,
                             max(length(params$xmin), 1) * 2
                           )
                         }
                         if (length(params$tip.length) == length(params$xmin)) {
                           params$tip.length <- rep(
                             params$tip.length,
                             each=2)
                         }
                         return(params)
                       },
                       compute_group = function(data, scales, tip.length) {
                         yrange <- scales$y$range$range
                         y.scale.range <- yrange[2] - yrange[1]
                         bracket.shorten <- data$bracket.shorten / 2

                         xmin <- data$xmin + bracket.shorten
                         xmax <- data$xmax - bracket.shorten

                         y.position <- data$y.position + (y.scale.range*data$step.increase) + data$bracket.nudge.y

                         label <- data$label

                         if (is.character(xmin)) {
                           xmin <- scales$x$map(xmin)
                         }
                         if (is.character(xmax)) {
                           xmax <- scales$x$map(xmax)
                         }
                         if ("tip.length" %in% colnames(data)) {
                           tip.length <-  rep(data$tip.length, each=2)
                         }
                         # Prepare bracket data
                         data <- rbind(data, data, data)

                         data$x <- c(xmin, xmin, xmax)
                         data$xend = c(xmin, xmax, xmax)
                         data$y <- c(
                           y.position - y.scale.range * tip.length[seq_len(length(tip.length)) %% 2 == 1],
                           y.position,
                           y.position)
                         data$yend <- c(
                           y.position,
                           y.position,
                           y.position - y.scale.range * tip.length[seq_len(length(tip.length)) %% 2 == 0])
                         data$annotation <- rep(label, 3)
                         data
                       }
)

GeomBracket <- ggproto("GeomBracket", Geom,
                       required_aes = c("x", "xend", "y", "yend", "annotation"),
                       default_aes = aes(
                         label = NULL, xmin = NULL, xmax = NULL,
                         y.position = NULL,
                         label.size = 3.2, colour = "black",
                         angle = NULL, hjust = 0.5, vjust = NULL,
                         alpha = NA, fontfamily = "", fontface = 1,
                         bracket.size = 0.6, bracket.colour = NULL,
                         linetype=1, lineend = "square",
                         bracket.shorten = 0, bracket.nudge.y = 0,
                         step.increase = 0
                       ),
                       draw_key = draw_key_path,
                       draw_group = function(data, panel_params,
                                             coord, type = "text",
                                             coord.flip = FALSE) {
                         lab <- as.character(data$annotation)
                         if (type == "expression") {
                           lab <- parse_as_expression(lab)
                         }

                         coords <- coord$transform(data, panel_params)

                         if (is.null(coords$vjust)) {
                           if (lab[1] %in% c("****", "***", "**", "*")) {
                             coords$vjust <- 0.5
                           } else {
                             coords$vjust <- 0
                           }
                         }

                         label.x <- mean(c(coords$x[1], tail(coords$xend, n = 1)))
                         label.y <- max(c(coords$y, coords$yend)) + 0.01
                         label.angle <- coords$angle
                         if (coord.flip) {
                           label.y <- mean(c(coords$y[1], tail(coords$yend, n = 1)))
                           label.x <- max(c(coords$x, coords$xend)) + 0.01
                           if (is.null(label.angle)) label.angle <- -90
                         }
                         if (is.null(label.angle)) label.angle <- 0

                         gList(
                           textGrob(
                             label = lab[1],
                             x = label.x,
                             y = label.y,
                             default.units = "native",
                             hjust = coords$hjust, vjust = coords$vjust,
                             rot = label.angle,
                             gp = gpar(
                               col = alpha(coords$colour, coords$alpha),
                               fontsize = coords$label.size * .pt,
                               fontfamily = coords$fontfamily,
                               fontface = coords$fontface
                             )
                           ),
                           segmentsGrob(
                             coords$x, coords$y,
                             default.units = "native",
                             coords$xend, coords$yend,
                             gp = gpar(
                               col = if (all(!is.na(coords$bracket.colour))) {
                                 alpha(coords$bracket.colour, coords$alpha)
                               } else {
                                 alpha(coords$colour, coords$alpha)
                               }
                               ,
                               lty = coords$linetype,
                               lwd = coords$bracket.size * .pt,
                               lineend = coords$lineend
                             )
                           )
                         )
                       }
)
