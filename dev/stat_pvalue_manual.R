#' Title
#'
#' Description.
#'
#' @param data Description.
#' @param label Description.
#' @param xmin Description.
#' @param xmax Description.
#' @param x Description.
#' @param y.position Description.
#' @param label.size Description.
#' @param colour Description.
#' @param color Description.
#' @param tip.length Description.
#' @param bracket.size Description.
#' @param bracket.colour Description.
#' @param bracket.shorten Description.
#' @param bracket.nudge.y Description.
#' @param step.increase Description.
#' @param step.group.by Description.
#' @param remove.bracket Description.
#' @param coord.flip Description.
#' @param position Description.
#' @param ... Description.
#'
#' @return Description.
#' @export
#'
#' @examples
#' #
#'
stat_pvalue_manual <- function(
  data, label = NULL, xmin = "group1", xmax = "group2",
  x = NULL, y.position = "y.position",
  label.size = 3.2,  colour = NULL, color = NULL,
  tip.length = 0.03, bracket.size = 0.6, bracket.colour = NULL,
  bracket.shorten = 0, bracket.nudge.y = 0, step.increase = 0,
  step.group.by = NULL, remove.bracket = FALSE,
  coord.flip = FALSE, position = "identity", ...
)
{
  # if label is missing, guess the column to use for significance label
  if(is.null(label)){
    label <- guess_signif_label_column(data)
  }

  # if label is a glue package expression, parse it
  if(grepl("\\{|\\}", label, perl = TRUE)){
    data$label <- glue::glue_data(data, label)
    label <- "label"
  }

  # check that xmin and label columns are in data
  if(!(label %in% colnames(data)))
    stop("can't find the label variable '", label, "' in the data")
  if(!(xmin %in% colnames(data)))
    stop("can't find the xmin variable '", xmin, "' in the data")

  # check if defined in function call: x, xmin, max
  all.x.is.missing <- is.null(x) & missing(xmin) & missing(xmax)

  # plot labels at x = max if conditions are met
  # uses default xmin = "group1" and xmax = "group2"
  if(all(data[[xmin]] == "all") & all.x.is.missing){
    is.grouped <- length(data[[xmax]]) > length(unique(data[[xmax]]))
    if(!is.grouped) x <- xmax
  }

  # check for remove.bracket
  # should stay before (!is.null(x))
  if(remove.bracket){
    xmin.length <- length(unique(data[[xmin]]))
    if(xmin.length == 1) {
      xmin <- xmax
      xmax <- NULL
    }
  }

  # determine the type of comparisons: one_group, two_groups, each_vs_ref, pairwise
  # should stay before (!is.null(x))
  ngroup1 <- length(unique(data[[xmin]]))

  if(!is.null(xmax)) {
    ngroup2 <- length(unique(data[[xmax]]))

    if(length(setdiff(unique(data[[xmax]]), "null model")) == 0){
      comparison <- "one_group"
    }
    else if(ngroup1 == 1 & ngroup2 >= 2){
      comparison <- "each_vs_ref"
    }
    else if(ngroup1 == 1 & ngroup2 == 1){
      comparison <- "two_groups"
    }
    else if (ngroup1 >= 2 & ngroup2 >= 2){
      comparison <- "pairwise"
    }
    else{
      stop("Make sure that xmin and xmax columns exist in the data.")
    }
  } else {
    if(ngroup1 >= 2) {
      comparison <- "two_groups"
    }
  }

  # only for p-value displayed as text (without brackets)
  if(!is.null(x)){
    xmin <- x
    xmax <- NULL
  }

  # validate p-value y position
  y.position <- validate_y_position(y.position, data)

  if(is.numeric(y.position)){
    data$y.position <- y.position
    y.position <- "y.position"
  }

  # if xmax is null, p-value is drawn as text, otherwise draw brackets
  if(!is.null(xmax)) {
    xmax <- data[[xmax]]
    pvalue.geom <- "bracket"
  }
  else {
    xmax <- NA
    pvalue.geom <- "text"
  }

  # build the statistical table for plotting
  # avoid re-using an existing xmin or xmax column in the data
  new_xmax <- xmax
  new_xmin <- data[[xmin]]

  data$label <- as.character(data[[label]])
  data$y.position <- data[[y.position]]
  data$xmin <- new_xmin
  data$xmax <- new_xmax

  # draw brackets else draw p-value text
  if(pvalue.geom == "bracket"){
    if(identical(data$xmin, data$xmax) | remove.bracket){
      bracket.size = 0
    }

    params <- list(group = 1:nrow(data),
                   label = "label", xmin = "xmin", xmax = "xmax",
                   y.position = "y.position", label.size = label.size,
                   colour = colour, color = color,
                   tip.length = tip.length, bracket.size = bracket.size,
                   bracket.colour = bracket.colour,
                   bracket.shorten = bracket.shorten,
                   bracket.nudge.y = bracket.nudge.y,
                   step.increase = step.increase, step.group.by = step.group.by,
                   coord.flip = coord.flip, position = position, ...)

    mapping <- list()
    option <- list()
    allowed.options <- c(
      # function arguments
      "y.position", "x", "label.size", "colour", "color", "tip.length",
      "bracket.size","bracket.colour", "bracket.shorten", "bracket.nudge.y",
      "step.increase", "coord.flip", "position",
      # extra aesthetics
      "hjust", "vjust", "linetype", "lineend",
      "fontface", "fontfamily", "alpha",
      # ggplot2 arguments
      "show.legend", "inherit.aes", "na.rm"
    )

    columns <- colnames(data)

    for (key in names(params)) {
      value <- params[[key]]
      if (is.null(value)) {
        # do nothing
      }
      else if (unlist(value)[1] %in% columns & key %in% allowed.options) {
        mapping[[key]] <- value
      }
      else if (key %in% allowed.options) {
        option[[key]] <- value
      }
      else if(key == "step.group.by"){
        # for geom_bracket, value are variable name
        # but this parameter is an option not an aes
        option[[key]] <- value
      } else if (key == "bracket.color" & missing(bracket.colour)) {
        option[["bracket.colour"]] <- value
      }
    }

    if (!is.null(position)) option[["position"]] <- position

    option[["data"]] <- data

    option[["mapping"]] <- do.call(ggplot2::aes_string, mapping)

    do.call(geom_bracket, option)

  } else {
    if(comparison == "each_vs_ref"){
      ref.group <- unique(data[[xmin]])
      group2 <- NULL

      # Add data rows used only for positioning the labels for grouped bars
      data <- add_ctr_rows(data, ref.group = ref.group)

      mapping <- ggplot2::aes(x = xmin, y = y.position,
                              label = label, group = group2)

      if(missing(position) & !missing(x)){
          position <- ggplot2::position_dodge(width = 0.8)
      }
    }
    else{
      mapping <- aes(x = xmin, y = y.position, label = label)
    }
    option <- list(data = data, size = label.size, position = position, ...)

    if(!missing(color)) {
      if(color %in% colnames(data)) {
        mapping$colour <- rlang::ensym(color)
      } else {
        option$colour <- color
      }
    }
    if(!missing(colour)) {
      if(colour %in% colnames(data)) {
        mapping$colour <- rlang::ensym(colour)
      } else {
        option$colour <- colour
      }
    }

    option[["mapping"]] <- mapping

    do.call(ggplot2::geom_text, option)
  }
}


