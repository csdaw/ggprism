#' Add p-values to a ggplot
#'
#' @description Add p-values with or without brackets to a ggplot.
#'
#' See [here](https://csdaw.github.io/ggprism/articles/pvalues.html)
#' or the examples section below for examples of how to use.
#'
#' `add_pvalue` is a refactored version of
#' `stat_pvalue_manual` from
#' [kassambara/ggpubr](https://github.com/kassambara/ggpubr), altered to
#' have less dependencies, and more flexibility with input format and
#' aesthetics. Any examples using `stat_pvalue_manual` found on
#' [Datanovia](https://www.datanovia.com/en/?s=p-value&search-type=default)
#' will also work with `add_pvalue`.
#'
#' @param data A `data.frame` with the statistics to plot. Default format
#' has the following columns: `group1 | group2 | p.adj | y.position | etc`.
#' `group1` and `group2` are the two groups that were compared.
#' `p.adj` is the adjusted p-value. `y.position` is the y coordinate
#' that specifies where on the plot the p-value should go. The column names
#' can differ from the default as long as their are specified when calling
#' the function.
#' @param label `string`. Name of column in `data` that
#' contains the text to plot (e.g. `label = "p.adj"`).
#' Can also be an expression that can be formatted by
#' \code{\link[glue]{glue}} (e.g. `label = "p = {p.adj}"`).
#' @param xmin `string`. Name of column in `data`  that
#' contains the position of the left side of the brackets.
#' Default is `"group1"`.
#' @param xmax Optional. `string`. Name of column in `data` that
#' contains the position of the right side of the brackets.
#' Default is `"group2"`. If `NULL`, the p-values are plotted as
#' text only.
#' @param x `string` or `numeric`. x coordinate of the p-value text.
#' Only use when plotting p-value as text only (no brackets).
#' @param y.position `string`. Name of column in
#' \code{data} containing the y coordinates (`numeric`) of each p-value to
#' be plotted. Can also be a single number to plot all p-values at the same
#' height or a `numeric` vector that will override `data`.
#' @param label.size `numeric`. Size of text.
#' @param colour,color `string`. Colour of text.
#' @param tip.length `numeric` vector. Length of bracket tips.
#' Use `0` to remove tips.
#' @param bracket.size `numeric`. Line width of bracket.
#' @param bracket.colour,bracket.color `string`. Colour of bracket. Default is
#' `NULL` which causes brackets to inherit the colour of the text.
#' @param bracket.shorten `numeric`. Shortens the brackets slightly to
#' allow them to be plotted side-by-side at the same y position.
#' @param bracket.nudge.y `numeric`. Changes the y position of
#' p-values. Useful for slightly adjusting p-values if the text is cut off.
#' @param step.increase `numeric`. Changes the space between brackets.
#' @param step.group.by `string`. Variable to group brackets by.
#' @param remove.bracket `logical`. If `TRUE` all brackets are
#' removed and p-value is shown as text only.
#' @param coord.flip `logical`. If `TRUE` p-values are rotated by
#' 90 degrees. Should be used with \code{\link[ggplot2]{coord_flip}}
#' @param position `string` or call to position function such as
#' \code{\link[ggplot2]{position_dodge}}. Typically used for adjusting x
#' position of p-values to be in line with dodged data.
#' @param ... Additional aesthetics or arguments passed to
#' \code{\link[ggplot2]{layer}}. See below for allowed values.
#'
#' @return Returns a _layer_ ggproto object with either `geom = GeomBracket` or
#' `geom = GeomText`.
#'
#' @section Allowed ... values:
#' `add_pvalue` understands the following additional aesthetics or arguments:
#'
#' \describe{
#'   \item{`fontface`}{`string`. Fontface of text (e.g. `"bold"`).}
#'   \item{`fontfamily`}{`string`. Fontfamily of text (e.g. `"Arial"`).}
#'   \item{`hjust`}{`numeric`. Horizontal justification of text.}
#'   \item{`vjust`}{`numeric`. Vertical justification of text.}
#'   \item{`alpha`}{`numeric`. Transparency of text and/or brackets.}
#'   \item{`linetype`}{`string` or `numeric`. Linetype of brackets
#'    (e.g. `"dashed"`).}
#'   \item{`lineend`}{`string`. Lineend of brackets (e.g. `"butt"`).}
#'   \item{`na.rm`}{`logical`. If `FALSE` (default), removes
#'   missing values with a warning. If `TRUE` silently removes missing
#'   values.}
#'   \item{`show.legend`}{`logical`. Should this layer be included in
#'   the legends? If `NA` (default), include if any aesthetics are mapped.
#'   If `FALSE`, never include or if `TRUE`, always include. It can
#'   also be a named `logical` vector to finely select the aesthetics to
#'   display.}
#'   \item{`inherit.aes`}{`logical`. If `FALSE`, overrides the
#'   default aesthetics, rather than combining with them.}
#' }
#'
#' @example inst/examples/ex-add_pvalue.R
#'
#' @export
add_pvalue <- function(data,
                       label = NULL, xmin = "group1", xmax = "group2",
                       x = NULL, y.position = "y.position",
                       label.size = 3.2,  colour = NULL, color = NULL,
                       tip.length = 0.03, bracket.size = 0.6,
                       bracket.colour = NULL, bracket.color = NULL,
                       bracket.shorten = 0,
                       bracket.nudge.y = 0, step.increase = 0,
                       step.group.by = NULL, remove.bracket = FALSE,
                       coord.flip = FALSE, position = "identity", ...) {
  # if label is missing, guess the column to use for significance label
  if (is.null(label)) {
    label <- guess_signif_label_column(data)
  }

  # if label is a glue package expression, parse it
  if (grepl("\\{|\\}", label, perl = TRUE)) {
    data$label <- glue_data(data, label)
    label <- "label"
  }

  # check that xmin and label columns are in data
  if (!(label %in% colnames(data)))
    stop("can't find the label variable '", label, "' in the data")
  if (!(xmin %in% colnames(data)))
    stop("can't find the xmin variable '", xmin, "' in the data")

  # check if defined in function call: x, xmin, max
  all.x.is.missing <- is.null(x) & missing(xmin) & missing(xmax)

  # plot labels at x = max if conditions are met
  # uses default xmin = "group1" and xmax = "group2"
  if (all(data[[xmin]] == "all") & all.x.is.missing) {
    is.grouped <- length(data[[xmax]]) > length(unique(data[[xmax]]))
    if (!is.grouped) x <- xmax
  }

  # check for remove.bracket
  # should stay before (!is.null(x))
  if (remove.bracket & !is.null(xmax)) {
    xmin.length <- length(unique(data[[xmin]]))
    xmax.length <- length(unique(data[[xmax]]))
    if (xmin.length == 1 & xmax.length >= 2) {
      xmin <- xmax
      xmax <- NULL
    }
    else if (xmin.length >= 2 & xmax.length == 1) {
      xmax <- NULL
    }
  }

  # determine the type of comparisons: one_group, two_groups, each_vs_ref, pairwise
  # should stay before (!is.null(x))
  ngroup1 <- length(unique(data[[xmin]]))

  if (!is.null(xmax)) {
    ngroup2 <- length(unique(data[[xmax]]))

    if (length(setdiff(unique(data[[xmax]]), "null model")) == 0) {
      comparison <- "one_group"
    }
    else if (ngroup1 == 1 & ngroup2 >= 2) {
      comparison <- "each_vs_ref"
    }
    else if (ngroup1 == 1 & ngroup2 == 1) {
      comparison <- "two_groups"
    }
    else if (ngroup1 >= 2 & ngroup2 >= 2) {
      comparison <- "pairwise"
    }
    else if (ngroup1 >= 2 & ngroup2 == 1) {
      comparison <- "each_vs_ref"
    }
  } else {
    if(ngroup1 >= 1) {
      comparison <- "two_groups"
    }
  }

  # only for p-value displayed as text (without brackets)
  if (!is.null(x)) {
    x <- validate_x_position(x, data)

    if (is.numeric(x)) {
      data$x <- x
      x <- "x"
    }

    xmin <- x
    xmax <- NULL
  }

  # validate p-value y position
  y.position <- validate_y_position(y.position, data)

  if (is.numeric(y.position)) {
    data$y.position <- y.position
    y.position <- "y.position"
  }

  # if xmax is null, p-value is drawn as text, otherwise draw brackets
  if (!is.null(xmax)) {
    xmax <- data[[xmax]]
    pvalue.geom <- "bracket"
  } else {
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
  if (pvalue.geom == "bracket") {
    params <- list(
      group = 1:nrow(data),
      label = "label", xmin = "xmin", xmax = "xmax",
      y.position = "y.position", label.size = label.size,
      colour = colour, color = color,
      tip.length = tip.length, bracket.size = bracket.size,
      bracket.colour = bracket.colour, bracket.color = bracket.color,
      bracket.shorten = bracket.shorten,
      bracket.nudge.y = bracket.nudge.y,
      step.increase = step.increase, step.group.by = step.group.by,
      coord.flip = coord.flip, position = position, ...
    )

    mapping <- list()
    option <- list()
    allowed.options <- c(
      # function arguments
      "y.position", "x", "label.size", "colour", "tip.length",
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
      else if (key == "step.group.by") {
        # for geom_bracket, value are variable name
        # but this parameter is an option not an aes
        option[[key]] <- value
      }
      else if (key == "color") {
        if (missing(colour)) {
          option[["colour"]] <- value
        } else {
          warn("Use colour or color but not both.")
        }
      }
      else if (key == "bracket.color") {
        if (missing(bracket.colour)) {
          option[["bracket.colour"]] <- value
        } else {
          warn("Use bracket.colour or bracket.color but not both.")
        }
      }
    }

    if (!is.null(position)) option[["position"]] <- position

    option[["data"]] <- data

    option[["mapping"]] <- do.call(aes_string, mapping)

    do.call(geom_bracket, option)

  } else {
    if (comparison == "each_vs_ref") {
      ref.group <- unique(data[[xmin]])
      group2 <- NULL

      # Add data rows used only for positioning the labels for grouped bars
      data <- add_ctr_rows(data, ref.group = ref.group)

      mapping <- aes(x = xmin, y = y.position,
                     label = label, group = group2)

      if (missing(position) & !missing(x)) {
        position <- position_dodge(width = 0.8)
      }
    } else {
      mapping <- aes(x = xmin, y = y.position, label = label)
    }

    option <- list(data = data, size = label.size, position = position, ...)

    if (!missing(color)) {
      if (color %in% colnames(data)) {
        mapping$colour <- ensym(color)
      } else {
        option$colour <- color
      }
    }

    if (!missing(colour)) {
      if (colour %in% colnames(data)) {
        mapping$colour <- ensym(colour)
      } else {
        option$colour <- colour
      }
    }

    option[["mapping"]] <- mapping

    do.call(geom_text, option)
  }
}


