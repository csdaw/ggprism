#' Add p-values to a ggplot
#'
#' @description Add p-values with or without brackets to a ggplot.
#'
#' See [here](https://csdaw.github.io/ggprism/articles/pvalues.html)
#' or the examples section below for examples of how to use.
#'
#' `add_pvalue` is a heavily re-written version of
#' `stat_pvalue_manual` from
#' [kassambara/ggpubr](https://github.com/kassambara/ggpubr). Any examples
#' using `stat_pvalue_manual` found on
#' [Datanovia](https://www.datanovia.com/en/?s=p-value&search-type=default)
#' should also work with `add_pvalue` with minimal to no alterations.
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
#' @param parse `logical`. Default is `FALSE`. If `TRUE` the text labels will
#' be parsed into expressions and displayed as described in [grDevices::plotmath].
#' @param label.size `numeric`. Size of text. Default is `3.2`.
#' @param colour,color `string`. Colour of text. Default is `"black"`.
#' @param tip.length `numeric` vector. Length of bracket tips. Default is `0.03`,
#' use `0` to remove tips.
#' @param bracket.size `numeric`. Line width of bracket. Default is `0.6`.
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
#' @param coord.flip `logical`. This argument is deprecated.
#' @param position `string` or call to position function such as
#' \code{\link[ggplot2]{position_dodge}}. Typically used for adjusting x
#' position of p-values to be in line with dodged data.
#' @param ... Additional aesthetics or arguments passed to
#' \code{\link[ggplot2]{layer}}. See below for allowed values.
#'
#' @return Returns a ggplot2 `Layer` object that can be added to a plot.
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
                       x = NULL, y.position = "y.position", parse = FALSE,
                       label.size = 3.2,  colour = NULL, color = NULL,
                       tip.length = 0.03, bracket.size = 0.6,
                       bracket.colour = NULL, bracket.color = NULL,
                       bracket.shorten = 0,
                       bracket.nudge.y = 0, step.increase = 0,
                       step.group.by = NULL, remove.bracket = FALSE,
                       coord.flip = FALSE, position = "identity", ...) {
  if (!is.null(color)) {
    colour <- color
  }

  ## Input checking ...

  if (coord.flip) {
    message("The `coord.flip = TRUE` argument is no longer necessary and can be removed.")
  }

  # if label is missing, guess the column to use for significance label
  if (is.null(label)) {
    label <- guess_signif_label_column(data)
  }

  # if label is a glue package expression, parse it
  if (grepl("\\{|\\}", label, perl = TRUE)) {
    data$label <- glue::glue_data(data, label)
    label <- "label"
  }

  if (all(data[[xmin]] == "all")) {
    remove.bracket <- TRUE
  }

  mapping <- aes(
    x = .data[[xmin]],
    xend = .data[[xmax]],
    y = .data[[y.position]],
    label = .data[[label]]
  )

  if (!is.null(x)) {
    x <- validate_x_position(x, data)

    if (is.numeric(x)) {
      data$xend <- x
      mapping$xend <- quo(`xend`)
    } else {
      mapping$x <- quo(.data[[x]])
      mapping$xend <- NULL
    }
  }

  # validate p-value y position
  y.position <- validate_y_position(y.position, data)

  if (is.numeric(y.position)) {
    data$y.position <- y.position
    mapping$y <- quo(`y.position`)
  }

  if (!is.null(step.group.by)) {
    mapping$group <- quo(.data[[step.group.by]])
  }

  if (!is.null(colour)) {
    if (colour %in% colnames(data)) {
      mapping$colour <- quo(.data[[colour]])
      colour <- NULL
    }
  }

  # Call geom_add_pvalue with appropriate inputs...
  geom_pvalue(
    mapping = mapping,
    data = data,
    position = "identity",
    ...,
    parse = parse,
    label.colour = colour,
    size = label.size,
    tip.length = tip.length,
    linewidth = bracket.size,
    bracket.colour = bracket.colour,
    bracket.color = bracket.color,
    bracket.shorten = bracket.shorten,
    bracket.nudge.y = bracket.nudge.y,
    step.increase = step.increase,
    step.group.by = step.group.by,
    remove.bracket = remove.bracket
  )
}
