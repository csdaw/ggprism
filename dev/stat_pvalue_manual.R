#' @include utilities.R geom_bracket.R
#' @importFrom dplyr pull
#' @importFrom glue glue
NULL
#'Add Manually P-values to a ggplot
#'
#'@description Add manually p-values to a ggplot, such as box blots, dot plots
#'  and stripcharts. Frequently asked questions are available on \href{https://www.datanovia.com/en/blog/tag/ggpubr/}{Datanovia ggpubr FAQ page}, for example:
#'  \itemize{
#'  \item \href{https://www.datanovia.com/en/blog/ggpubr-how-to-add-adjusted-p-values-to-a-multi-panel-ggplot/}{How to Add Adjusted P-values to a Multi-Panel GGPlot}
#'  \item \href{https://www.datanovia.com/en/blog/ggpubr-how-to-add-p-values-generated-elsewhere-to-a-ggplot/}{How to Add P-Values Generated Elsewhere to a GGPLOT}
#'  \item \href{https://www.datanovia.com/en/blog/how-to-create-stacked-bar-plots-with-error-bars-and-p-values/}{How to Create Stacked Bar Plots with Error Bars and P-values}
#'  }
#'@inheritParams geom_bracket
#'@param data a data frame containing statitistical test results. The expected
#'  default format should contain the following columns: \code{group1 | group2 |
#'  p | y.position | etc}. \code{group1} and \code{group2} are the groups that
#'  have been compared. \code{p} is the resulting p-value. \code{y.position} is
#'  the y coordinates of the p-values in the plot.
#'@param label the column containing the label (e.g.: label = "p" or label =
#'  "p.adj"), where \code{p} is the p-value. Can be also an expression that can
#'  be formatted by the \code{\link[glue]{glue}()} package. For example, when
#'  specifying label = "t-test, p = \{p\}", the expression \{p\} will be
#'  replaced by its value.
#'@param y.position column containing the coordinates (in data units) to be used
#'  for absolute positioning of the label. Default value is "y.position". Can be
#'  also a numeric vector.
#'@param xmin column containing the position of the left sides of the brackets.
#'  Default value is "group1".
#'@param xmax  (optional) column containing the position of the right sides of
#'  the brackets. Default value is "group2". If NULL, the p-values are plotted
#'  as a simple text.
#'@param x x position of the p-value. Should be used only when you want plot the
#'  p-value as text (without brackets).
#'@param label.size size of label text.
#'@param bracket.size Width of the lines of the bracket.
#'@param color text and line color. Can be variable name in the data for coloring by groups.x
#'@param tip.length numeric vector with the fraction of total height that the
#'  bar goes down to indicate the precise column. Default is 0.03.
#'@param remove.bracket logical, if \code{TRUE}, brackets are removed from the
#'  plot. Considered only in the situation, where comparisons are performed
#'  against reference group or against "all".
#'@param hide.ns logical value. If TRUE, hide ns symbol when displaying
#'  significance levels. Filter is done by checking the column
#'  \code{p.adj.signif}, \code{p.signif}, \code{p.adj} and \code{p}.
#'@param position position adjustment, either as a string, or the result of a
#'  call to a position adjustment function.
#'@param ... other arguments passed to the function \code{geom_bracket()} or
#'  \code{geom_text()}
#'@seealso \code{\link{stat_compare_means}}
#'@examples
#'
#'# T-test
#'stat.test <- compare_means(
#'  len ~ dose, data = ToothGrowth,
#'  method = "t.test"
#')
#'stat.test
#'
#'# Create a simple box plot
#'p <- ggboxplot(ToothGrowth, x = "dose", y = "len")
#'p
#'
#'# Perform a t-test between groups
#'stat.test <- compare_means(
#'  len ~ dose, data = ToothGrowth,
#'  method = "t.test"
#')
#'stat.test
#'
#'# Add manually p-values from stat.test data
#'# First specify the y.position of each comparison
#'stat.test <- stat.test %>%
#'  mutate(y.position = c(29, 35, 39))
#'p + stat_pvalue_manual(stat.test, label = "p.adj")
#'
#'# Customize the label with glue expression
#'# (https://github.com/tidyverse/glue)
#'p + stat_pvalue_manual(stat.test, label = "p = {p.adj}")
#'
#'
#' # Grouped bar plots
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' ToothGrowth$dose <- as.factor(ToothGrowth$dose)
#' # Comparisons against reference
#' stat.test <- compare_means(
#'   len ~ dose, data = ToothGrowth, group.by = "supp",
#'   method = "t.test", ref.group = "0.5"
#' )
#' stat.test
#' # Plot
#' bp <- ggbarplot(ToothGrowth, x = "supp", y = "len",
#'                 fill = "dose", palette = "jco",
#'                 add = "mean_sd", add.params = list(group = "dose"),
#'                 position = position_dodge(0.8))
#' bp + stat_pvalue_manual(
#'   stat.test, x = "supp", y.position = 33,
#'   label = "p.signif",
#'   position = position_dodge(0.8)
#' )
#'
#'@export
stat_pvalue_manual <- function(
  data, label = NULL, y.position = "y.position",
  xmin = "group1", xmax = "group2", x = NULL,
  label.size = 3.2,
  bracket.size = 0.6,
  bracket.nudge.y = 0, bracket.shorten = 0, bracket.colour = NULL,
  colour = NULL, color = NULL, tip.length = 0.03,
  remove.bracket = FALSE, step.increase = 0, step.group.by = NULL,
  hide.ns = FALSE, coord.flip = FALSE,
  position = "identity", ...
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

  # filter out non-significant results if required
  if(hide.ns){
    data <- remove_ns(data)
  }

  # determine the type of comparisons: one_group, two_groups, each_vs_ref, pairwise
  comparison <- detect_comparison_type(data)

  # check if defined in function call: x, xmin, max
  all.x.is.missing <- is.null(x) & missing(xmin) & missing(xmax)

  # plot labels at x = max if conditions are met
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
  new_xmax <- xmax  # avoid re-using an existing xmax in the data
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

    params <- list(xmin = "xmin", xmax = "xmax", label = "label",
                   y.position = "y.position",
                   group = 1:nrow(data), tip.length = tip.length,
                   label.size = label.size, bracket.size = bracket.size,
                   bracket.nudge.y = bracket.nudge.y,
                   bracket.shorten = bracket.shorten, bracket.colour = bracket.colour,
                   colour = colour, color = color,
                   step.increase = step.increase, step.group.by = step.group.by,
                   coord.flip = coord.flip, position = position, ...)

    mapping <- list()
    option <- list()
    allowed.options <- c(
      # function arguments
      "y.position", "x", "label.size", "bracket.size", "bracket.nudge.y",
      "bracket.shorten", "bracket.colour", "colour", "color", "tip.length",
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
      }
    }

    if (!is.null(position)) option[["position"]] <- position

    option[["data"]] <- data

    option[["mapping"]] <- do.call(ggplot2::aes_string, mapping)

    do.call(geom_bracket, option)

  } else {
    if(comparison == "each_vs_ref"){
      ref.group <- unique(data$group1)
      group2 <- NULL

      # Add data rows used only for positioning the labels for grouped bars
      data <- add_ctr_rows(data, ref.group = ref.group)

      mapping <- ggplot2::aes(x = xmin, y = y.position, label = label, group = group2)

      if(missing(position) & !missing(x)){
          position <- ggplot2::position_dodge(0.8)
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


