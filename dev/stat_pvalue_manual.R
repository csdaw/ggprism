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
#'@param color text and line color. Can be variable name in the data for coloring by groups.
#'@param linetype linetype. Can be variable name in the data for changing linetype by groups.
#'@param tip.length numeric vector with the fraction of total height that the
#'  bar goes down to indicate the precise column. Default is 0.03.
#'@param remove.bracket logical, if \code{TRUE}, brackets are removed from the
#'  plot. Considered only in the situation, where comparisons are performed
#'  against reference group or against "all".
#'@param hide.ns logical value. If TRUE, hide ns symbol when displaying
#'  significance levels. Filter is done by checking the column
#'  \code{p.adj.signif}, \code{p.signif}, \code{p.adj} and \code{p}.
#'@param vjust move the text up or down relative to the bracket. Can be also a
#'  column name available in the data.
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
  label.size = 3.88, bracket.size = 0.3,
  bracket.nudge.y = 0, bracket.shorten = 0,
  colour = "black", linetype = 1, tip.length = 0.03,
  remove.bracket = FALSE, step.increase = 0, step.group.by = NULL,
  hide.ns = FALSE, vjust = 0, coord.flip = FALSE,
  position = "identity", ...
)
{
  if(is.null(label)){

    # Guess column to be used as significance labem
    guess_signif_label_column <- function(data) {
      potential.label <- c(
        "label", "labels", "p.adj.signif", "p.adj", "padj",
        "p.signif", "p.value", "pval", "p.val", "p"
      )
      res <- intersect(potential.label, colnames(data))
      if(length(res) > 0){
        res <- res[1]
      }
      else{
        stop("label is missing")
      }
      res
    }


    label <- guess_signif_label_column(data)
  }

  if(hide.ns){
    # remove non significant
    remove_ns <- function(data){
      columns <- colnames(data)
      if("p.adj.signif" %in% columns){
        data <- data[data$p.adj.signif != "ns", ]
      }
      else if("p.adj" %in% columns){
        data <- data[data$p.adj <= 0.05, ]
      }
      else if("p.signif" %in% columns){
        data <- data[data$p.signif != "ns", ]
      }
      else if("p" %in% columns){
        data <- data[data$p <= 0.05, ]
      }
      data
    }

    data <- remove_ns(data)
  }

  assertthat_group_columns_exists <- function(data){
    groups.exist <- all(c("group1", "group2") %in% colnames(data))
    if(!groups.exist){
      if(inherits(data, "rstatix_test") & "group" %in% colnames(data)){
        data$group1 <- "all"
        data$group2 <- data$group
      }
      else{
        stop("data should contain group1 and group2 columns")
      }
    }
    invisible(data)
  }

  data <- assertthat_group_columns_exists(data)

  # Returns the type of comparisons: one_group, two_groups, each_vs_ref, pairwise
  detect_comparison_type <- function(data){
    ngroup1 <- length(unique(data$group1))
    ngroup2 <- length(unique(data$group2))


    is_null_model <- function(data){
      group2 <- unique(data$group2)
      .diff <- setdiff(group2, "null model")
      length(.diff) == 0
    }

    if(is_null_model(data)){
      type <- "one_group"
    }
    else if(ngroup1 == 1 & ngroup2 >= 2){
      type <- "each_vs_ref"
    }
    else if(ngroup1 == 1 & ngroup2 == 1){
      type <- "two_groups"
    }
    else if (ngroup1 >= 2 & ngroup2 >= 2){
      type <- "pairwise"
    }
    else if(all(c("group1", "group2") %in% colnames(data))){
      # filtered data
      type <- "pairwise"
    }
    else{
      stop("Make sure that group1 and group2 columns exist in the data.")
    }
    type
  }

  comparison <- detect_comparison_type(data)


  all.x.is.missing <- is.null(x) & missing(xmin) & missing(xmax)

  if(all(data$group1 == "all") & all.x.is.missing){
    is.grouped <- length(data$group2) > length(unique(data$group2))
    if(!is.grouped) x <- "group2" # labels will be plotted at x = "group2"
  }

  # Detect automatically if xmin and xmax exists in the data.
  if(all.x.is.missing){
    if(all(c("xmin", "xmax") %in% colnames(data))){
      xmin <- "xmin"
      xmax <- "xmax"
    }
  }

  # should stay before (!is.null(x))
  if(remove.bracket){
    group1.length <- length(unique(data$group1))
    if(group1.length == 1) {
      xmin <- xmax
      xmax <- NULL
    }
  }

  # P-value displayed as text (without brackets)
  if(!is.null(x)){
    xmin <- x
    xmax <- NULL
  }

  # Check if a string contains curly bracket
  .contains_curlybracket <- function(x){
    grepl("\\{|\\}", x, perl = TRUE)
  }

  # If label is a glue package expression
  if(.contains_curlybracket(label)){
    # move this to importfrom later
    `%>%` <- magrittr::`%>%`

    data$label <- data %>% glue::glue_data(label)
    label <- "label"
  }

  available.variables <- colnames(data)

  if(!(label %in% available.variables))
    stop("can't find the label variable '", label, "' in the data")
  if(!(xmin %in% available.variables))
    stop("can't find the xmin variable '", xmin, "' in the data")

  # get validate p-value y-position
  .valid_y_position <- function(y.position, data){
    if(is.numeric(y.position)){
      number.of.test <- nrow(data)
      number.of.ycoord <- length(y.position)
      xtimes <- number.of.test / number.of.ycoord

      if(number.of.ycoord < number.of.test)
        y.position <- rep(y.position, xtimes)
    }
    else if(is.character(y.position)){
      if(!(y.position %in% colnames(data)))
        stop("can't find the y.position variable '", y.position, "' in the data")
    }
    return(y.position)
  }

  y.position <- .valid_y_position(y.position, data)
  if(is.numeric(y.position)){
    data$y.position <- y.position
    y.position <- "y.position"
  }

  # If xmax is null, pvalue is drawn as text
  if(!is.null(xmax)) {
    xmax <- data[[xmax]]
    pvalue.geom <- "bracket"
  }
  else {
    xmax <- NA
    pvalue.geom <- "text"
  }
  if(!is.null(xmin)){
    xmin <- data[[xmin]]
  }
  else{
    xmin <- NA
  }

  # Build the statistical table for plotting
  xxmax <- xmax  # so that mutate will avoid re-using an existing xmax in the data
  xxmin <- xmin

  data$label <- as.character(data[[label]])
  data$y.position <- data[[y.position]]
  data$xmin <- xxmin
  data$xmax <- xxmax

  # guess label default vjust
  guess_label_default_vjust <- function(label){
    if(label %in% c("****", "***", "**", "*"))
      vjust <- 0.5
    else vjust <- 0
    vjust
  }

  guess_labels_default_vjust <- function(labels){
    labels <- mapply(guess_label_default_vjust,
                     labels, USE.NAMES = FALSE)
    labels
  }

  # vjust
  if(is.character(vjust)){
    vjust <- data[[vjust]]
  }
  else if(missing(vjust)){
    vjust <- guess_labels_default_vjust(data$label)
  }

  data$vjust <- vjust

  # Draw brackets else draw p-values
  if(pvalue.geom == "bracket"){
    if(identical(data$xmin, data$xmax) | remove.bracket){
      # case when ref.group = "all"
      bracket.size = 0
    }

    params <- list(xmin = "xmin", xmax = "xmax", label = "label",
                   y.position = "y.position", vjust = "vjust",
                   group = 1:nrow(data), tip.length = tip.length,
                   label.size = label.size, bracket.size = bracket.size,
                   bracket.nudge.y = bracket.nudge.y,
                   bracket.shorten = bracket.shorten,
                   colour = colour, linetype = linetype,
                   step.increase = step.increase, step.group.by = step.group.by,
                   coord.flip = coord.flip, position = position, ...)

    mapping <- list()
    option <- list()
    allowed.options <- c(
      # general
      "x", "y", "colour", "linetype", "fill", "shape", "width",
      "alpha", "na.rm", "lwd", "pch", "cex", "position", "stat", "geom",
      "show.legend", "inherit.aes", "fontface", "family",
      # bracket specific
      "y.position", "tip.length", "label.size", "bracket.size", "step.increase",
      "bracket.nudge.y", "bracket.shorten", "coord.flip"
    )
    columns <- colnames(data)

    for (key in names(params)) {
      value <- params[[key]]
      if (is.null(value)) {

      }
      else if (unlist(value)[1] %in% columns & key %in% allowed.options) {
        mapping[[key]] <- value

      }
      else if (key %in% allowed.options) {
        option[[key]] <- value
      }
      else if(key == "step.group.by"){
        # for geom_bracket, value are variable name.
        # but this parameter is an option not an aes
        option[[key]] <- value
      }
      else if(key == "color"){
        # set color to colour
        option[["colour"]] <- value
      }
      # else warnings("Don't know '", key, "'")
    }

    if (!is.null(position)) option[["position"]] <- position

    option[["data"]] <- data

    option[["mapping"]] <- do.call(ggplot2::aes_string, mapping)

    do.call(geom_bracket, option)

  } else {
    if(comparison == "each_vs_ref"){
      ref.group <- unique(data$group1)
      group2 <- NULL

      keep_only_tbl_df_classes <- function(x){
        toremove <- setdiff(class(x), c("tbl_df", "tbl", "data.frame"))
        if(length(toremove) > 0){
          class(x) <- setdiff(class(x), toremove)
        }
        x
      }

      # For ctr rows: the comparaison of ctr against itself
      # useful only when positionning the label of grouped bars
      add_ctr_rows <- function(data, ref.group){
        xmin <- NULL
        data <- keep_only_tbl_df_classes(data)

        ctr <- data[!duplicated(data$xmin), ]
        ctr$group2 <- ref.group
        ctr$label <- " "

        rbind(ctr, data)
      }

      data <- add_ctr_rows(data, ref.group = ref.group)

      mapping <- ggplot2::aes(x = xmin, y = y.position, vjust = vjust, label = label, group = group2)

      is_grouping_variable <- function(x){
        !(x %in% c("group1", "group2"))
      }

      if(missing(position) & !missing(x)){
        if (is_grouping_variable(x))
          position <- ggplot2::position_dodge(0.8)
      }
    }
    else{
      mapping <- aes(x = xmin, y = y.position, vjust = vjust, label = label)
    }
    option <- list(data = data, size = label.size, position = position, ...)
    if(color %in% colnames(data)) mapping$colour <- rlang::ensym(color)
    else option$color <- color
    option[["mapping"]] <- mapping
    do.call(ggplot2::geom_text, option)
  }
}
