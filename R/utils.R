#### Define custom elements upon package load ----------------------------------
.onLoad <- function(libname, pkgname) {
  register_theme_elements(
    prism.ticks.length            = unit(2, "pt"),
    element_tree = list(
      prism.ticks.length          = el_def("unit"),
      prism.ticks.length.x        = el_def("unit", "prism.ticks.length"),
      prism.ticks.length.x.top    = el_def("unit", "prism.ticks.length.x"),
      prism.ticks.length.x.bottom = el_def("unit", "prism.ticks.length.x"),
      prism.ticks.length.y        = el_def("unit", "prism.ticks.length"),
      prism.ticks.length.y.left   = el_def("unit", "prism.ticks.length.y"),
      prism.ticks.length.y.right  = el_def("unit", "prism.ticks.length.y")
    )
  )
}

#### Helper functions for stat_pvalue_manual -----------------------------------

# Guess the column to be used as the significance labels
guess_signif_label_column <- function(data) {
  potential.label <- c(
    "label", "labels", "p.adj.signif", "p.adj", "padj",
    "p.signif", "p.value", "pval", "p.val", "p"
  )
  res <- intersect(potential.label, colnames(data))
  if (length(res) > 0) {
    res <- res[1]
  } else {
    stop("label is missing")
  }
  res
}

# Validate p-value x position
validate_x_position <- function(x, data) {
  if (is.numeric(x)) {
    number.of.test <- nrow(data)
    number.of.xcoord <- length(x)
    xtimes <- number.of.test / number.of.xcoord

    if (number.of.xcoord < number.of.test) x <- rep(x, xtimes)

  } else if (is.character(x)) {
    if (!(x %in% colnames(data)))
      stop("can't find the x variable '", x, "' in the data")
  }
  return(x)
}

# Validate p-value y position
validate_y_position <- function(y.position, data) {
  if (is.numeric(y.position)) {
    number.of.test <- nrow(data)
    number.of.ycoord <- length(y.position)
    xtimes <- number.of.test / number.of.ycoord

    if (number.of.ycoord < number.of.test) y.position <- rep(y.position, xtimes)

  } else if (is.character(y.position)) {
    if (!(y.position %in% colnames(data)))
      stop("can't find the y.position variable '", y.position, "' in the data")
  }
  return(y.position)
}

keep_only_tbl_df_classes <- function(x) {
  to.remove <- setdiff(class(x), c("tbl_df", "tbl", "data.frame"))
  if (length(to.remove) > 0) {
    class(x) <- setdiff(class(x), to.remove)
  }
  x
}

# For control rows: the comparison of control against itself
# Used only when positioning the labels for grouped bars
add_ctr_rows <- function(data, ref.group) {
  xmin <- NULL
  data <- keep_only_tbl_df_classes(data)

  ctr <- data[!duplicated(data$xmin), ]
  ctr$group2 <- ref.group
  ctr$label <- " "

  rbind(ctr, data)
}

#### Helper functions for geom_bracket -----------------------------------------

# Add increments to bracket height
add_step_increase <- function(data, step.increase) {
  comparisons.number <- 0:(nrow(data) - 1)
  step.increase <- step.increase * comparisons.number
  data$step.increase <- step.increase
  data
}

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

#### Copies of internal ggplot2 functions -----------------------------------

absoluteGrob <- function(grob, width = NULL, height = NULL,
                         xmin = NULL, ymin = NULL, vp = NULL) {

  gTree(
    children = grob,
    width = width, height = height,
    xmin = xmin, ymin = ymin,
    vp = vp, cl = "absoluteGrob"
  )
}

draw_axis_labels <- function(break_positions, break_labels, label_element, is_vertical,
                             check.overlap = FALSE) {

  position_dim <- if (is_vertical) "y" else "x"
  label_margin_name <- if (is_vertical) "margin_x" else "margin_y"

  n_breaks <- length(break_positions)
  break_positions <- unit(break_positions, "native")

  if (check.overlap) {
    priority <- axis_label_priority(n_breaks)
    break_labels <- break_labels[priority]
    break_positions <- break_positions[priority]
  }

  labels_grob <- exec(
    element_grob, label_element,
    !!position_dim := break_positions,
    !!label_margin_name := TRUE,
    label = break_labels,
    check.overlap = check.overlap
  )
}

#' Determine the label priority for a given number of labels
#'
#' @param n The number of labels
#'
#' @return The vector `seq_len(n)` arranged such that the
#'   first, last, and middle elements are recursively
#'   placed at the beginning of the vector.
#' @noRd
#'
axis_label_priority <- function(n) {
  if (n <= 0) {
    return(numeric(0))
  }

  c(1, n, axis_label_priority_between(1, n))
}

axis_label_priority_between <- function(x, y) {
  n <- y - x + 1
  if (n <= 2) {
    return(numeric(0))
  }

  mid <- x - 1 + (n + 1) %/% 2
  c(
    mid,
    axis_label_priority_between(x, mid),
    axis_label_priority_between(mid, y)
  )
}

#' Override axis text angle and alignment
#'
#' @param axis_position One of bottom, left, top, or right
#' @param angle The text angle, or NULL to override nothing
#'
#' @return An [element_text()] that contains parameters that should be
#'   overridden from the user- or theme-supplied element.
#' @noRd
#'
axis_label_element_overrides <- function(axis_position, angle = NULL) {
  if (is.null(angle)) {
    return(element_text(angle = NULL, hjust = NULL, vjust = NULL))
  }

  # it is not worth the effort to align upside-down labels properly
  if (angle > 90 || angle < -90) {
    stop("`angle` must be between 90 and -90")
  }

  if (axis_position == "bottom") {
    element_text(
      angle = angle,
      hjust = if (angle > 0) 1 else if (angle < 0) 0 else 0.5,
      vjust = if (abs(angle) == 90) 0.5 else 1
    )
  } else if (axis_position == "left") {
    element_text(
      angle = angle,
      hjust = if (abs(angle) == 90) 0.5 else 1,
      vjust = if (angle > 0) 0 else if (angle < 0) 1 else 0.5,
    )
  } else if (axis_position == "top") {
    element_text(
      angle = angle,
      hjust = if (angle > 0) 0 else if (angle < 0) 1 else 0.5,
      vjust = if (abs(angle) == 90) 0.5 else 0
    )
  } else if (axis_position == "right") {
    element_text(
      angle = angle,
      hjust = if (abs(angle) == 90) 0.5 else 0,
      vjust = if (angle > 0) 1 else if (angle < 0) 0 else 0.5,
    )
  } else {
    stop(c(
      "Unrecognized `axis_position`\n",
      "Use one of 'top', 'bottom', 'left' or 'right'."
    ))
  }
}

# Parse takes a vector of n lines and returns m expressions.
# See https://github.com/tidyverse/ggplot2/issues/2864 for discussion.
#
# parse(text = c("alpha", "", "gamma"))
# #> expression(alpha, gamma)
#
# parse_safe(text = c("alpha", "", "gamma"))
# #> expression(alpha, NA, gamma)
#
parse_safe <- function(text) {
  if (!is.character(text)) {
    stop("`text` must be a character vector")
  }
  out <- vector("expression", length(text))
  for (i in seq_along(text)) {
    expr <- parse(text = text[[i]])
    out[[i]] <- if (length(expr) == 0) NA else expr[[1]]
  }
  out
}
