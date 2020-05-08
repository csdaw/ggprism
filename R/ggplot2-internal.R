# Unexported ggplot2 functions
# Internal functions required for guide_prism_minor
# Avoids accessing with ggplot2:::

#' Absolute grob
#'
#' This grob has fixed dimensions and position.
#'
#' It's still experimental
#'
#' @keywords internal
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


axis_label_element_overrides <- function(axis_position, angle = NULL) {
  if (is.null(angle)) {
    return(element_text(angle = NULL, hjust = NULL, vjust = NULL))
  }

  # it is not worth the effort to align upside-down labels properly
  if (angle > 90 || angle < -90) {
    abort("`angle` must be between 90 and -90")
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
    abort(glue("Unrecognized position: '{axis_position}'"))
  }
}

# Fast data.frame constructor and indexing
# No checking, recycling etc. unless asked for
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) {
    abort("Elements must be named")
  }
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) {
      abort("Elements must equal the number of rows or 1")
    }
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
}
