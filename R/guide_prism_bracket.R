#' Axis guide with brackets
#'
#' This guide turns the axis into brackets drawn around each axis label.
#'
#' The number of brackets can be adjusted using the \code{breaks}
#' argument in \code{scale_(x|y)_continuous()} or \code{scale_(x|y)_discrete()}.
#'
#' @inheritParams ggplot2::guide_axis
#' @param bracket_width \code{numeric}. Controls the width of the bracket. Try
#' values between 0 and 1.
#' @param outside \code{logical}. Default is \code{TRUE} and brackets point
#' outwards. If \code{FALSE} the bracket crossbar is moved so the ticks appear
#' to point inwards towards the plotting area.
#'
#' @example inst/examples/ex-guide_prism_bracket.R
#'
#' @export
guide_prism_bracket <- function(title = waiver(), check.overlap = FALSE,
                                angle = NULL, n.dodge = 1, order = 0,
                                position = waiver(), bracket_width = NULL,
                                outside = TRUE) {
  structure(
    list(
      title = title,

      # customizations
      check.overlap = check.overlap,
      angle = angle,
      n.dodge = n.dodge,

      # general
      order = order,
      position = position,

      # parameter
      available_aes = c("x", "y"),

      # custom
      bracket_width = bracket_width,
      outside = outside,

      name = "axis"
    ),
    class = c("guide", "prism_bracket", "axis")
  )
}

#' @rdname guide-helpers
#' @export
guide_gengrob.prism_bracket <- function(guide, theme) {
  aesthetic <- names(guide$key)[!grepl("^\\.", names(guide$key))][1]

  draw_prism_bracket(
    break_positions = guide$key[[aesthetic]],
    break_labels = guide$key$.label,
    axis_position = guide$position,
    theme = theme,
    check.overlap = guide$check.overlap,
    angle = guide$angle,
    n.dodge = guide$n.dodge,
    bracket_width = guide$bracket_width,
    outside = guide$outside
  )
}


#' Grob for bracket axes
#'
#' @param break_position position of bracket center and labels
#' @param break_labels labels between ticks
#' @param axis_position position of axis (top, bottom, left or right)
#' @param theme A complete \code{\link[ggplot2]{theme}} object
#' @param check.overlap silently remove overlapping labels,
#'   (recursively) prioritizing the first, last, and middle labels.
#' @param angle Compared to setting the angle in
#'   \code{\link[ggplot2]{theme}} / \code{\link[ggplot2]{element_text}},
#'   this also uses some heuristics to automatically pick the \code{hjust} and
#'   \code{vjust} that you probably want.
#' @param n.dodge The number of rows (for vertical axes) or columns (for
#'   horizontal axes) that should be used to render the labels. This is
#'   useful for displaying labels that would otherwise overlap.
#' @noRd
#'
draw_prism_bracket <- function(break_positions, break_labels, axis_position,
                               theme,check.overlap = FALSE, angle = NULL,
                               n.dodge = 1, bracket_width = NULL,
                               outside = TRUE) {

  axis_position <- match.arg(axis_position, c("top", "bottom", "right", "left"))
  aesthetic <- if (axis_position %in% c("top", "bottom")) "x" else "y"

  # resolve elements
  line_element_name <- paste0("axis.line.", aesthetic, ".", axis_position)
  tick_element_name <- paste0("axis.ticks.", aesthetic, ".", axis_position)
  tick_length_element_name <- paste0("axis.ticks.length.", aesthetic, ".", axis_position)
  label_element_name <- paste0("axis.text.", aesthetic, ".", axis_position)

  line_element <- calc_element(line_element_name, theme)
  tick_element <- calc_element(tick_element_name, theme)
  tick_length <- calc_element(tick_length_element_name, theme)
  label_element <- calc_element(label_element_name, theme)

  # override label element parameters for rotation
  if (inherits(label_element, "element_text")) {
    label_overrides <- .ggint$axis_label_element_overrides(axis_position, angle)
    # label_overrides is an element_text, but label_element may not be;
    # to merge the two elements, we just copy angle, hjust, and vjust
    # unless their values are NULL
    if (!is.null(label_overrides$angle)) {
      label_element$angle <- label_overrides$angle
    }
    if (!is.null(label_overrides$hjust)) {
      label_element$hjust <- label_overrides$hjust
    }
    if (!is.null(label_overrides$vjust)) {
      label_element$vjust <- label_overrides$vjust
    }
  }

  # conditionally set parameters that depend on axis orientation
  is_vertical <- axis_position %in% c("left",  "right")

  position_dim <- if (is_vertical) "y" else "x"
  non_position_dim <- if (is_vertical) "x" else "y"
  position_size <- if (is_vertical) "height" else "width"
  non_position_size <- if (is_vertical) "width" else "height"
  gtable_element <- if (is_vertical) gtable_row else gtable_col
  measure_gtable <- if (is_vertical) gtable_width else gtable_height
  measure_labels_non_pos <- if (is_vertical) grobWidth else grobHeight

  # conditionally set parameters that depend on which side of the panel
  # the axis is on
  is_second <- axis_position %in% c("right", "top")

  tick_direction <- if (is_second) 1 else -1
  non_position_panel <- if (is_second) unit(0, "npc") else unit(1, "npc")
  tick_coordinate_order <- if (is_second) c(2, 1) else c(1, 2)

  # conditionally set the gtable ordering
  labels_first_gtable <- axis_position %in% c("left", "top") # position in gtable

  # set common parameters
  n_breaks <- length(break_positions)
  opposite_positions <- c("top" = "bottom", "bottom" = "top",
                          "right" = "left", "left" = "right")
  axis_position_opposite <- unname(opposite_positions[axis_position])

  # autocalculate bracket width based on number of breaks if missing
  # best for discrete axes, bad for continuous axes
  if (is.null(bracket_width)) {
    if (n_breaks == 1) {
      bracket_width <- 0.75
    }
    else if (n_breaks > 1) {
      bracket_width <- (0.8 + 0.01 * n_breaks) / n_breaks
    }
  }

  # draw elements
  half_bracket <- bracket_width / 2

  lines_grob <- exec(
    element_grob, line_element,
    !!position_dim := unit.c(
      unit(
        sort(c(break_positions - half_bracket,
               break_positions + half_bracket)), "native"
      )
    ),
    !!non_position_dim := if (outside) {
      rep(
        unit.c(non_position_panel, non_position_panel),
        times = n_breaks
      )
    } else {
      rep(
        unit.c(non_position_panel + (tick_direction * tick_length),
               non_position_panel + (tick_direction * tick_length)),
        times = n_breaks
      )
    },
    id.lengths = rep(2, times = n_breaks)
  )

  if (n_breaks == 0) {
    return(
      .ggint$absoluteGrob(
        gList(lines_grob),
        width = grobWidth(lines_grob),
        height = grobHeight(lines_grob)
      )
    )
  }

  # break_labels can be a list() of language objects
  if (is.list(break_labels)) {
    if (any(vapply(break_labels, is.language, logical(1)))) {
      break_labels <- do.call(expression, break_labels)
    } else {
      break_labels <- unlist(break_labels)
    }
  }

  # calculate multiple rows/columns of labels (which is usually 1)
  dodge_pos <- rep(seq_len(n.dodge), length.out = n_breaks)
  dodge_indices <- split(seq_len(n_breaks), dodge_pos)

  label_grobs <- lapply(dodge_indices, function(indices) {
    .ggint$draw_axis_labels(
      break_positions = break_positions[indices],
      break_labels = break_labels[indices],
      label_element = label_element,
      is_vertical = is_vertical,
      check.overlap = check.overlap
    )
  })

  ticks_grob <- exec(
    element_grob, tick_element,
    !!position_dim := unit.c(
      rep(unit(break_positions - half_bracket, "native"), each = 2),
      rep(unit(break_positions + half_bracket, "native"), each = 2)
    ),
    !!non_position_dim := rep(
      unit.c(non_position_panel + (tick_direction * tick_length),
             non_position_panel)[tick_coordinate_order],
      times = n_breaks * 2
    ),
    id.lengths = rep(2, times = n_breaks * 2)
  )

  # create gtable
  non_position_sizes <- paste0(non_position_size, "s")
  label_dims <- do.call(unit.c, lapply(label_grobs, measure_labels_non_pos))
  grobs <- c(list(ticks_grob), label_grobs)
  grob_dims <- unit.c(tick_length, label_dims)

  if (labels_first_gtable) {
    grobs <- rev(grobs)
    grob_dims <- rev(grob_dims)
  }

  gt <- exec(
    gtable_element,
    name = "axis",
    grobs = grobs,
    !!non_position_sizes := grob_dims,
    !!position_size := unit(1, "npc")
  )

  # create viewport
  justvp <- exec(
    viewport,
    !!non_position_dim := non_position_panel,
    !!non_position_size := measure_gtable(gt),
    just = axis_position_opposite
  )

  .ggint$absoluteGrob(
    gList(lines_grob, gt),
    width = gtable_width(gt),
    height = gtable_height(gt),
    vp = justvp
  )
}
