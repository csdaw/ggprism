#' Axis guide
#'
#' Axis guides are the visual representation of position scales like those
#' created with [scale_(x|y)_continuous()][scale_x_continuous()] and
#' [scale_(x|y)_discrete()][scale_x_discrete()].
#'
#' @inheritParams guide_legend
#' @param check.overlap silently remove overlapping labels,
#'   (recursively) prioritizing the first, last, and middle labels.
#' @param angle Compared to setting the angle in [theme()] / [element_text()],
#'   this also uses some heuristics to automatically pick the `hjust` and `vjust` that
#'   you probably want.
#' @param n.dodge The number of rows (for vertical axes) or columns (for
#'   horizontal axes) that should be used to render the labels. This is
#'   useful for displaying labels that would otherwise overlap.
#' @param order Used to determine the order of the guides (left-to-right,
#'   top-to-bottom), if more than one  guide must be drawn at the same location.
#' @param position Where this guide should be drawn: one of top, bottom,
#'   left, or right.
#'
#' @export
#'
#' @examples
#' # plot with overlapping text
#' p <- ggplot(mpg, aes(cty * 100, hwy * 100)) +
#'   geom_point() +
#'   facet_wrap(vars(class))
#'
#' # axis guides can be customized in the scale_* functions or
#' # using guides()
#' p + scale_x_continuous(guide = guide_axis(n.dodge = 2))
#' p + guides(x = guide_axis(angle = 90))
#'
#' # can also be used to add a duplicate guide
#' p + guides(x = guide_axis(n.dodge = 2), y.sec = guide_axis())
#'
#'
guide_prism_minor <- function(title = waiver(), check.overlap = FALSE, angle = NULL,
                              n.dodge = 1, order = 0, position = waiver()) {
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

      name = "axis"
    ),
    class = c("guide", "prism_minor", "axis")
  )
}

#' @export
guide_train.prism_minor <- function(guide, scale, aesthetic = NULL) {

  aesthetic <- aesthetic %||% scale$aesthetics[1]
  breaks <- scale$get_breaks()

  empty_ticks <- new_data_frame(
    list(aesthetic = numeric(0), .value = numeric(0), .label = character(0))
  )
  names(empty_ticks) <- c(aesthetic, ".value", ".label")

  if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
    warn(glue(
      "axis guide needs appropriate scales: ",
      glue_collapse(guide$available_aes, ", ", last = " or ")
    ))
    guide$key <- empty_ticks
  } else if (length(breaks) == 0) {
    guide$key <- empty_ticks
  } else {
    mapped_breaks <- if (scale$is_discrete()) scale$map(breaks) else breaks
    ticks <- new_data_frame(setNames(list(mapped_breaks), aesthetic))
    ticks$.value <- breaks
    ticks$.label <- scale$get_labels(breaks)

    guide$key <- ticks[is.finite(ticks[[aesthetic]]), ]
  }

  guide$name <- paste0(guide$name, "_", aesthetic)
  guide$hash <- digest::digest(list(guide$title, guide$key$.value, guide$key$.label, guide$name))
  guide
}

#' @export
guide_gengrob.prism_minor <- function(guide, theme) {
  aesthetic <- names(guide$key)[!grepl("^\\.", names(guide$key))][1]

  draw_axis(
    break_positions = guide$key[[aesthetic]],
    break_labels = guide$key$.label,
    axis_position = guide$position,
    theme = theme,
    check.overlap = guide$check.overlap,
    angle = guide$angle,
    n.dodge = guide$n.dodge
  )
}

#' Grob for axes
#'
#' @param break_position position of ticks
#' @param break_labels labels at ticks
#' @param axis_position position of axis (top, bottom, left or right)
#' @param theme A complete [theme()] object
#' @param check.overlap silently remove overlapping labels,
#'   (recursively) prioritizing the first, last, and middle labels.
#' @param angle Compared to setting the angle in [theme()] / [element_text()],
#'   this also uses some heuristics to automatically pick the `hjust` and `vjust` that
#'   you probably want.
#' @param n.dodge The number of rows (for vertical axes) or columns (for
#'   horizontal axes) that should be used to render the labels. This is
#'   useful for displaying labels that would otherwise overlap.
#'
#' @noRd
#'
draw_prism_minor <- function(break_positions, break_labels, axis_position, theme,
                      check.overlap = FALSE, angle = NULL, n.dodge = 1) {

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
    label_overrides <- axis_label_element_overrides(axis_position, angle)
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
  labels_first_gtable <- axis_position %in% c("left", "top") # refers to position in gtable

  # set common parameters
  n_breaks <- length(break_positions)
  opposite_positions <- c("top" = "bottom", "bottom" = "top", "right" = "left", "left" = "right")
  axis_position_opposite <- unname(opposite_positions[axis_position])

  # draw elements
  line_grob <- exec(
    element_grob, line_element,
    !!position_dim := unit(c(0, 1), "npc"),
    !!non_position_dim := unit.c(non_position_panel, non_position_panel)
  )

  if (n_breaks == 0) {
    return(
      absoluteGrob(
        gList(line_grob),
        width = grobWidth(line_grob),
        height = grobHeight(line_grob)
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
    draw_axis_labels(
      break_positions = break_positions[indices],
      break_labels = break_labels[indices],
      label_element = label_element,
      is_vertical = is_vertical,
      check.overlap = check.overlap
    )
  })

  ticks_grob <- exec(
    element_grob, tick_element,
    !!position_dim := rep(unit(break_positions, "native"), each = 2),
    !!non_position_dim := rep(
      unit.c(non_position_panel + (tick_direction * tick_length), non_position_panel)[tick_coordinate_order],
      times = n_breaks
    ),
    id.lengths = rep(2, times = n_breaks)
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

  absoluteGrob(
    gList(line_grob, gt),
    width = gtable_width(gt),
    height = gtable_height(gt),
    vp = justvp
  )
}
