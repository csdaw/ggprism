
#' @export
#' @importFrom ggplot2 guide_train
guide_train.prism_axis <- function(guide, scale, aesthetic = NULL) {

  aesthetic <- aesthetic %||% scale$aesthetics[1]
  breaks <- scale$get_breaks()

  empty_ticks <- base::data.frame(
    aesthetic = numeric(0),
    .value    = numeric(0),
    .label    = character(0)
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
    ticks <- base::data.frame(setNames(list(mapped_breaks), aesthetic))
    ticks$.value <- breaks
    ticks$.label <- scale$get_labels(breaks)

    guide$key <- ticks[is.finite(ticks[[aesthetic]]), ]
  }

  guide$name <- paste0(guide$name, "_", aesthetic)
  guide$hash <- digest(list(guide$title, guide$key$.value,
                            guide$key$.label, guide$name, guide$is_major))
  guide
}

#' @export
#' @importFrom ggplot2 guide_transform
guide_transform.prism_axis <- function(guide, coord, panel_params) {

  if (is.null(guide$position) || nrow(guide$key) == 0) {
    return(guide)
  }

  aesthetics <- names(guide$key)[!grepl("^\\.", names(guide$key))]

  if (all(c("x", "y") %in% aesthetics)) {
    guide$key <- coord$transform(guide$key, panel_params)
  } else {
    other_aesthetic <- setdiff(c("x", "y"), aesthetics)
    override_value  <- if(guide$position %in% c("bottom", "left")) -Inf else Inf
    guide$key[[other_aesthetic]] <- override_value

    guide$key <- coord$transform(guide$key, panel_params)

    warn_for_guide_position(guide)
  }
  guide
}

warn_for_guide_position <- function(guide) {
  # This is trying to catch when a user specifies a position perpendicular
  # to the direction of the axis (e.g., a "y" axis on "top").
  # The strategy is to check that two or more unique breaks are mapped
  # to the same value along the axis.
  breaks_are_unique <- !duplicated(guide$key$.value)
  empty <- is.null(guide$key) || prod(dim(guide$key)) == 0 ||
    inherits(guide$key, "waiver")
  if (empty || sum(breaks_are_unique) == 1) {
    return()
  }

  if (guide$position %in% c("top", "bottom")) {
    position_aes <- "x"
  } else if(guide$position %in% c("left", "right")) {
    position_aes <- "y"
  } else {
    return()
  }

  if (length(unique(guide$key[[position_aes]][breaks_are_unique])) == 1) {
    warn(c(
      "Position guide is perpendicular to the intended axis",
      "i" = "Did you mean to specify a different guide `position`?"
    ))
  }
}

#' @export
#' @importFrom ggplot2 guide_geom
guide_geom.prism_axis <- function(guide, layers, ...) {
  guide
}

#' @export
#' @importFrom ggplot2 guide_merge
guide_merge.prism_axis <- function(guide, layers, ...) {
  if (!inherits(new_guide, "guide_none")) {
    warn(c(
      "Discarding guide on merge",
      "i" = "Do you have more than one guide with the same position?"
    ))
  }
}
