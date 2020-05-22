# Copied from teunbrand/ggh4x/R/utils.R
# Function for grabbing internal function of ggplot2 that are also used here
.grab_ggplot_internals <- function() {
  objects <- c(
    "absoluteGrob",
    "axis_label_element_overrides",
    "draw_axis_labels",
    "new_data_frame"
  )
  objects <- setNames(objects, objects)
  out <- lapply(objects, function(i) {
    getFromNamespace(i, "ggplot2")
  })
}

# Store the needed ggplot internals here
.ggint <- .grab_ggplot_internals()
