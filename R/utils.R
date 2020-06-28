# Define custom elements upon package load
.onLoad <- function(libname, pkgname) {
  register_theme_elements(
    prism.ticks.length   = unit(2, "pt"),
    element_tree = list(
      prism.ticks.length  = el_def("unit"),
      prism.ticks.length.x = el_def("unit", "prism.ticks.length"),
      prism.ticks.length.x.top = el_def("unit", "prism.ticks.length.x"),
      prism.ticks.length.x.bottom = el_def("unit", "prism.ticks.length.x"),
      prism.ticks.length.y = el_def("unit", "prism.ticks.length"),
      prism.ticks.length.y.left = el_def("unit", "prism.ticks.length.y"),
      prism.ticks.length.y.right = el_def("unit", "prism.ticks.length.y")
    )
  )
}

# Copied from https://github.com/teunbrand/ggh4x/tree/master/R/utils.R
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
