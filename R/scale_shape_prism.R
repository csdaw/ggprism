prism_shape_pal <- function() {
  shapes <- c(
    # standard shapes
    "circle small", "square", "triangle", "diamond",
    # filled shapes
    "circle filled", "square filled", "triangle filled",
    "triangle down filled", "diamond filled",
    # other shapes,
    "asterisk", "plus", "cross", "circle plus", "square cross"
    )
  values <- c(
    # standard shapes
    16, 15, 17, 18,
    # filled shapes
    21, 22, 24, 25, 23,
    # other shapes
    8, 3, 4, 10, 7
    )
  out <- scales::manual_pal(values)
  attr(out, "max_n") <- length(shapes)
  out
}

scale_shape_prism <- function(...) {
  discrete_scale("shape", "prism", prism_shape_pal(), ...)
}
