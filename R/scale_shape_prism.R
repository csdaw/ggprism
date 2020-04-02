prism_shape_pal <- function() {
  shapes <- c()
  values <- c(16, 15, 17, 18, 21, 22, 24, 25, 23, 8, 3, 4, 13, 7)
  out <- scales::manual_pal(shapes)
  attr(out, "max_n") <- length(shapes)
  out
}

scale_shape_prism <- function(...) {
  discrete_scale("shape", "prism", prism_shape_pal(), ...)
}



