#### Define custom elements upon package load ----------------------------------
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

#### Get required internal ggplot2 functions -----------------------------------

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

#### Helper functions for stat_pvalue_manual -----------------------------------

# Guess the column to be used as the significance labels
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

# Filter out non-significant results if required
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

# Validate p-value y position
validate_y_position <- function(y.position, data){
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

keep_only_tbl_df_classes <- function(x){
  toremove <- setdiff(class(x), c("tbl_df", "tbl", "data.frame"))
  if(length(toremove) > 0){
    class(x) <- setdiff(class(x), toremove)
  }
  x
}

# For control rows: the comparison of control against itself
# Used only when positioning the labels for grouped bars
add_ctr_rows <- function(data, ref.group){
  xmin <- NULL
  data <- keep_only_tbl_df_classes(data)

  ctr <- data[!duplicated(data$xmin), ]
  ctr$group2 <- ref.group
  ctr$label <- " "

  rbind(ctr, data)
}
