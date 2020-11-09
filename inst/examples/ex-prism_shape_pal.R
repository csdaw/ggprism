## list all available shape
ggprism_data$shape_palettes

## select some shapes from a palette
prism_shape_pal(palette = "filled")(4)

## see all the shapes in a specific palette
# define a function for convenience
show_shapes <- function(palette) {
  df_shapes <- ggprism_data$shape_palettes[[palette]][, -1]

  ggplot(df_shapes, aes(x = 0, y = 0, shape = pch)) +
    geom_point(aes(shape = pch), size = 5, fill = 'red') +
    scale_shape_identity() +
    facet_wrap(~ pch) +
    theme_void()
}

# show the colours in the palette "complete"
show_shapes("complete")
