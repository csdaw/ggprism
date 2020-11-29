library(ggplot2)

## list all available shape palettes
ggprism_data$shape_palettes

## select some shapes from a palette
prism_shape_pal(palette = "filled")(4)

## see all the shapes in a specific palette
# define a function for convenience
show_shapes <- function(palette) {
  df_shapes <- ggprism_data$shape_palettes[[palette]][, -1]
  df_shapes$pch_f <- factor(df_shapes$pch, levels = df_shapes$pch)

  ggplot(df_shapes, aes(x = 0, y = 0, shape = pch)) +
    geom_point(aes(shape = pch), size = 5, fill = 'red') +
    scale_shape_identity() +
    facet_wrap(~ pch_f) +
    theme_void()
}

# show the shapes in the palette "complete"
show_shapes("complete")
