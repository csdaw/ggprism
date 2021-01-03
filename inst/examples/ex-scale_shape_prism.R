library(ggplot2)

## list all available shape palettes
ggprism_data$shape_palettes

## define a base plot
base <- ggplot(mtcars, aes(x = wt, y = mpg,
                           shape = factor(cyl))) +
  geom_point(size = 3)

## works pretty much the same as ggplot2 scale_shape_manual
base +
  scale_shape_prism(palette = "complete")

## change shape scale title in legend
base +
  scale_shape_prism(
    palette = "default",
    name = "Cylinders"
  )

## change shape labels in legend
base +
  scale_shape_prism(
    palette = "default",
    name = "Cylinders",
    label = c("4 cyl", "6 cyl", "8 cyl")
  )

## change shape labels in legend with a function
base +
  scale_shape_prism(
    palette = "default",
    name = "Cylinders",
    label = function(x) paste(x, "cyl")
  )

## change order of shapes in legend
base +
  scale_shape_prism(
    palette = "default",
    name = "Cylinders",
    label = function(x) paste(x, "cyl"),
    breaks = c(8, 4, 6)
  )

## to change which shape is assigned to which cyl
## you need to change the factor levels in the underlying data
base <- ggplot(mtcars, aes(x = wt, y = mpg,
                           shape = factor(cyl, levels = c(6, 4, 8)))) +
  geom_point(size = 3)

base +
  scale_shape_prism(
    palette = "default",
    name = "Cylinders"
  )

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
