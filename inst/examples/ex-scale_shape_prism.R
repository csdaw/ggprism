## base plots
base <- ggplot(mtcars, aes(x = wt, y = mpg,
                           shape = factor(cyl))) +
  geom_point(size = 3)

base2 <- ggplot(mtcars, aes(x = wt, y = mpg,
                            shape = factor(cyl),
                            colour = factor(cyl))) +
  geom_point(size = 3)

base3 <- ggplot(mtcars, aes(x = wt, y = mpg,
                            shape = factor(cyl),
                            fill = factor(cyl))) +
  geom_point(size = 3, colour = "black")

## works pretty much the same as ggplot2 scale_colour_manual
base +
  scale_shape_prism(palette = "default")

base +
  scale_shape_prism(palette = "complete")

base2 +
  scale_shape_prism(palette = "default")

base3 +
  scale_shape_prism(palette = "filled")

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
