library("ggplot2")

p <- ggplot(mtcars, aes(x = wt, y = mpg, shape = factor(carb))) +
  geom_point(size = 3)
p + ggprism::scale_shape_prism()
