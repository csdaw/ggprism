#### Setup ---------------------------------------------------------------------
## load libraries
library(ggplot2)

p <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, shape = as.factor(dose))) +
  geom_point()

#### Tests ---------------------------------------------------------------------
# test that scale_shape_prism has correct class
expect_equal(class(scale_shape_prism()), c("ScaleDiscrete", "Scale", "ggproto", "gg"))

# test that scale_shape_prism works
g <- p + scale_shape_prism(palette = "default")

expect_silent(ggplotGrob(g))

# test that prism_shape_pal has correct structure
p1 <- prism_shape_pal(palette = "default")

expect_true(is.function(p1))

expect_equal(attr(p1, "max_n"), 9)

# test that prism_shape_pal produces a two digit number
expect_true(grepl("[0-9]{2}", p1(1)))

# test that error occurs if palette does not exist
expect_error(p + scale_shape_prism(palette = "banana"))
