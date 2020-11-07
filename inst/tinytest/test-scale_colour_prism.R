#### Setup ---------------------------------------------------------------------
## load libraries
library(ggplot2)

p <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, colour = as.factor(dose))) +
  geom_point()

#### Tests ---------------------------------------------------------------------
# test that scale_colour_prism has correct class
expect_equal(class(scale_colour_prism()), c("ScaleDiscrete", "Scale", "ggproto", "gg"))
expect_equal(class(scale_color_prism()), c("ScaleDiscrete", "Scale", "ggproto", "gg"))

# test that scale_colour_prism works
g1 <- p + scale_colour_prism(palette = "colors")
g2 <- p + scale_color_prism(palette = "colors")

expect_silent(ggplotGrob(g1))
expect_silent(ggplotGrob(g2))

# test that prism_colour_pal has correct structure
p1 <- prism_colour_pal(palette = "colors")
p2 <- prism_color_pal(palette = "colors")

expect_equal(class(p1), "function")
expect_equal(class(p2), "function")

expect_equal(attr(p1, "max_n"), 20)
expect_equal(attr(p2, "max_n"), 20)

# test that prism_colour_pal produces a valid hex colour
expect_true(grepl("^#[a-zA-Z0-9]{6}$", p1(1)))
expect_true(grepl("^#[a-zA-Z0-9]{6}$", p2(1)))

# test that error occurs if palette does not exist
expect_error(p + scale_colour_prism(palette = "banana"))
expect_error(p + scale_color_prism(palette = "banana"))
