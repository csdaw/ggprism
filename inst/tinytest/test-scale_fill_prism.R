#### Setup ---------------------------------------------------------------------
## load libraries
library(ggplot2)

p <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, fill = as.factor(dose))) +
  geom_col()

#### Tests ---------------------------------------------------------------------
# test that scale_fill_prism has correct class
expect_equal(class(scale_fill_prism()), c("ScaleDiscrete", "Scale", "ggproto", "gg"))

# test that scale_fill_prism works
g <- p + scale_fill_prism(palette = "colors")

expect_silent(ggplotGrob(g))

# test that prism_fill_pal has correct structure
p1 <- prism_fill_pal(palette = "colors")

expect_equal(class(p1), "function")

expect_equal(attr(p1, "max_n"), 20)

# test that prism_fill_pal produces a valid hex colour
expect_true(grepl("^#[a-zA-Z0-9]{6}$", p1(1)))

# test that error occurs if palette does not exist
expect_error(p + scale_fill_prism(palette = "banana"))
