#### Setup ---------------------------------------------------------------------
## load libraries
library(ggplot2)

p <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len)) +
  geom_point()

#### Tests ---------------------------------------------------------------------
# test that theme_prism has correct class
expect_equal(class(theme_prism()), c("theme", "gg"))

# test that theme_prism defaults work
g <- p + theme_prism()

expect_silent(ggplotGrob(g))

# test that theme_prism axis text angle can be changed
g <- p + theme_prism(axis_text_angle = 45)

expect_silent(ggplotGrob(g))

# test that theme_prism border can be set
g <- p + theme_prism(border = TRUE) +
  coord_cartesian(clip = "off")

expect_silent(ggplotGrob(g))

#### Sanity checks -------------------------------------------------------------
# test that error occurs if palette does not exist
expect_error(p + theme_prism(palette = "banana"))

# test that error occurs if improper axis text angle is used
expect_error(p + theme_prism(axis_text_angle = 22))

# test that error occurs if border isn't boolean
expect_error(p + theme_prism(border = NULL))
expect_error(p + theme_prism(border = "banana"))
expect_error(p + theme_prism(border = 0))
