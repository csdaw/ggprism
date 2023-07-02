#### Setup ---------------------------------------------------------------------

## load libraries
library(ggplot2)

p <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len)) +
  geom_point(na.rm = TRUE) +
  theme(axis.line = element_line(colour = "black"))

# grab axis function from teunbrand/ggh4x tests
grab_axis <- function(plot, side = "b") {
  gt <- ggplotGrob(plot)
  grb <- gt$grobs[grep(paste0("axis-", side), gt$layout$name)][[1]]
  grb <- grb$children[vapply(grb$children, inherits, logical(1), "gtable")][[1]]
  return(grb)
}

#### Tests ---------------------------------------------------------------------

# test that guide_prism_bracket defaults work
g1 <- p + scale_x_discrete(guide = "axis")
g2 <- p + scale_x_discrete(guide = "prism_bracket")

expect_silent(ggplotGrob(g1))
expect_silent(ggplotGrob(g2))

control <- grab_axis(g1, side = "b")
test <- grab_axis(g2, side = "b")

expect_equal(length(test$grobs[[1]]$x), length(control$grobs[[1]]$x) * 2)

# test that guide_prism_bracket works with coord flip
g1 <- p + scale_x_discrete(guide = "axis") +
  coord_flip()
g2 <- p + scale_x_discrete(guide = "prism_bracket") +
  coord_flip()

expect_silent(ggplotGrob(g1))
expect_silent(ggplotGrob(g2))

control <- grab_axis(g1, side = "l")
test <- grab_axis(g2, side = "l")

if (utils::packageVersion("ggplot2") <= "3.4.2") {
  expect_equal(length(test$grobs[[1]]$x), length(control$grobs[[1]]$x) * 2)
} else {
  expect_equal(length(test$grobs[[1]]$x), length(control$grobs[[2]]$x) * 2)
}

# test that guide_prism_bracket works with y axis and continuous scale
g1 <- p + scale_y_continuous(guide = "axis")
g2 <- p + scale_y_continuous(guide = "prism_bracket")

expect_silent(ggplotGrob(g1))
expect_silent(ggplotGrob(g2))

control <- grab_axis(g1, side = "l")
test <- grab_axis(g2, side = "l")

# p + scale_x_discrete(position = "right", guide = "prism_bracket")

# test that bracket width can be adjusted or left missing
g1 <- p + scale_x_discrete(guide = "prism_bracket")
g2 <- p + scale_x_discrete(guide = guide_prism_bracket(width = (0.8 + 0.01 * 3) / 3))

expect_silent(ggplotGrob(g1))
expect_silent(ggplotGrob(g2))

control <- grab_axis(g1, side = "b")
test <- grab_axis(g2, side = "b")

control$grobs[[1]]$x
test$grobs[[1]]$x

expect_equal(control$grobs[[1]]$x, test$grobs[[1]]$x)

# test that bracket direction can be adjusted
g <- p + scale_x_discrete(guide = guide_prism_bracket(outside = FALSE))

expect_silent(ggplotGrob(g))
