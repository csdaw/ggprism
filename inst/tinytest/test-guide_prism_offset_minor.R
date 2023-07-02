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
# test that guide_prism_offset_minor defaults work
g1 <- p + scale_y_continuous(guide = "axis")
g2 <- p + scale_y_continuous(guide = "prism_offset_minor")

expect_silent(ggplotGrob(g1))
expect_silent(ggplotGrob(g2))

control <- grab_axis(g1, side = "l")
test <- grab_axis(g2, side = "l")

if (utils::packageVersion("ggplot2") <= "3.4.2") {
  expect_equal(length(test$grobs[[2]]$y), (length(control$grobs[[2]]$y) * 2) + 2)
} else {
  expect_equal(length(test$grobs[[2]]$y), (length(control$grobs[[1]]$y) * 2) + 2)
}

# test that guide_prism_offset_minor works with coord flip
g1 <- p + scale_y_continuous(guide = "axis") +
  coord_flip()
g2 <- p + scale_y_continuous(guide = "prism_offset_minor") +
  coord_flip()

expect_silent(ggplotGrob(g1))
expect_silent(ggplotGrob(g2))

control <- grab_axis(g1, side = "b")
test <- grab_axis(g2, side = "b")

expect_equal(length(test$grobs[[1]]$x), (length(control$grobs[[1]]$x) * 2) + 2)

# test that prism.ticks.length theme element works
g <- p + scale_y_continuous(guide = "prism_offset_minor") +
  theme(prism.ticks.length = unit(20, "pt"))

expect_silent(ggplotGrob(g))

test <- grab_axis(g, side = "l")$grobs[[2]]$x
expect_equal(length(unique(test)), 3)

# test that guide_prism_minor produces error with no minor breaks
g1 <- p + scale_x_discrete(guide = "prism_offset_minor")
g2 <- p + scale_y_continuous(guide = "prism_offset_minor", minor_breaks = NULL)

expect_error(ggplotGrob(g1))
expect_error(ggplotGrob(g2))
