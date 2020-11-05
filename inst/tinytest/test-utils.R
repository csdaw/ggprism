# test that ggplot2 internal function grabber works
x <- ggprism:::.grab_ggplot_internals()
classes <- sapply(x, class)
expect_equal(length(classes), 4)
