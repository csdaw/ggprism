# test that ggplot2 internal function grabber works
x <- .grab_ggplot_internals()
classes <- sapply(x, class)
expect_equal(length(classes), 4)
