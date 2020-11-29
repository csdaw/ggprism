#### Setup ---------------------------------------------------------------------
## load libraries
library(ggplot2)

#### Tests ---------------------------------------------------------------------
# test that preview_theme has correct class
expect_equal(class(preview_theme), c("function"))

# test that preview_theme works
g <- preview_theme("floral")

expect_silent(ggplotGrob(g))

#### Sanity checks -------------------------------------------------------------
# test that error occurs if palette does not exist
expect_error(preview_theme(palette = "banana"))

# test that error occurs if palette is "all_null"
expect_error(preview_theme(palette = "all_null"))
