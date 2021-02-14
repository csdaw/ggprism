#### Setup ---------------------------------------------------------------------
# Load libraries
library(magrittr)

# Make a test data.frame
df <- ToothGrowth
df$dose <- factor(df$dose)

#### Tests ---------------------------------------------------------------------
# test that custom theme elements are registered properly
ggprism:::.onLoad()
expect_true(
  all(
    c("prism.ticks.length",
      "prism.ticks.length.x",
      "prism.ticks.length.x.top",
      "prism.ticks.length.x.bottom",
      "prism.ticks.length.y",
      "prism.ticks.length.y.left",
      "prism.ticks.length.y.right") %in% names(ggplot2::get_element_tree()))
)

# test that ggplot2 internal function grabber works
x <- ggprism:::.grab_ggplot_internals()
classes <- sapply(x, class)
expect_equal(length(classes), 4)

# test that validate_x_position works where length(x) < nrow(data)
expect_equal(ggprism:::validate_x_position(1, df), rep(1, 60))

# test that validate_y_position works where length(y) < nrow(data)
expect_equal(ggprism:::validate_y_position(1, df), rep(1, 60))

# test that keep_only_tbl_df_classes works
df2 <- df %>%
  rstatix::group_by(supp) %>%
  rstatix::t_test(len ~ dose, ref.group = "0.5")

expect_equal(
  class(ggprism:::keep_only_tbl_df_classes(df2)),
  c("tbl_df", "tbl", "data.frame")
)

#### Sanity checks -------------------------------------------------------------
# test that validate_x_position throws an error when x column is not in data
expect_error(
  ggprism:::validate_x_position("banana", df),
  "can't find the x variable 'banana' in the data"
)

# test that validate_y_position throws an error when y column is not in data
expect_error(
  ggprism:::validate_y_position("apple", df),
  "can't find the y.position variable 'apple' in the data"
)

# test that guess_signif_label_column throws an error if no suitable label is found
expect_error(
  ggprism:::guess_signif_label_column(df),
  "label is missing"
)
