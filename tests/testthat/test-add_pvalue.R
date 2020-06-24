context("add_pvalue")

#### Setup ---------------------------------------------------------------------

## load libraries
library(ggplot2)

## data to plot
tg <- ToothGrowth
tg$dose <- as.factor(tg$dose)
tg$group <- factor(rep(c("grp1", "grp2"), 30))

## minimal examples of statistic tables (brackets):
# one group
one.mean <- tibble::tribble(
  ~group1, ~group2,      ~p.signif, ~y.position, ~dose,
  "1",     "null model", "****",    35,          "0.5",
  "1",     "null model", "****",    35,          "1",
  "1",     "null model", "****",    35,          "2"
)

# two groups
two.means <- tibble::tribble(
  ~group1, ~group2, ~p,     ~y.position,
  "OJ",    "VC",    0.0606, 36
)

two.means.grouped1 <- tibble::tribble(
  ~group1, ~group2, ~p.adj,  ~y.position, ~dose,
  "OJ",    "VC",    0.0127,  24,          "0.5",
  "OJ",    "VC",    0.00312, 30,          "1",
  "OJ",    "VC",    0.964,   36.5,        "2"
)

two.means.grouped2 <- tibble::tribble(
  ~group1, ~group2, ~p.signif, ~y.position, ~group, ~dose,
  "OJ",    "VC",    "*",       21,          "grp1", "0.5",
  "OJ",    "VC",    "**",      30,          "grp2", "1"
)

# each versus ref
each.vs.ref <- tibble::tribble(
  ~group1, ~group2, ~p.adj,   ~y.position,
  "0.5",   "1",     8.80e-14, 35,
  "0.5",   "2",     1.27e-7,  38
)

each.vs.basemean <- tibble::tribble(
  ~group1, ~group2, ~p.adj,     ~y.position,
  "all",   "0.5",   0.00000087, 35,
  "all",   "0.5",   0.512,      35,
  "all",   "0.5",   0.00000087, 35
)

# pairwise
pairwise <- tibble::tribble(
  ~group1, ~group2,  ~p.signif, ~y.position,
  "0.5",   "1",      "****",    34,
  "0.5",   "2",      "****",    36,
  "1",     "2",      "****",    38
)

pairwise.grouped <- tibble::tribble(
  ~group1, ~group2, ~p.adj,  ~y.position, ~supp,
  "0.5",   "1",     2.63e-4, 33.5,        "OJ",
  "0.5",   "2",     3.96e-6, 37.6,        "OJ",
  "1",     "2",     1.18e-1, 41.6,        "OJ",
  "0.5",   "1",     2.04e-6, 36.5,        "VC",
  "0.5",   "2",     1.40e-7, 40.6,        "VC",
  "1",     "2",     2.75e-4, 44.6,        "VC"
)

## base plots
base.tg1 <- ggplot(tg, aes(x = supp, y = len)) +
  geom_boxplot()

base.tg2 <- ggplot(tg, aes(x = dose, y = len)) +
  geom_boxplot(aes(fill = dose))

base.tg3 <- ggplot(tg, aes(x = dose, y = len)) +
  geom_boxplot(aes(fill = supp))

#### Tests (brackets) ----------------------------------------------------------

test_that("class is StatBracket and inherits Stat", {
  g <- base.tg1 + add_pvalue(two.means)

  expect_s3_class(g$layers[[2]]$stat, "StatBracket")
  expect_s3_class(g$layers[[2]]$stat, "Stat")
})

test_that("class is GeomBracket and inherits Geom", {
  g <- base.tg1 + add_pvalue(two.means)

  expect_s3_class(g$layers[[2]]$geom, "GeomBracket")
  expect_s3_class(g$layers[[2]]$geom, "Geom")
})

test_that("label can be glue expression", {
  g <- base.tg1 + add_pvalue(two.means, label = "p = {p}")

  expect_length(layer_grob(g), 1)
})

test_that("label can be column with any name", {
  two.means$custom.label <- "ns"
  g <- base.tg1 + add_pvalue(two.means, label = "custom.label")

  expect_length(layer_grob(g), 1)
})

test_that("xmin and xmax can be column with any name", {
  colnames(two.means)[1:2] <- c("apple", "banana")
  g <- base.tg1 + add_pvalue(two.means, xmin = "apple", xmax = "banana")

  expect_length(layer_grob(g), 1)
})

test_that("y.position can be column with any name", {
  colnames(two.means)[4] <- "orange"
  g <- base.tg1 + add_pvalue(two.means, y.position = "orange")

  expect_length(layer_grob(g), 1)
})

test_that("label.size can be adjusted", {
  g <- base.tg1 + add_pvalue(two.means, label.size = 1)

  expect_identical(g$layers[[2]]$aes_params$label.size, 1)
})

test_that("colour or color can be changed manually", {
  g1 <- base.tg1 + add_pvalue(two.means, colour = "red")
  g2 <- base.tg1 + add_pvalue(two.means, color = "red")

  expect_equal(g1, g2)
})

test_that("warning occurs if colour and color are set", {
  expect_warning(base.tg1 + add_pvalue(two.means, colour = "red", color = "black"),
                 "Duplicated aesthetics after name standardisation: colour")
})

test_that("colour can be set by grouping variable", {
  g1 <- base.tg3 + add_pvalue(pairwise.grouped, colour = "supp")
  g2 <- base.tg3 + add_pvalue(pairwise.grouped, color = "supp")

  expect_equal(g1, g2)
})

test_that("tip.length can be adjusted", {
  g1 <- base.tg2 + add_pvalue(each.vs.ref, tip.length = 0.01) # both at once
  g2 <- base.tg2 + add_pvalue(each.vs.ref, tip.length = c(0.01, 0.01)) # individually

  expect_identical(
    rep(g1$layers[[2]]$stat_params$tip.length, 2),
    g2$layers[[2]]$stat_params$tip.length
  )
})

test_that("tip.length can be negative", {
  g <- base.tg2 + add_pvalue(each.vs.ref, tip.length = -0.01)

  expect_length(layer_grob(g), 1)
})

test_that("tip.length can be zero", {
  g <- base.tg2 + add_pvalue(each.vs.ref, tip.length = 0)

  expect_length(layer_grob(g), 1)
})

test_that("bracket.size can be adjusted", {
  g1 <- base.tg1 + add_pvalue(two.means, bracket.size = 3) # all at once
  g2 <- base.tg1 + add_pvalue(two.means, bracket.size = c(3, 3, 3)) # individual segments

  expect_identical(
    rep(g1$layers[[2]]$stat_params$bracket.size, 3),
    g2$layers[[2]]$stat_params$bracket.size
  )
})

test_that("bracket.colour or bracket.color can be changed manually", {
  g1 <- base.tg1 + add_pvalue(two.means, bracket.colour = "red")
  g2 <- base.tg1 + add_pvalue(two.means, bracket.color = "red")

  expect_equal(g1, g2)
})

test_that("bracket.colour can be adjusted separately to colour", {
  g <- base.tg1 + add_pvalue(two.means, bracket.colour = "red", colour = "blue")

  expect_false(
    isTRUE(
      all.equal(
        g$layers[[2]]$aes_params$colour,
        g$layers[[2]]$data$bracket.colour
      )
    )
  )
})

test_that("brackets can be shortened", {
  g1 <- base.tg2 + add_pvalue(pairwise, bracket.shorten = 0.05) # all at once
  g2 <- base.tg2 + add_pvalue(pairwise, bracket.shorten = c(0.05, 0.05, 0.05)) #individually

  expect_equal(g1, g2)
})

test_that("bracket.nudge.y works", {
  g <- base.tg3 + add_pvalue(pairwise.grouped, bracket.nudge.y = 10)

  expect_length(layer_grob(g), 1)
})

test_that("step.increase works", {
  g <- base.tg3 + add_pvalue(pairwise.grouped, step.increase = 0.2, step.group.by = "supp")

  expect_length(layer_grob(g), 1)
})

test_that("step.group.by works", {
  g <- base.tg3 + add_pvalue(pairwise.grouped, colour = "supp", step.group.by = "supp")

  expect_length(layer_grob(g), 1)
})

test_that("coord.flip works", {
  g <- base.tg2 + add_pvalue(pairwise, coord.flip = TRUE) +
    coord_flip()

  expect_length(layer_grob(g), 1)
})

test_that("comparison against basemean works", {
  g <- base.tg2 + add_pvalue(each.vs.basemean)

  expect_length(layer_grob(g), 1)
})

test_that("add_pvalue works with facets using grouped data and free scales", {
  base.facet <- base.tg1 + facet_wrap(~ dose, scales = "free")
  g <- base.facet + add_pvalue(two.means.grouped1) # must have dose column

  expect_length(layer_grob(g), 3)

  gt <- ggplotGrob(g)
  gt <- gt$grobs[grepl("panel", gt$layout$name)][[1]]
  expect_match(gt$children[[4]]$name, "geom_bracket")
  expect_length(gt$children[[4]]$children, 2)
})

test_that("add_pvalue works with multiply grouped data", {
  base.facet <- base.tg1 + facet_grid(group ~ dose)
  g <- base.facet + add_pvalue(two.means.grouped2) # must have group, dose columns

  expect_length(layer_grob(g), 6)

  gt <- ggplotGrob(g)
  gt <- gt$grobs[grepl("panel", gt$layout$name)][[1]]
  expect_match(gt$children[[4]]$name, "geom_bracket")
  expect_length(gt$children[[4]]$children, 2)
})

test_that("add_pvalue works with multiple brackets per facet", {
  base.facet <- base.tg3 + facet_wrap(~ supp)
  g <- base.facet + add_pvalue(pairwise.grouped)

  expect_length(layer_grob(g), 2)

  gt <- ggplotGrob(g)
  gt <- gt$grobs[grepl("panel", gt$layout$name)][[1]]

  expect_match(gt$children[[4]]$name, "geom_bracket")
  expect_length(gt$children[[4]]$children, 6)
})

#### Tests (no brackets) -------------------------------------------------------

test_that("class is GeomText when brackets are removed", {
  # need to provide x
  g <- base.tg1 + add_pvalue(two.means, x = 1.5, remove.bracket = TRUE)

  expect_s3_class(g$layers[[2]]$geom, "GeomText")
})

test_that("comparison against null works", {
  g <- base.tg2 + add_pvalue(one.mean, x = "dose")

  expect_length(layer_grob(g), 1)
})

test_that("manual xmax = NULL works", {
  g <- base.tg3 + add_pvalue(two.means.grouped1, xmin = "dose", xmax = NULL)

  expect_length(layer_grob(g), 1)
})

test_that("colour or color can be changed manually (no brackets)", {
  g1 <- base.tg2 + add_pvalue(one.mean, x = "dose", colour = "blue")
  g2 <- base.tg2 + add_pvalue(one.mean, x = "dose", color = "blue")

  expect_equal(g1, g2)
})

test_that("colour can be set by grouping variable", {
  g1 <- base.tg2 + add_pvalue(one.mean, x = "dose", colour = "dose")
  g2 <- base.tg2 + add_pvalue(one.mean, x = "dose", color = "dose")

  expect_equal(g1, g2)
})

test_that("coord.flip works (no brackets)", {
  g <- base.tg2 + add_pvalue(each.vs.ref, coord.flip = TRUE, remove.bracket = TRUE) +
    coord_flip()

  expect_length(layer_grob(g), 1)
})

test_that("changing label angle works (no brackets)", {
  g <- base.tg2 + add_pvalue(each.vs.ref, coord.flip = TRUE, remove.bracket = TRUE, angle = 90) +
    coord_flip()

  expect_length(layer_grob(g), 1)
})
