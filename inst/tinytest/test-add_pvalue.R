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

each.vs.ref.grouped <- tibble::tribble(
  ~group1, ~group2, ~p.signif, ~y.position, ~supp,
  "0.5",   "1",     "***",     33.6,        "OJ",
  "0.5",   "2",     "***",     39,          "OJ",
  "0.5",   "1",     "***",     36.6,        "VC",
  "0.5",   "2",     "***",     42,          "VC"
)
each.vs.ref.grouped$supp <- factor(each.vs.ref.grouped$supp)

each.vs.basemean <- tibble::tribble(
  ~group1, ~group2, ~p.adj,     ~y.position,
  "all",   "0.5",   0.00000087, 35,
  "all",   "1",     0.512,      35,
  "all",   "2",     0.00000087, 35
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

base.tg4 <- ggplot(tg, aes(x = supp, y = len)) +
  geom_boxplot(aes(fill = dose))

#### Tests (brackets) ----------------------------------------------------------

# test that class is StatBracket and inherits Stat
g <- base.tg1 + add_pvalue(two.means)
expect_equal(class(g$layers[[2]]$stat), c("StatBracket", "Stat", "ggproto", "gg"))

# test that class is GeomBracket and inherits Geom
expect_equal(class(g$layers[[2]]$geom), c("GeomBracket", "Geom", "ggproto", "gg"))

# test that label can be glue expression
g <- base.tg1 + add_pvalue(two.means, label = "p = {p}")

expect_silent(ggplotGrob(g))

# test_that label can be column with any name
two.means$custom.label <- "ns"
g <- base.tg1 + add_pvalue(two.means, label = "custom.label")

expect_silent(ggplotGrob(g))

# test_that xmin and xmax can be column with any name
colnames(two.means)[1:2] <- c("apple", "banana")
g <- base.tg1 + add_pvalue(two.means, xmin = "apple", xmax = "banana")

expect_silent(ggplotGrob(g))
colnames(two.means)[1:2] <- c("group1", "group2") # change them back

# test_that y.position can be column with any name
colnames(two.means)[4] <- "orange"
g <- base.tg1 + add_pvalue(two.means, y.position = "orange")

expect_silent(ggplotGrob(g))
colnames(two.means)[4] <- "y.position" # change it back

# test that label.size can be adjusted
g <- base.tg1 + add_pvalue(two.means, label.size = 1)

expect_identical(g$layers[[2]]$aes_params$label.size, 1)

# test_that colour or color can be changed manually
g1 <- base.tg1 + add_pvalue(two.means, colour = "red")
g2 <- base.tg1 + add_pvalue(two.means, color = "red")

# not a good way to test equivalency, see https://github.com/r-lib/waldo/issues/56
expect_equal(g1$layers[[2]]$data, g2$layers[[2]]$data)

# test that colour can be set by grouping variable
g1 <- base.tg3 + add_pvalue(pairwise.grouped, colour = "supp")
g2 <- base.tg3 + add_pvalue(pairwise.grouped, color = "supp")

# not a good way to test equivalency, see https://github.com/r-lib/waldo/issues/56
expect_equal(g1$layers[[2]]$data, g2$layers[[2]]$data)

# test that tip.length can be adjusted
g1 <- base.tg2 + add_pvalue(each.vs.ref, tip.length = 0.01) # both at once
g2 <- base.tg2 + add_pvalue(each.vs.ref, tip.length = c(0.01, 0.01)) # individually

expect_identical(
  rep(g1$layers[[2]]$stat_params$tip.length, 2),
  g2$layers[[2]]$stat_params$tip.length
)

# test that tip.length can be negative
g <- base.tg2 + add_pvalue(each.vs.ref, tip.length = -0.01)

expect_silent(ggplotGrob(g))

# test_that tip.length can be zero
g <- base.tg2 + add_pvalue(each.vs.ref, tip.length = 0)

expect_silent(ggplotGrob(g))

# test that bracket.size can be adjusted
g1 <- base.tg1 + add_pvalue(two.means, bracket.size = 3) # all at once
g2 <- base.tg1 + add_pvalue(two.means, bracket.size = c(3, 3, 3)) # individual segments

expect_identical(
  rep(g1$layers[[2]]$stat_params$bracket.size, 3),
  g2$layers[[2]]$stat_params$bracket.size
)

# test that bracket.colour or bracket.color can be changed manually
g1 <- base.tg1 + add_pvalue(two.means, bracket.colour = "red")
g2 <- base.tg1 + add_pvalue(two.means, bracket.color = "red")

# not a good way to test equivalency, see https://github.com/r-lib/waldo/issues/56
expect_equal(g1$layers[[2]]$data, g2$layers[[2]]$data)

# test that bracket.colour can be adjusted separately to colour
g <- base.tg1 + add_pvalue(two.means, bracket.colour = "red", colour = "blue")

expect_false(
  isTRUE(
    all.equal(
      g$layers[[2]]$aes_params$colour,
      g$layers[[2]]$data$bracket.colour
    )
  )
)

# test_that brackets can be shortened
g1 <- base.tg2 + add_pvalue(pairwise, bracket.shorten = 0.05) # all at once
g2 <- base.tg2 + add_pvalue(pairwise, bracket.shorten = c(0.05, 0.05, 0.05)) #individually

# not a good way to test equivalency, see https://github.com/r-lib/waldo/issues/56
expect_equal(g1$layers[[2]]$data, g2$layers[[2]]$data)

# test that bracket.nudge.y works
g <- base.tg3 + add_pvalue(pairwise.grouped, bracket.nudge.y = 10)

expect_silent(ggplotGrob(g))

# test_that step.increase works
g <- base.tg3 + add_pvalue(pairwise.grouped, step.increase = 0.2,
                           step.group.by = "supp")

expect_silent(ggplotGrob(g))

# test_that("step.group.by works", {
g <- base.tg3 + add_pvalue(pairwise.grouped, colour = "supp",
                           step.group.by = "supp")

expect_silent(ggplotGrob(g))

# test that coord.flip works
g <- base.tg2 + add_pvalue(pairwise, coord.flip = TRUE) +
  coord_flip()

expect_silent(ggplotGrob(g))

# test that comparison against basemean works
g <- base.tg2 + add_pvalue(each.vs.basemean)

expect_silent(ggplotGrob(g))

# test that comparison against reference group works with opposite orientation
g <- base.tg2 + add_pvalue(each.vs.ref, xmin = "group2", xmax = "group1")

expect_silent(ggplotGrob(g))

# test that add_pvalue works with facets using grouped data and free scales
base.facet <- base.tg1 + facet_wrap(~ dose, scales = "free")
g <- base.facet + add_pvalue(two.means.grouped1) # must have dose column

expect_equal(length(layer_grob(g)), 3)
expect_silent(ggplotGrob(g))

gt <- ggplotGrob(g)
gt <- gt$grobs[grepl("panel", gt$layout$name)][[1]]
expect_true(grepl("geom_bracket", gt$children[[4]]$name))
expect_equal(length(gt$children[[4]]$children), 2)

# test that add_pvalue works with multiply grouped data
base.facet <- base.tg1 + facet_grid(group ~ dose)
g <- base.facet + add_pvalue(two.means.grouped2) # must have group, dose columns

expect_equal(length(layer_grob(g)), 6)
expect_silent(ggplotGrob(g))

gt <- ggplotGrob(g)
gt <- gt$grobs[grepl("panel", gt$layout$name)][[1]]
expect_true(grepl("geom_bracket", gt$children[[4]]$name))
expect_equal(length(gt$children[[4]]$children), 2)

# test that add_pvalue works with multiple brackets per facet
base.facet <- base.tg3 + facet_wrap(~ supp)
g <- base.facet + add_pvalue(pairwise.grouped)

expect_equal(length(layer_grob(g)), 2)
expect_silent(ggplotGrob(g))

gt <- ggplotGrob(g)
gt <- gt$grobs[grepl("panel", gt$layout$name)][[1]]
expect_true(grepl("geom_bracket", gt$children[[4]]$name))
expect_equal(length(gt$children[[4]]$children), 6)

#### Tests (no brackets) -------------------------------------------------------

# test that remove.bracket works
g <- base.tg2 + add_pvalue(each.vs.ref, remove.bracket = TRUE)
expect_silent(ggplotGrob(g))

# test that remove.bracket works with opposite reference orientation
g <- base.tg2 + add_pvalue(each.vs.ref, remove.bracket = TRUE,
                           xmin = "group2", xmax = "group1")
expect_silent(ggplotGrob(g))

# test that class is GeomText when brackets are removed
# need to provide x
g <- base.tg1 + add_pvalue(two.means, x = 1.5, remove.bracket = TRUE)
expect_equal(class(g$layers[[2]]$geom), c("GeomText", "Geom", "ggproto", "gg"))

# test that comparison against null works
g <- base.tg2 + add_pvalue(one.mean, x = "dose", y = 35)

expect_silent(ggplotGrob(g))

# test that manual xmax = NULL works
g <- base.tg3 + add_pvalue(two.means.grouped1, xmin = "dose", xmax = NULL)

expect_silent(ggplotGrob(g))

# test that colour or color can be changed manually (no brackets)
g1 <- base.tg2 + add_pvalue(one.mean, x = "dose", colour = "blue")
g2 <- base.tg2 + add_pvalue(one.mean, x = "dose", color = "blue")

# not a good way to test equivalency, see https://github.com/r-lib/waldo/issues/56
expect_equal(g1$layers[[2]]$data, g2$layers[[2]]$data)

# test that colour can be set by grouping variable
g1 <- base.tg2 + add_pvalue(one.mean, x = "dose", colour = "dose")
g2 <- base.tg2 + add_pvalue(one.mean, x = "dose", color = "dose")

# not a good way to test equivalency, see https://github.com/r-lib/waldo/issues/56
expect_equal(g1$layers[[2]]$data, g2$layers[[2]]$data)

# test that coord.flip works (no brackets)
g <- base.tg2 + add_pvalue(each.vs.ref, coord.flip = TRUE,
                           remove.bracket = TRUE) +
  coord_flip()

expect_silent(ggplotGrob(g))

# test that changing label angle works (no brackets)
g <- base.tg2 + add_pvalue(each.vs.ref, coord.flip = TRUE,
                           remove.bracket = TRUE, angle = 90) +
  coord_flip()

expect_silent(ggplotGrob(g))

# test that grouped comparison against reference works
g <- base.tg4 + add_pvalue(each.vs.ref.grouped, x = "supp")

expect_silent(ggplotGrob(g))

#### Sanity checks -------------------------------------------------------------

# test that warning occurs if colour and color are set
expect_warning(base.tg1 + add_pvalue(two.means, colour = "red", color = "black"),
               "Use colour or color but not both.")

# test that warning occurs if bracket.colour and bracket.color are set
expect_warning(base.tg1 + add_pvalue(two.means, bracket.colour = "blue",
                                     bracket.color = "red"),
               "Use bracket.colour or bracket.color but not both.")

# test that specific xmin, xmax works even if group1 and group2 are in the data
two.means.copy <- two.means
colnames(two.means.copy)[1:2] <- c("apple", "banana")
two.means.copy$group1 <- 111
two.means.copy$group2 <- 222

expect_silent(base.tg1 + add_pvalue(two.means.copy, xmin = "apple", xmax = "banana"))

# test that error occurs if xmin column specified is missing from data
expect_error(base.tg1 + add_pvalue(two.means, xmin = "apple"),
             "can't find the xmin variable 'apple' in the data")

# test that error occurs if label column specified is missing from data
expect_error(base.tg1 + add_pvalue(two.means, label = "apple"),
             "can't find the label variable 'apple' in the data")
