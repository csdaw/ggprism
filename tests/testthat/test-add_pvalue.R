context("add_pvalue")

#### Setup ---------------------------------------------------------------------

## data to plot
data <- ToothGrowth
data$dose <- as.factor(data$dose)

## minimal examples of statistic tables (brackets):

# one group

# two groups
two.means <- tribble(
  ~group1, ~group2, ~p,     ~y.position,
  "OJ",    "VC",    0.0606, 36
)

two.means.grouped <- tribble(
  ~group1, ~group2, ~p.adj,  ~y.position, ~dose,
  "OJ",    "VC",    0.0127,  24,          0.5,
  "OJ",    "VC",    0.00312, 30,          1,
  "OJ",    "VC",    0.964,   36.5,        2
  )

# each versus ref

# pairwise

## base plots
base <- ggplot(data, aes(x = supp, y = len)) +
  geom_boxplot()

#### Tests (brackets) ----------------------------------------------------------

test_that("GeomBracket adds to plot", {
  g <- base + add_pvalue(two.means)
  expect_is(g$layers[[2]]$geom, "GeomBracket")
  # check inherits Geom
  expect_is(g$layers[[2]]$geom, "Geom")
})

test_that("works with facets", {
  base.facet <- base + facet_wrap(~ dose)
  g <- base.facet + add_pvalue(two.means.grouped, label = "p.adj")
})

#### Tests (text) --------------------------------------------------------------

test_that("GeomText adds to plot when brackets are removed", {
  # need to provide x
  g <- base + add_pvalue(two.means, x = 1.5, remove.bracket = TRUE)
  expect_is(g$layers[[2]]$geom, "GeomText")
})
