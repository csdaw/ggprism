## we will use the ToothGrowth dataset for all examples
tg <- ToothGrowth
tg$dose <- as.factor(tg$dose)
tg$group <- factor(rep(c("grp1", "grp2"), 30))

## p-value bracket comparing two means
# p-value table (its best to use these column names)
two.means <- tibble::tribble(
  ~group1, ~group2, ~p,     ~y.position,
  "OJ",    "VC",    0.0606, 36
)

# boxplot (or another geom...)
ggplot(tg, aes(x = supp, y = len)) +
  geom_boxplot() +
  add_pvalue(two.means)

# if your table has special column names you will need to specify them
two.means <- tibble::tribble(
  ~apple, ~banana, ~my.pval, ~some.y.position,
  "OJ",    "VC",    0.0606,  36
)

ggplot(tg, aes(x = supp, y = len)) +
  geom_boxplot() +
  add_pvalue(
    two.means,
    xmin = "apple",
    xmax = "banana",
    label = "my.pval",
    y.position = "some.y.position"
  )

## you can make the label a glue expression
two.means <- tibble::tribble(
  ~group1, ~group2, ~p,     ~y.position,
  "OJ",    "VC",    0.0606, 36
)

ggplot(tg, aes(x = supp, y = len)) +
  geom_boxplot() +
  add_pvalue(two.means, label = "p = {p}")

## you can change aesthetics of the bracket and label
ggplot(tg, aes(x = supp, y = len)) +
  geom_boxplot() +
  add_pvalue(
    two.means,
    label = "p = {p}",
    colour = "red", # label
    label.size = 6, # label
    fontface = "bold", # label
    fontfamily = "serif", # label
    angle = 45, # label
    bracket.colour = "blue", # bracket
    bracket.size = 1, # bracket
    linetype = "dashed", # bracket
    lineend = "round" # bracket
  )

## you can change the tip length of the bracket
# make them longer
ggplot(tg, aes(x = supp, y = len)) +
  geom_boxplot() +
  add_pvalue(two.means, tip.length = 0.1)

# make them disappear
ggplot(tg, aes(x = supp, y = len)) +
  geom_boxplot() +
  add_pvalue(two.means, tip.length = 0)

# make one side longer than the other
ggplot(tg, aes(x = supp, y = len)) +
  geom_boxplot() +
  add_pvalue(two.means, tip.length = c(0.1, 0))

## p-value brackets with comparisons to a reference sample
each.vs.ref <- tibble::tribble(
  ~group1, ~group2, ~p.adj,   ~y.position,
  "0.5",   "1",     8.80e-14, 35,
  "0.5",   "2",     1.27e-7,  38
)

ggplot(tg, aes(x = dose, y = len)) +
  geom_boxplot(aes(fill = dose)) +
  add_pvalue(each.vs.ref)

## p-value brackets with pairwise comparisons
pairwise <- tibble::tribble(
  ~group1, ~group2,  ~p.signif, ~y.position,
  "0.5",   "1",      "****",    38,
  "0.5",   "2",      "****",    36,
  "1",     "2",      "****",    38
)

# you can shorten the length of brackets that are close together
ggplot(tg, aes(x = dose, y = len)) +
  geom_boxplot(aes(fill = dose)) +
  add_pvalue(
    pairwise,
    bracket.shorten = c(0.05, 0, 0.05)
  )

# you can nudge brackets that are not quite in the correct y position
# instead of changing the p-value table
ggplot(tg, aes(x = dose, y = len)) +
  geom_boxplot(aes(fill = dose)) +
  add_pvalue(
    pairwise,
    bracket.shorten = c(0.05, 0, 0.05),
    bracket.nudge.y = c(0.5, 0, 0.5)
  )

## p-value brackets with pairwise comparisons of grouped data
pairwise.grouped <- tibble::tribble(
  ~group1, ~group2, ~p.adj,  ~y.position, ~supp,
  "0.5",   "1",     2.63e-4, 33.5,        "OJ",
  "0.5",   "2",     3.96e-6, 37.6,        "OJ",
  "1",     "2",     1.18e-1, 41.6,        "OJ",
  "0.5",   "1",     2.04e-6, 36.5,        "VC",
  "0.5",   "2",     1.40e-7, 40.6,        "VC",
  "1",     "2",     2.75e-4, 44.6,        "VC"
)

# use step.increase to change the spacing between different brackets in the
# groups specified by step.group.by
ggplot(tg, aes(x = dose, y = len)) +
  geom_boxplot(aes(fill = supp)) +
  add_pvalue(
    pairwise.grouped,
    colour = "supp",
    tip.length = 0,
    step.group.by = "supp",
    step.increase = 0.03
  )

## p-value (brackets) with single facet variable
two.means.grouped1 <- tibble::tribble(
  ~group1, ~group2, ~p.adj,  ~y.position, ~dose,
  "OJ",    "VC",    0.0127,  24,          "0.5",
  "OJ",    "VC",    0.00312, 30,          "1",
  "OJ",    "VC",    0.964,   36.5,        "2"
)

ggplot(tg, aes(x = supp, y = len)) +
  geom_boxplot() +
  facet_wrap(~ dose, scales = "free") +
  add_pvalue(two.means.grouped1) # table must have dose column

## p-value (brackets) with single facet variable and multiple brackets per facet
pairwise.grouped <- tibble::tribble(
  ~group1, ~group2, ~p.adj,  ~y.position, ~supp,
  "0.5",   "1",     2.63e-4, 33.5,        "OJ",
  "0.5",   "2",     3.96e-6, 37.6,        "OJ",
  "1",     "2",     1.18e-1, 41.6,        "OJ",
  "0.5",   "1",     2.04e-6, 36.5,        "VC",
  "0.5",   "2",     1.40e-7, 40.6,        "VC",
  "1",     "2",     2.75e-4, 44.6,        "VC"
)

ggplot(tg, aes(x = dose, y = len)) +
  geom_boxplot(aes(fill = supp)) +
  facet_wrap(~ supp) +
  add_pvalue(pairwise.grouped)

## p-value (brackets) with two facet variables
two.means.grouped2 <- tibble::tribble(
  ~group1, ~group2, ~p.signif, ~y.position, ~group, ~dose,
  "OJ",    "VC",    "*",       21,          "grp1", "0.5",
  "OJ",    "VC",    "**",      30,          "grp2", "1"
)

ggplot(tg, aes(x = supp, y = len)) +
  geom_boxplot() +
  facet_wrap(group ~ dose) +
  add_pvalue(two.means.grouped2) # table must have dose and group column

## p-value (text only) comparing two means
two.means <- tibble::tribble(
  ~group1, ~group2, ~p,     ~y.position,
  "OJ",    "VC",    0.0606, 36
)

ggplot(tg, aes(x = supp, y = len)) +
  geom_boxplot() +
  add_pvalue(two.means, remove.bracket = TRUE, x = 1.5)

## p-value (text only) with coord_flip, override y.position, change angle
ggplot(tg, aes(x = supp, y = len)) +
  geom_boxplot() +
  add_pvalue(
    two.means,
    remove.bracket = TRUE,
    x = 1.5,
    y.position = 32,
    angle = 45
  ) +
  coord_flip()

## p-value (text only) comparing to the null
one.mean <- tibble::tribble(
  ~group1, ~group2,      ~p.signif, ~y.position, ~dose,
  "1",     "null model", "****",    35,          "0.5",
  "1",     "null model", "****",    35,          "1",
  "1",     "null model", "****",    35,          "2"
)

ggplot(tg, aes(x = dose, y = len)) +
  geom_boxplot(aes(fill = dose)) +
  add_pvalue(one.mean, x = "dose")

## p-value (text only) with comparisons to a base mean
each.vs.basemean <- tibble::tribble(
  ~group1, ~group2, ~p.adj, ~y.position, ~x,
  "all",   "0.5",   "****", 35, 1,
  "all",   "0.5",   "ns",   35, 2,
  "all",   "0.5",   "****", 35, 3
)

ggplot(tg, aes(x = dose, y = len)) +
  geom_boxplot(aes(fill = dose)) +
  add_pvalue(each.vs.basemean, x = "x")

## p-value (text only) with comparison to reference sample
each.vs.ref <- tibble::tribble(
  ~group1, ~group2, ~p.adj,   ~y.position,
  "0.5",   "1",     8.80e-14, 35,
  "0.5",   "2",     1.27e-7,  38
)

ggplot(tg, aes(x = dose, y = len)) +
  geom_boxplot(aes(fill = dose)) +
  add_pvalue(each.vs.ref, coord.flip = TRUE, remove.bracket = TRUE)

## p-value (text only) with a grouping variable
two.means.grouped1 <- tibble::tribble(
  ~group1, ~group2, ~p.adj,  ~y.position, ~dose,
  "OJ",    "VC",    0.0127,  24,          "0.5",
  "OJ",    "VC",    0.00312, 30,          "1",
  "OJ",    "VC",    0.964,   36.5,        "2"
)

ggplot(tg, aes(x = dose, y = len)) +
  geom_boxplot(aes(fill = supp)) +
  add_pvalue(two.means.grouped1, x = "dose")
