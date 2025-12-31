#### Setup ---------------------------------------------------------------------
library(tinytest)
library(ggplot2)
library(dplyr)
using("tinysnapshot")

## Most of these tests are borrowed from the p-value vignette

#### Basic use (brackets) ------------------------------------------------------
p <- ggplot(datasets::sleep, aes(x = group, y = extra)) +
  geom_point(aes(shape = group), position = position_jitter(width = 0.1, seed = 1)) +
  stat_summary(geom = "crossbar", fun = mean, colour = "red", width = 0.2) +
  theme_prism() +
  theme(legend.position = "none")

# construct p-value data.frame
result <- t.test(extra ~ group, data = sleep)$p.value
result <- signif(result, digits = 3)
df_p_val <- data.frame(
  group1 = "1",
  group2 = "2",
  label = result,
  y.position = 6
)

# add p-value brackets
p1 <- p + add_pvalue(df_p_val,
                     xmin = "group1",
                     xmax = "group2",
                     label = "label",
                     y.position = "y.position")

expect_snapshot_plot(p1, label = "add_pvalue01")

# change the plot look
# change bracket and label aesthetics
p2 <- p + add_pvalue(df_p_val,
                     label = "p = {label}",
                     colour = "red", # label
                     label.size = 8, # label
                     fontface = "bold", # label
                     fontfamily = "serif", # label
                     angle = 45, # label
                     hjust = 1, # label
                     vjust = 2, # label
                     bracket.colour = "blue", # bracket
                     bracket.size = 1, # bracket
                     linetype = "dashed", # bracket
                     lineend = "round") # bracket

expect_snapshot_plot(p2, label = "add_pvalue02")

# make bracket tips longer and use coord_flip
p3 <- p + add_pvalue(df_p_val, tip.length = 0.15) +
  coord_flip()

expect_snapshot_plot(p3, label = "add_pvalue03")

# change bracket tips independently
# (make one side disappear and the other longer)
p4 <- p + add_pvalue(df_p_val, tip.length = c(0.2, 0))

expect_snapshot_plot(p4, label = "add_pvalue04")




#### Basic use (no brackets) ---------------------------------------------------

# position label above "group1"
p5 <- p + add_pvalue(df_p_val, label = "p = {label}",
                     remove.bracket = TRUE, x = 1)

expect_snapshot_plot(p5, label = "add_pvalue05")

# create a box plot of the ToothGrowth data set
p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) +
  geom_boxplot(aes(fill = dose), colour = "black") +
  theme_prism() +
  theme(legend.position = "none")

# compare means again reference
result1 <- t.test(len ~ dose,
                  data = subset(ToothGrowth, dose %in% c(0.5, 1.0)))$p.value
result2 <- t.test(len ~ dose,
                  data = subset(ToothGrowth, dose %in% c(0.5, 2.0)))$p.value

# Benjamini-Hochberg correction for multiple testing
result <- p.adjust(c(result1, result2), method = "BH")

# don't need group2 column (i.e. xmax)
# instead just specify x position in the same way as y.position
df_p_val <- data.frame(
  group1 = c(0.5, 0.5),
  group2 = c(1, 2),
  x = c(2, 3),
  label = signif(result, digits = 3),
  y.position = c(35, 35)
)

p6 <- p + add_pvalue(df_p_val,
                     xmin = "group1",
                     x = "x",
                     label = "p = {label}",
                     y.position = "y.position",
                     label.size = 3.2,
                     fontface = "bold")

expect_snapshot_plot(p6, label = "add_pvalue06")

# as above but with italics
df_p_val$p.exprs.ital <- lapply(
  paste(round(log10(df_p_val$label), 0)),
  function(x) bquote(italic("P = 1x10"^.(x)))
)

p7 <- p + add_pvalue(df_p_val,
                     xmin = "group1",
                     x = "x",
                     label = "p.exprs.ital",
                     y.position = "y.position",
                     parse = TRUE)

expect_snapshot_plot(p7, label = "add_pvalue07")




#### Compare two means ---------------------------------------------------------

df_p_val <- rstatix::t_test(ToothGrowth, len ~ supp) %>%
  rstatix::add_x_position()

p <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) +
  stat_summary(geom = "col", fun = mean) +
  stat_summary(geom = "errorbar",
               fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               width = 0.3) +
  theme_prism() +
  coord_cartesian(ylim = c(0, 35)) +
  scale_y_continuous(breaks = seq(0, 35, 5), expand = c(0, 0))

# normal plot
p8 <- p + add_pvalue(df_p_val, y.position = 30)

expect_snapshot_plot(p8, label = "add_pvalue08")



#### Compare mean vs reference -------------------------------------------------

df_p_val <- rstatix::t_test(ToothGrowth, len ~ dose, ref.group = "0.5") %>%
  rstatix::add_xy_position()

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) +
  stat_summary(geom = "col", fun = mean) +
  stat_summary(geom = "errorbar",
               fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               width = 0.3) +
  theme_prism() +
  coord_cartesian(ylim = c(0, 40)) +
  scale_y_continuous(breaks = seq(0, 40, 5), expand = c(0, 0))

# with brackets
p9 <- p + add_pvalue(df_p_val, label = "p.adj.signif")

# without brackets
p10 <- p + add_pvalue(df_p_val, label = "p.adj.signif", remove.bracket = TRUE)

expect_snapshot_plot(p9, label = "add_pvalue09")

expect_snapshot_plot(p10, label = "add_pvalue10")


df_p_val <- rstatix::t_test(ToothGrowth, len ~ dose, ref.group = "all")

p11 <- p + add_pvalue(df_p_val,
                      label = "p.adj.signif",
                      y.position = 35)

expect_snapshot_plot(p11, label = "add_pvalue11")


df_p_val <- ToothGrowth %>%
  rstatix::group_by(factor(dose)) %>%
  rstatix::t_test(len ~ 1, mu = 26) %>%
  rstatix::adjust_pvalue(p.col = "p", method = "holm") %>%
  rstatix::add_significance(p.col = "p.adj")

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) +
  stat_summary(geom = "col", fun = mean) +
  stat_summary(geom = "errorbar",
               fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               width = 0.3) +
  theme_prism() +
  coord_cartesian(ylim = c(0, 40)) +
  scale_y_continuous(breaks = seq(0, 40, 5), expand = c(0, 0))

# remember xmin and x are referring to the column dames in df_p_val
p12 <- p + add_pvalue(df_p_val,
                      xmin = "group1",
                      x = "factor(dose)",
                      y = 37,
                      label = "p.adj.signif")

expect_snapshot_plot(p12, label = "add_pvalue12")




#### Multiple pairwise comparisons ---------------------------------------------

df_p_val <- rstatix::t_test(ToothGrowth, len ~ dose)

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.2) +
  theme_prism() +
  coord_cartesian(ylim = c(0, 45)) +
  scale_y_continuous(breaks = seq(0, 45, 5), expand = c(0, 0))

p13 <- p + add_pvalue(df_p_val,
                      y.position = c(44, 41, 44),
                      bracket.shorten = c(0.025, 0, 0.025))

expect_snapshot_plot(p13, label = "add_pvalue13")




#### Grouped pairwise comparisons ----------------------------------------------

df_p_val <- ToothGrowth %>%
  rstatix::group_by(supp) %>%
  rstatix::t_test(len ~ dose) %>%
  rstatix::add_xy_position()

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) +
  geom_boxplot(aes(fill = supp)) +
  theme_prism()

# remember colour and step.group.by are referring to a column name in df_p_val
p14 <- p + add_pvalue(df_p_val,
                      label = "p = {p.adj}",
                      colour = "supp",
                      fontface = "bold",
                      step.group.by = "supp",
                      step.increase = 0.1,
                      tip.length = 0,
                      bracket.colour = "black",
                      show.legend = FALSE)

expect_snapshot_plot(p14, label = "add_pvalue14")


df_p_val <- ToothGrowth %>%
  rstatix::group_by(dose) %>%
  rstatix::t_test(len ~ supp) %>%
  rstatix::adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  rstatix::add_significance(p.col = "p.adj") %>%
  rstatix::add_xy_position(x = "dose", dodge = 0.8) # important for positioning!

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) +
  geom_boxplot(aes(fill = supp)) +
  theme_prism() +
  coord_cartesian(ylim = c(0, 40))

p15 <- p + add_pvalue(df_p_val,
                      xmin = "xmin",
                      xmax = "xmax",
                      label = "p = {p.adj}",
                      tip.length = 0)

expect_snapshot_plot(p15, label = "add_pvalue15")


df_p_val1 <- ToothGrowth %>%
  rstatix::group_by(dose) %>%
  rstatix::t_test(len ~ supp) %>%
  rstatix::adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  rstatix::add_significance(p.col = "p.adj") %>%
  rstatix::add_xy_position(x = "dose", dodge = 0.8) # important for positioning!

df_p_val2 <- rstatix::t_test(ToothGrowth, len ~ dose,
                             p.adjust.method = "bonferroni") %>%
  rstatix::add_xy_position()

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) +
  geom_boxplot(aes(fill = supp)) +
  theme_prism() +
  coord_cartesian(ylim = c(0, 45))

p16 <- p + add_pvalue(df_p_val1,
                      xmin = "xmin",
                      xmax = "xmax",
                      label = "p = {p.adj}",
                      tip.length = 0) +
  add_pvalue(df_p_val2,
             label = "p = {p.adj}",
             tip.length = 0.01,
             bracket.nudge.y = 2,
             step.increase = 0.015)

expect_snapshot_plot(p16, label = "add_pvalue16")




#### Facets --------------------------------------------------------------------

df_p_val <- ToothGrowth %>%
  rstatix::group_by(dose) %>%
  rstatix::t_test(len ~ supp) %>%
  rstatix::add_xy_position()

p <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) +
  geom_boxplot(width = 0.2) +
  facet_wrap(
    ~ dose, scales = "free",
    labeller = labeller(dose = function(x) paste("dose =", x))
  ) +
  theme_prism()

p17 <- p + add_pvalue(df_p_val)

expect_snapshot_plot(p17, label = "add_pvalue17")


df_p_val <- ToothGrowth %>%
  rstatix::group_by(supp) %>%
  rstatix::t_test(len ~ dose) %>%
  rstatix::add_xy_position()

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) +
  geom_boxplot(width = 0.4) +
  facet_wrap(~ supp, scales = "free") +
  theme_prism()

p18 <- p + add_pvalue(df_p_val)

expect_snapshot_plot(p18, label = "add_pvalue18")


# add a grouping variable to ToothGrowth
tg <- ToothGrowth
tg$dose <- factor(tg$dose)
tg$grp <- factor(rep(c("grp1", "grp2"), 30))

# construct the p-value table by hand
df_p_val <- data.frame(
  group1 = c("OJ", "OJ"),
  group2 = c("VC", "VC"),
  p.adj = c(0.0449, 0.00265),
  y.position = c(22, 27),
  grp = c("grp1", "grp2"),
  dose = c("0.5", "1")
)

p <- ggplot(tg, aes(x = factor(supp), y = len)) +
  geom_boxplot(width = 0.4) +
  facet_wrap(grp ~ dose, scales = "free") +
  theme_prism()

p19 <- p + add_pvalue(df_p_val, bracket.nudge.y = 3)

expect_snapshot_plot(p19, label = "add_pvalue19")

