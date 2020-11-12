
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggprism <img src="man/figures/ggprism_logo.png" align = "right" width = "150" />

<!-- badges: start -->

[![R build
status](https://github.com/csdaw/ggprism/workflows/R-CMD-check/badge.svg)](https://github.com/csdaw/ggprism/actions)
<!-- badges: end -->

The `ggprism` package provides various themes, palettes, and other
useful functions to customise ggplots and give them the *‘GraphPad
Prism’* look.

## Install

Install the development version from github.

``` r
remotes::install_github("csdaw/ggprism")
```

## How to Use

See the Getting Started page for a quick overview of `ggprism` features.
Detailed examples and instructions can be found below and on the
`ggprism` website.

``` r
tg <- ToothGrowth
tg$dose <- as.factor(tg$dose)

base <- ggplot(tg, aes(x = dose, y = len, colour = dose, fill = dose)) + 
  geom_violin(trim = FALSE) + 
  geom_boxplot(width = 0.2, colour = "black") + 
  scale_y_continuous(limits = c(-5, 40))
```

``` r
base

base + 
  scale_color_prism("floral") + 
  scale_fill_prism("floral") + 
  theme_prism(base_size = 16) + 
  guides(y = "prism_minor") + 
  theme(legend.position = "none")
```

<img src="man/figures/README-ex-1.png" width="400" /><img src="man/figures/README-ex-2.png" width="400" />

## Feedback and Contributions

Any feedback, issues, suggestions, and contributions are welcome and
should be shared via `Github`
[issues](https://github.com/csdaw/ggprism/issues). Bug reports should be
submitted with a minimal reproducible example, e.g. using the
[`reprex`](https://reprex.tidyverse.org) package.

Pull requests for contributions should be made against the `dev` branch.
Some ways to contribute might include:

-   Updates and corrections to documentation
-   Examples and vignettes for existing functions
-   Bug fixes
-   New functions with associated documentation, examples, and tests

## Cite

Charlotte Dawson (2020), *ggprism: A ggplot2 extension inspired by
GraphPad Prism*. R package version 0.0.0.9000,
<https://csdaw.github.io/ggprism/>.

## More Examples

Recreates [this
figure](https://cdn.graphpad.com/assets/0.27.0/images/srcset/prism-get-actionable-help-w1920-800.png)
from the GraphPad Prism website. See this vignette for the source code
and step-by-step instructions.

<img src="man/figures/README-ex2-1.png" width="400" /><img src="man/figures/README-ex2-2.png" width="400" />

Recreates Figure 2B from [this
paper](https://doi.org/10.1038/s42003-020-0889-1), which was originally
created using GraphPad Prism. See this vignette for the source code and
step-by-step instructions.

<img src="man/figures/README-ex3-1.png" width="400" /><img src="man/figures/README-ex3-2.png" width="400" />
