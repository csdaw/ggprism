ggprism
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

Prism colour schemes as ggplot2 themes.

This R package is a work in progress and is currently undocumented and
untested.

## Functions

``` r
# base ggplot2
library(ggplot2)
library(ggprism)
p <- ggplot(midwest, aes(x=percprof, y=poptotal)) +
  geom_point(aes(shape = state, fill = state, colour = state), size = 2)
p
```

<img src="man/figures/ex-base-1.png" width="384" />

### `theme_prism`

``` r
p <- p + theme_prism("black_and_white")
p
```

<img src="man/figures/ex-theme-1.png" width="384" />

### `scale_shape_prism`

``` r
p <- p + scale_shape_prism("filled")
p
```

<img src="man/figures/ex-shape-1.png" width="384" />

### `scale_fill_prism`

``` r
p <- p + scale_fill_prism("prism_light")
p
```

<img src="man/figures/ex-fill-1.png" width="384" />

### `scale_colour_prism`

``` r
# also can use scale_color_prism
p <- p + scale_colour_prism("prism_light") 
p
```

<img src="man/figures/ex-colour-1.png" width="384" />

### `guide_prism_minor`

``` r
p <- p + scale_x_continuous(minor_breaks = seq(0, 20, 1), 
                            guide = "prism_minor") + 
  scale_y_continuous(minor_breaks = seq(0, 5e6, 0.5e6), 
                     guide = "prism_minor") + 
  coord_cartesian(xlim = c(0, 20))
p
```

<img src="man/figures/ex-minor-1.png" width="384" />
