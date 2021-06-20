# ggprism (development version)

* Add `parse` argument to `add_pvalue()` which allows the use of plotmath
expressions for the text labels.

# ggprism 1.0.3

* Fix one of the figures in p-values vignette
* Replace some `annotation_ticks()` tests to ensure compatibility with
the upcoming ggplot2 v3.4.0

# ggprism 1.0.2

* Make `add_pvalue()` more flexible to different input columns
* Fix errors in p-values vignette so it works again
* Increase test coverage
* Re-write some tests so they pass for R 3.6.3
* Update README (fix some incorrect links and update citation info)

# ggprism 1.0.1

* First CRAN submission
* Remove separate LICENSE file
* Reword package description in DESCRIPTION.
* Fix various typos in vignettes and function documentation.
* Move the two example vignettes to be online only, i.e. not included
in the installed package.

# ggprism 1.0.0

* Initial GitHub release. Provide functions for changing ggplot2 themes, 
axis guides, colours, and fills, and adding p-value brackets. Also include
small data set with fly wing data.
