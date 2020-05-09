.PHONY: build
build: readme data

.PHONY: readme
readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'

.PHONY: data
data:
	Rscript data-raw/build.R

data-raw/build.R: data-raw/*.yml
	Rscript data-raw/fill-palettes.R
	Rscript data-raw/shape-palettes.R
	Rscript data-raw/themes.R
