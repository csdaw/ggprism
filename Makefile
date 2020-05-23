.PHONY: build
build: data docs readme

.PHONY: readme
readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'
	rm -f README.html

.PHONY: docs
docs:
	Rscript -e 'devtools::document()'

.PHONY: data
data:
	Rscript data-raw/build.R

data-raw/build.R: data-raw/*.yml
	Rscript data-raw/fill-palettes.R
	Rscript data-raw/shape-palettes.R
	Rscript data-raw/themes.R

.PHONY: check
check:
	Rscript -e 'devtools::check()'
