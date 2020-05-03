suppressPackageStartupMessages({
  library("purrr")
  library("tibble")
  library("rlang")
  library("here")
  library("yaml")
})

ggprism_data <- new_environment()

load_themes <- function() {
  out <- yaml.load_file(here("data-raw", "themes.yml"))

  map(out, ~ map_dfr(., as_tibble))
}
ggprism_data$themes <- load_themes()


load_fill_palettes <- function() {
  out <- yaml.load_file(here("data-raw", "fill-palettes.yml"))

  out
}
ggprism_data$fill_palettes <- load_fill_palettes()


load_shape_palettes <- function() {
  out <- yaml.load_file(here("data-raw", "shape-palettes.yml"))

  map(out, ~ map_dfr(., as_tibble))
}
ggprism_data$shape_palettes <- load_shape_palettes()


# save to rda
ggprism_data <- as.list(ggprism_data)

usethis::use_data(ggprism_data, overwrite = TRUE)


