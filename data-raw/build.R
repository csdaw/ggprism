suppressPackageStartupMessages({
  library("purrr")
  library("tibble")
  library("rlang")
  library("here")
  library("yaml")
})

ggprism_data <- new_environment()

# build themes
load_themes <- function() {
  out <- yaml.load_file(here("data-raw", "themes.yml"))

  map(out, ~ map_dfr(., as_tibble))
}

ggprism_data$themes <- load_themes()

# build colour palettes


# build fill palettes


# build shape palettes


# save to rda
ggprism_data <- as.list(ggprism_data)

usethis::use_data(ggprism_data, overwrite = TRUE)


