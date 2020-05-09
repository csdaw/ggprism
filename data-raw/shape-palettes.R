# Make shapes YAML file
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(here)

shapes_dir <- "data-raw/shapes"

shapes_files <- dir(shapes_dir, pattern = "\\.txt$",
                    full.names = TRUE, recursive = TRUE)

process_prism_shape <- function(path) {
  theme <- read_tsv(path, col_names = TRUE)
}

shape_palettes <- map(shapes_files, process_prism_shape)

names(shape_palettes) <- tools::file_path_sans_ext(basename(shapes_files))

cat(yaml::as.yaml(shape_palettes, column.major = FALSE),
    file = here("data-raw", "shape-palettes.yml"))
