# Extract Fill Colours from Prism Colour Schemes
#
# Extract themes from Prism colour scheme .txt files. Text files were
# generated from .clr files included with an installed version of Prism on
# MacOS using a Swift script.
#
# See:
#
# - https://stackoverflow.com/questions/49379341
#
library(readr)
library(dplyr)
library(purrr)
library(here)

scheme_dir <- "data-raw/schemes"

scheme_files <- dir(scheme_dir, pattern = "\\.txt$",
                    full.names = TRUE, recursive = TRUE)

process_prism_fill <- function(path) {
  fill_palette <- read_delim(path, delim = " ",
                      col_names = c("name", "r", "g", "b", "value")) %>%
    select(name, value) %>%
    filter(grepl("dataSetBarFillColor", name)) %>%
    pull(value)
}


fill_palettes <- map(scheme_files, process_prism_fill)

names(fill_palettes) <- tools::file_path_sans_ext(basename(scheme_files))

# extract second fill palette for special cases
special_cases <- dir(scheme_dir, pattern = "(neon|warm_and_sunny|winter_bright)\\.txt$",
                     full.names = TRUE, recursive = TRUE)

process_special_case <- function(path) {
  fill_palette_alt <- read_delim(path, delim = " ",
                                 col_names = c("name", "r", "g", "b", "value")) %>%
    select(name, value) %>%
    filter(grepl("dataSetBarPatternColor", name)) %>%
    pull(value)
}

fill_palettes_alt <- map(special_cases, process_special_case)

names(fill_palettes_alt) <- paste0(tools::file_path_sans_ext(basename(special_cases)),"2")

# combine all fill palettes in a single list
fill_palettes <- c(fill_palettes, fill_palettes_alt)

# remove neon and replace with neon2
fill_palettes <- within(fill_palettes, rm("neon"))
names(fill_palettes) <- gsub("neon2", "neon", names(fill_palettes))

# sort alphabetically
fill_palettes <- fill_palettes[order(names(fill_palettes))]

cat(yaml::as.yaml(fill_palettes),
    file = here("data-raw", "fill-palettes.yml"))
