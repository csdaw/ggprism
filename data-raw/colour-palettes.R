# Extract Outline Colours from Prism Colour Schemes
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
                    full.names = TRUE, recursive = TRUE) %>%
  set_names(tools::file_path_sans_ext(basename(.)))

process_prism_colour <- function(path, regex) {
  border_palette <- read_delim(path, delim = " ",
                               col_names = c("name", "r", "g", "b", "value")) %>%
    select(name, value) %>%
    filter(grepl(regex, name)) %>%
    pull(value)
}

# extract border palette from all colour schemes, then
# replace the border palettes from some schemes with the line palette
bad_borders <- paste(c("diazo", "magma", "plasma", "inferno", "viridis",
                       "warm_and_sunny"), collapse = "|") %>%
  grep(., scheme_files, value = TRUE) %>%
  as.list()

border_palettes <- scheme_files %>%
  map(process_prism_colour, regex = "dataSetBorderColor") %>%
  within(rm(list = names(bad_borders)))

border_palettes2 <- bad_borders %>%
  map(process_prism_colour, regex = "dataSetLineColor")

border_palettes <- c(border_palettes, border_palettes2)

# extract line palettes from 17 colour schemes
good_lines <- paste(c("flames", "floral", "mustard_field", "^pastels",
                      "pearl", "prism_dark", "prism_light", "quiet",
                      "spring", "starry", "waves", "blueprint", "fir", "ocean",
                      "sunny_garden", "wool_muffler"), collapse = "|") %>%
  grep(., scheme_files, value = TRUE) %>%
  as.list()

line_palettes <- good_lines %>%
  map(process_prism_colour, regex = "dataSetLineColor")

# extract error bar palettes from 7 colour schemes
good_errors <- paste(c("stained_glass", "warm_pastels",
                       "blueprint", "fir", "ocean",
                       "sunny_garden", "wool_muffler"), collapse = "|") %>%
  grep(., scheme_files, value = TRUE) %>%
  as.list()

error_palettes <- good_errors %>%
  map(process_prism_colour, regex = "dataSetErrorColor")

# combine all colour palettes in 1 list and give them unique names
colour_palettes <- c(border_palettes, line_palettes, error_palettes)

names(colour_palettes) <- make.unique(names(colour_palettes), sep = "") %>%
  sub("2", "3", .) %>%
  sub("1", "2", .)

# sort alphabetically
colour_palettes <- colour_palettes[order(names(colour_palettes))]

cat(yaml::as.yaml(colour_palettes),
    file = here("data-raw", "colour-palettes.yml"))

