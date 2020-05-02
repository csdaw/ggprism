# Extract Themes from Prism Colour Schemes
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
library(stringr)
library(purrr)
library(here)

scheme_dir <- "data-raw/schemes"

scheme_files <- dir(scheme_dir, pattern = "\\.txt$",
                    full.names = TRUE, recursive = TRUE)

process_prism_scheme <- function(path) {
  theme <- read_delim(path, delim = " ",
             col_names = c("name", "r", "g", "b", "value")) %>%
    select(name, value) %>%
    filter(!str_detect(name, "^dataSet") | str_detect(name, "dataSetLegendColor1"))
}


themes <- map(scheme_files, process_prism_scheme)

names(themes) <- tools::file_path_sans_ext(basename(scheme_files))

cat(yaml::as.yaml(themes, column.major = FALSE),
    file = here("data-raw", "themes.yml"))
