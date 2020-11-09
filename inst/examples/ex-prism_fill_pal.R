library(ggplot2)

## list all available fill palettes and their lengths
lengths(ggprism_data$fill_palettes)

## select some colours from a palette
prism_fill_pal(palette = "summer")(4)

## see all the colours in a specific palette
# define a function for convenience
library(scales)

show_palette <- function(palette) {
  scales::show_col(
    prism_fill_pal(palette = palette)(
      attr(prism_fill_pal(palette = palette), "max_n")
    )
  )
}

# show the colours in the palette "pearl"
show_palette("floral")
