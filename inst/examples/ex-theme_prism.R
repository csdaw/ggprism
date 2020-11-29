library(ggplot2)

# see ?preview_theme for a convenient function to preview ggprism themes
# before using theme_prism

## base plot
base <- ggplot(mpg, aes(x = displ, y = cty, colour = class)) +
  geom_point()

## default palette is "black_and_white"
## default base_size is 14 (compared with 11 for theme_grey)
base +
  theme_prism()

## try some other palettes
base +
  theme_prism(palette = "office")

base +
  theme_prism(palette = "flames")

## try matching the theme_prism palette with same colour palette
base +
  theme_prism(palette = "stained_glass") +
  scale_color_prism(palette = "stained_glass")

base +
  theme_prism(palette = "candy_bright") +
  scale_color_prism(palette = "candy_bright")

## change the font face
base +
  theme_prism(base_fontface = "plain")

## change the font family
base +
  theme_prism(base_family = "serif")

## base_line_size scales automatically as you change base_size
base +
  theme_prism(base_size = 10)

## but you can also change it manually
base +
  theme_prism(base_size = 16, base_line_size = 0.8)

## easily change x axis text angle
base +
  theme_prism(axis_text_angle = 45)

## add a border (need to turn off clipping)
base +
  theme_prism(border = TRUE) +
  coord_cartesian(clip = "off")

## change border thickness
base +
  theme_prism(border = TRUE, base_rect_size = 2) +
  coord_cartesian(clip = "off")
