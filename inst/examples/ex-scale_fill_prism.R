## base plot
base <- ggplot(mtcars, aes(x = mpg, fill = factor(cyl))) +
  geom_density(alpha = 0.75)

## works pretty much the same as ggplot2 scale_fill_manual
base +
  scale_fill_prism(palette = "candy_bright")

## try combining the ggprism colour and fill scales
base2 <- ggplot(mtcars, aes(x = mpg, fill = factor(cyl), colour = factor(cyl))) +
  geom_density(alpha = 0.75)

base2 +
  scale_fill_prism(palette = "floral") +
  scale_colour_prism(palette = "floral")

## change fill scale title in legend
base +
  scale_fill_prism(
    palette = "candy_bright",
    name = "Cylinders"
  )

## change fill labels in legend
base +
  scale_fill_prism(
    palette = "candy_bright",
    name = "Cylinders",
    label = c("4 cyl", "6 cyl", "8 cyl")
  )

## change colour labels in legend with a function
base +
  scale_fill_prism(
    palette = "candy_bright",
    name = "Cylinders",
    label = function(x) paste(x, "cyl")
  )

## change order of colours in legend
base +
  scale_fill_prism(
    palette = "candy_bright",
    name = "Cylinders",
    label = function(x) paste(x, "cyl"),
    breaks = c(8, 4, 6)
  )

## to change which colour is assigned to which cyl
## you need to change the factor levels in the underlying data
base <- ggplot(mtcars, aes(x = mpg,
                           fill = factor(cyl, levels = c(6, 4, 8)))) +
  geom_density(alpha = 0.75)

base +
  scale_fill_prism(
    palette = "candy_bright",
    name = "Cylinders"
  )
