## base plot
base <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme(axis.line = element_line(colour = "black"))

## use offset guide via scale_x/y_continuous
base +
  scale_x_continuous(
    limits = c(1, 6),
    breaks = seq(1, 6, by = 1),
    guide = "prism_offset"
  ) +
  scale_y_continuous(
    guide = "prism_offset"
  )

## use offset guide via guides argument
base +
  guides(x = "prism_offset", y = "prism_offset") +
  scale_x_continuous(
    limits = c(1, 6),
    breaks = seq(1, 6, by = 1)
  )

## change colour and tick length with the usual elements
base +
  scale_x_continuous(
    limits = c(0, 6),
    minor_breaks = seq(0, 6, 0.5),
    guide = "prism_offset"
  ) +
  scale_y_continuous(
    limits = c(10, 35),
    minor_breaks = seq(10, 35, 1.25),
    guide = "prism_offset"
  ) +
  theme(
    axis.ticks.length = unit(10, "pt"),
    axis.ticks = element_line(colour = "red"),
    axis.line = element_line(colour = "blue")
  )
