## base plot
base <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

## add minor ticks to x and y axes
base +
  scale_x_continuous(
    limits = c(0, 6),
    guide = "prism_offset_minor"
  ) +
  scale_y_continuous(
    limits = c(10, 35),
    guide = "prism_offset_minor"
  ) +
  theme(axis.line = element_line(colour = "black"))

## adjust number of minor ticks by adjusting minor breaks
base +
  scale_x_continuous(
    limits = c(0, 6),
    minor_breaks = seq(0, 6, 0.5),
    guide = "prism_offset_minor"
  ) +
  scale_y_continuous(
    limits = c(10, 35),
    minor_breaks = seq(10, 35, 1.25),
    guide = "prism_offset_minor"
  ) +
  theme(axis.line = element_line(colour = "black"))

## adjust the length of major ticks with the usual axis.ticks.length element
base +
  scale_x_continuous(
    limits = c(0, 6),
    minor_breaks = seq(0, 6, 0.5),
    guide = "prism_offset_minor"
  ) +
  scale_y_continuous(
    limits = c(10, 35),
    minor_breaks = seq(10, 35, 1.25),
    guide = "prism_offset_minor"
  ) +
  theme(
    axis.line = element_line(colour = "black"),
    axis.ticks.length = unit(10, "pt")
  )

## adjust the length of minor ticks with a new prism.ticks.length element
base +
  scale_x_continuous(
    limits = c(0, 6),
    minor_breaks = seq(0, 6, 0.5),
    guide = "prism_offset_minor"
  ) +
  scale_y_continuous(
    limits = c(10, 35),
    minor_breaks = seq(10, 35, 1.25),
    guide = "prism_offset_minor"
  ) +
  theme(
    axis.line = element_line(colour = "black"),
    axis.ticks.length = unit(10, "pt"),
    prism.ticks.length = unit(5, "pt")
  )

## to get log10 minor ticks just use a log10 scale and set the minor breaks
ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point(na.rm = TRUE) +
  scale_x_log10(limits = c(1e0, 1e4),
                minor_breaks = rep(1:9, 4)*(10^rep(0:3, each = 9)),
                guide = "prism_offset_minor") +
  theme(axis.line = element_line(colour = "black"))
