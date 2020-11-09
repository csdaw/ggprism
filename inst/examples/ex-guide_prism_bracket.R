## base plot
base <- ggplot(mpg, aes(x = as.factor(cyl), y = hwy)) +
  geom_jitter(width = 0.2) +
  theme(axis.line = element_line(colour = "black"))

## use brackets on x axis
# if not specified, the width of the brackets is guessed
base + scale_x_discrete(guide = "prism_bracket")

# you can add brackets using the guide function as well
base + guides(x = "prism_bracket")

## works with coord_flip
base + scale_x_discrete(guide = "prism_bracket") +
  coord_flip()

## adjust bracket width
base + scale_x_discrete(guide = guide_prism_bracket(width = 0.12))

## make brackets point inward
base + scale_x_discrete(guide = guide_prism_bracket(width = 0.12, outside = FALSE))

## change colour with the usual axis.line, axis.ticks, axis.text elements
base + scale_x_discrete(guide = guide_prism_bracket(width = 0.12, outside = FALSE)) +
  theme(axis.line.x = element_line(colour = "red"),
        axis.ticks.x = element_line(colour = "blue"),
        axis.text.x = element_text(colour = "green"))
