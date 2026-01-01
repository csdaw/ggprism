create_bracket <- function(x, y, xend, yend, tip.length, direction = c("down", "up")) {
  # argument checking
  stopifnot(is.numeric(tip.length) & length(tip.length) == 2)

  direction <- match.arg(direction)

  # Calculate distinace between two input points
  dx <- xend - x
  dy <- yend - y
  len <- sqrt(dx^2 + dy^2)

  # Convert to unit vector
  ux <- dx / len
  uy <- dy / len

  # Rotate unit vector either left of right
  if (direction == "up") {
    perp_x <- -uy
    perp_y <- ux
  } else {
    perp_x <- uy
    perp_y <- -ux
  }

  # Create and return the transformed data frame
  data.frame(
    x = c(x + tip.length[1] * perp_x,
          x,
          xend,
          xend + tip.length[2] * perp_x),
    y = c(y + tip.length[1] * perp_y,
          y,
          yend,
          yend + tip.length[2] * perp_y)
  )
}
