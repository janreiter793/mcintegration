print.mcintegrator <- function(x) {
  cat(x$result)
}

summary.mcintegrator <- function(x) {
  cat(
    paste0(
      "Monte Carlo integration\n",
      "Result: ", x$result, ",\n",
      "fineness: ", x$fineness, ",\n",
      "Interval: [", x$interval[1], ";", x$interval[2], "].\n"
    )
  )
}

plot.mcintegrator <- function(x) {
  distance <- (x$interval[2] - x$interval[1])
  x.func <- seq(x$interval[1] - distance,
                x$interval[2] + distance,
                by = (3 * distance) / (x$fineness))
  y.func <- sapply(x.func, x$func)
  plot(x = x.func, y = y.func, type = "l", lwd = 4, col = "red",
       xlab = "x", ylab = "y"); grid()
  points(x$points_below, col = "blue", pch = 20)
  points(x$points_above, col = "pink", pch = 20)
  lines(x = x.func, y = y.func, lwd = 4, col = "red")
  abline(v = 0, lwd = 2)
  abline(h = 0, lwd = 2)
}

printthis <- function(x) {
  print(x)
}

plotthis <- function(x) {
  plot(x)
}

summarythis <- function(x) {
  summary(x)
}
