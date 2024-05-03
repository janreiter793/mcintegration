#' Print mcintegrator object
#'
#' @param x An mcintegrator object.
#' @param ... Extra arguments read by print
#'
#' @return Prints the approximation of the integral.
#' @export
#'
#' @examples
#' x <- mcintegrator(sin, 0, pi / 2)
#' print(x)
print.mcintegrator <- function(x, ...) {
  cat(x$result)
}

#' Summarize mcintegrator object
#'
#' @param object An mcintegrator object.
#' @param ... Extra arguments read by summary
#'
#' @return Prints a summary containing result, fineness, and integration interval
#'         of the mcintegrator object.
#' @export
#'
#' @examples
#' x <- mcintegrator(sin, 0, pi / 2)
#' summary(x)
summary.mcintegrator <- function(object, ...) {
  cat(
    paste0(
      "Monte Carlo integration\n",
      "Result: ", object$result, ",\n",
      "fineness: ", object$fineness, ",\n",
      "Interval: [", object$interval[1], ";", object$interval[2], "].\n"
    )
  )
}

#' Plot mcintegrator object
#'
#' @param x An mcintegrator object.
#' @param ... Extra arguments read by plot
#'
#' @return A plot of the function integrated, with points under the function
#'         line in the color blue, and points above the function line in the
#'         color pink.
#' @export
#'
#' @examples
#' x <- mcintegrator(sin, 0, pi / 2)
#' plot(x)
plot.mcintegrator <- function(x, ...) {
  distance <- (x$interval[2] - x$interval[1])
  x.func <- seq(x$interval[1] - distance,
                x$interval[2] + distance,
                by = (3 * distance) / (x$fineness))
  y.func <- sapply(x.func, x$func)
  plot(x = x.func, y = y.func, type = "l", lwd = 4, col = "red",
       xlab = "x", ylab = "y"); graphics::grid()
  graphics::points(x$points_below, col = "blue", pch = 20)
  graphics::points(x$points_above, col = "pink", pch = 20)
  graphics::lines(x = x.func, y = y.func, lwd = 4, col = "red")
  graphics::abline(v = 0, lwd = 2)
  graphics::abline(h = 0, lwd = 2)
}
