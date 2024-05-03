#' Integrates a function using a Monte Carlo
#'
#' @param fn A univariate function that is non-negative and monotone in the interval \[a;b\].
#' @param a Start of integration interval.
#' @param b End of integration interval.
#'
#' @return Returns a list with the following values:
#' * `result`: The approximation of the integral.
#' * `point`: A dataframe with two columns x and y, specifying all the point
#'            coordinates used in the approximation.
#'* `points_above`: A dataframe like `points` containing the points above
#'                  the function line.
#'* `points_below`: A dataframe like 'points_above', but with the
#'                  points below the function line.
#'* `interval`: A vector of two numeric values specifying the integration interval.
#'* `upper`: The largest value of the function in the integration interval.
#'* `fineness`: The number of points used for the approximation.
#'* `func`: The integrand.
#' @export
#'
#' @examples
#' x <- integrator(sin, 0, pi / 2)
#' y <- integrator(function(x) {-x}, -1, 0)
integrator <- function(fn, a, b) {
  upper <- max(c(fn(a), fn(b)))

  fineness <- ceiling(upper * (b - a) * 500)

  x <- stats::runif(fineness, min = a, max = b)
  y <- stats::runif(fineness, min = 0, max = upper)
  area <- upper * (b - a)

  above <- (sapply(x, fn) < y)
  below <- (sapply(x, fn) >= y)

  return(list(result = mean(below) * area,
         points = data.frame(x = x, y = y),
         points_above = data.frame(x = x[above], y = y[above]),
         points_below = data.frame(x = x[below], y = y[below]),
         interval = c(a, b),
         upper = upper,
         fineness = fineness,
         func = fn))
}
