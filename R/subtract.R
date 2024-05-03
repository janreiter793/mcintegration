#' Subtract mcintegration objects
#'
#' @param x An mcintegration object.
#' @param y An mcintegration object.
#'
#' @return Returns a numeric value corresponding to the result of y subtracted
#'         from the result of x.
#' @export
#'
#' @examples
#' x <- mcintegrator(sin, 0, pi / 2)
#' y <- mcintegrator(function(x) {-x}, -1, 0)
#' subtract(x, y)
subtract <- function(x, y) {
  return(x$result - y$result)
}
