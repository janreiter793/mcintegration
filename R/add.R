#' Add mcintegration objects
#'
#' @param x An mcintegration object.
#' @param y An mcintegration object.
#'
#' @return Returns a numeric value corresponding to the result of x added with
#'         the result of y.
#' @export
#'
#' @examples
#' x <- mcintegrator(sin, 0, pi / 2)
#' y <- mcintegrator(function(x) {-x}, -1, 0)
#' add(x, y)
add <- function(x, y) {
  return(x$result + y$result)
}
