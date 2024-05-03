new_mcintegrator <- function(x) {
  stopifnot(is.list(x))
  return(structure(x, class = "mcintegrator"))
}

validate_mcintegrator <- function(x) {
  stopifnot(inherits(x, "mcintegrator"))
  if(length(attr(x, "names")) != 8) {
    stop("mcintegrator have not obtained all things")
  }
  return(x)
}

#' Definite integration of non-negative monotone functions using Monte Carlo
#'
#' @param fn A univariate function that is non-negative and monotone in the interval \[a;b\].
#' @param a Start of integration interval.
#' @param b End of integration interval.
#'
#' @return Returns an object with class mcintegrator. See
#'         ?integrator for details.
#' @export
#'
#' @examples
#' x <- mcintegrator(sin, 0, pi / 2)
#' y <- mcintegrator(function(x) {-x}, -1, 0)
mcintegrator <- function(fn, a, b) {
  stopifnot(is.function(fn))
  stopifnot(is.numeric(a))
  stopifnot(is.numeric(b))
  stopifnot(a < b)
  stopifnot(fn(a) > -1e-2)
  stopifnot(fn(b) > -1e-2)

  return(validate_mcintegrator(new_mcintegrator(integrator(fn, a, b))))
}
