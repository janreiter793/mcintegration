new_mcintegrator <- function(fn, a, b) {
  stopifnot(is.function(fn))
  stopifnot(is.numeric(a))
  stopifnot(is.numeric(b))
  stopifnot(a < b)

  return(structure(integrator(fn, a, b), class = "mcintegrator"))
}

validate_mcintegrator <- function(x) {
  stopifnot(inherits(x, "mcintegrator"))
  if(length(attr(x, "names")) != 8) {
    stop("mcintegrator have not obtained all things")
  }
  return(x)
}

mcintegrator <- function(fn, a, b) {
  return(validate_mcintegrator(new_mcintegrator(fn, a, b)))
}
