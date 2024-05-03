integrator <- function(fn, a, b) {
  upper <- max(c(fn(a), fn(b)))

  fineness <- ceiling(upper * (b - a) * 500)

  x <- runif(fineness, min = a, max = b)
  y <- runif(fineness, min = 0, max = upper)
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
