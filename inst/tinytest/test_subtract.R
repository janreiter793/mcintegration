x <- mcintegrator(sin, 0, pi / 2)
y <- mcintegrator(function(x) { x }, 0, 5)

expect_true(subtract(x, y) - (x$result - y$result) < 1e-2)

x <- mcintegrator(cos, 3 * pi / 2, 2 * pi)
y <- mcintegrator(function(x) { x }, 0, 2)

expect_true(subtract(x, y) - (x$result - y$result) < 1e-2)
