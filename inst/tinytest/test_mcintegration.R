a <- mcintegrator(sin, 0, pi / 2)
b <- mcintegrator(function(x) { x }, 0, 5)
c <- mcintegrator(cos, 3 * pi / 2, 2 * pi)
d <- mcintegrator(function(x) { x }, 0, 2)

# Result should always be non-negtaive
expect_true(a$result >= 0)
expect_true(b$result >= 0)
expect_true(c$result >= 0)
expect_true(d$result >= 0)

# Intervals should be non-empty
expect_true(a$interval[2] - a$interval[1] > 0)
expect_true(b$interval[2] - b$interval[1] > 0)
expect_true(c$interval[2] - c$interval[1] > 0)
expect_true(d$interval[2] - d$interval[1] > 0)

# Upper bound should be positive
expect_true(a$upper > 0)
expect_true(b$upper > 0)
expect_true(c$upper > 0)
expect_true(d$upper > 0)

# The fineness should correspond to these values
expect_true(a$fineness - ceiling(1 * pi / 2 * 500) < 1e-2)
expect_true(b$fineness - ceiling(5 * 5 * 500) < 1e-2)
expect_true(c$fineness - ceiling(1 * pi / 2 * 500) < 1e-2)
expect_true(d$fineness - ceiling(2 * 2 * 500) < 1e-2)
