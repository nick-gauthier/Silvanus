context("Population dynamics")

test_that('linear approximation of pgamma is correct', {
  x <- c(.1, .5, .8, 1)
  expect_equal(approx_pgamma(x, shape = 4.5, scale = 0.1), pgamma(x, shape = 4.5, scale = .1))
})
