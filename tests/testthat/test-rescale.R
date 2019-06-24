#testthat::context("Scaling")

a <- 1:6
ap <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
an <- c(-1, -0.8, -0.6, -0.4, -0.2, 0)
ab <- c(-1, -0.6, -0.2, 0.2, 0.6, 1)

m <- matrix(a, 2, 3)
mp <- matrix(ap, 2, 3)
mn <- matrix(an, 2, 3)
mb <- matrix(ab, 2, 3)

test_that('vectors scale properly', {
  expect_equal(rescale(a, 'positive'), ap)
  expect_equal(rescale(a, 'negative'), an)
  expect_equal(rescale(a, 'both'), ab)
})

test_that('matrices remain matrices', {
  expect_true(is.matrix(rescale(m, 'positive')))
  expect_true(is.matrix(rescale(m, 'negative')))
  expect_true(is.matrix(rescale(m, 'both')))
})

test_that('matrix dimensions do not change', {
  expect_identical(dim(m), dim(rescale(m, 'positive')))
  expect_identical(dim(m), dim(rescale(m, 'negative')))
  expect_identical(dim(m), dim(rescale(m, 'both')))
})

test_that('matrices scale properly', {
  expect_equal(rescale(m, 'positive'), mp)
  expect_equal(rescale(m, 'negative'), mn)
  expect_equal(rescale(m, 'both'), mb)
})



