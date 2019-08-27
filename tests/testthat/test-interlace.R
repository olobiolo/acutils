#context('testing interleaving of vectors')

a3 <- letters[1:3]
b3 <- 1:3
a3b3 <- c(a3[1], b3[1], a3[2], b3[2], a3[3], b3[3])
a5 <- letters[1:5]
a5b3 <- c(a3[1], b3[1], a3[2], b3[2], a3[3], b3[3], a5[4], a5[5])

test_that('interleaving two equally long vectors', {
  expect_identical(interlace(a3, b3), a3b3)
})

test_that('interleaving two unequally long vectors', {
  expect_identical(interlace(a5, b3), a5b3)
})
