#context('checking baseline subtraction')

a <- data.frame(plate = rep(1:2, each = 20),
                well = rep(1:20, 2),
                class = rep(c(rep('base', 3), rep('notbase', 17)), 2),
                one = c(rnorm(20, 3), rnorm(20, 5)),
                two = c(rnorm(20, 13), rnorm(20, 15)))
a$one <- ifelse(a$class == 'base' & a$plate == 1, 1, a$one)
a$one <- ifelse(a$class == 'base' & a$plate == 2, 3, a$one)
a$two <- ifelse(a$class == 'base' & a$plate == 1, 11, a$two)
a$two <- ifelse(a$class == 'base' & a$plate == 2, 5, a$two)

a.b <- a
a.b$one <- a.b$one - 2
a.bb <- a
a.bb$one <- a.bb$one - 2
a.bb$two <- a.bb$two - 8

aa <- dplyr::group_by(a, plate)
aa.b <- aa
aa.b$one <- aa.b$one - rep(c(1,3), each = 20)
aa.bb <- aa
aa.bb$one <- aa.bb$one - rep(c(1,3), each = 20)
aa.bb$two <- aa.bb$two - rep(c(11,5), each = 20)

A.b <- baseline(a, 'one', class == 'base')
A.bb <- baseline(a, c('one', 'two'), class == 'base')
AA.b <- baseline(aa, 'one', class == 'base')
AA.bb <- baseline(aa, c('one', 'two'), class == 'base')


test_that('baseline is subtracted, data.frame', {
  expect_identical(a.b, A.b)
  expect_identical(a.bb, A.bb)
})

test_that('baseline is subtracted, grouped_df', {
  expect_identical(aa.b, AA.b)
  expect_identical(aa.bb, AA.bb)
})

test_that('good specifications of references', {
  expect_identical(baseline(a, 'one', class == 'base'),
                   baseline(a, 'one', "class == 'base'"))
})

test_that('wrong specification of references', {
  expect_error(baseline(a, 'one'))
  expect_error(baseline(a, 'one', class == 'low'))
})
