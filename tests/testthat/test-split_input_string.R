
test_that("string with no separators is returned as is", {
  expect_identical(split_input_string('onetwo'), 'onetwo')
})

test_that("string is split", {
  expect_identical(split_input_string('one,two'), c('one', 'two'))
  expect_identical(split_input_string('one;two'), c('one', 'two'))
  expect_identical(split_input_string('one:two'), c('one', 'two'))
  expect_identical(split_input_string('one&two'), c('one', 'two'))
  expect_identical(split_input_string('one=two'), c('one', 'two'))
  expect_identical(split_input_string('one+two'), c('one', 'two'))
  expect_identical(split_input_string('one/two'), c('one', 'two'))
  expect_identical(split_input_string('one two'), c('one', 'two'))
})

test_that("two separating characters work",{
  expect_identical(split_input_string('one, two'), c('one', 'two'))
})

test_that("more than two separating characters work",{
  expect_identical(split_input_string('one,; two'), c('one', 'two'))
})

test_that("border spaces are removed",{
  expect_identical(split_input_string(' one, two '), c('one', 'two'))
})

test_that("errors are triggered", {
  expect_error(split_input_string(3245))
  expect_error(split_input_string(c('one two', 'three,four')))
})

test_that("illegal separators trigger errors", {
  expect_error(split_input_string(c('one.two'), sep = '.'))
  expect_error(split_input_string(c('one-two'), sep = '-'))
  expect_error(split_input_string(c('one_two'), sep = '_'))
  expect_error(split_input_string(c('one\two'), sep = '\\'))
})
