context('Inserting zeros into elements of vector')

intg <- c(1L, 2L, 10L, 20L)
doub <- intg / 1.0
doub2 <- intg + 0.1
char <- as.character(intg)
charNA <- c(char, NA_character_)
fact <- as.factor(char)
wells <- paste0('A', intg)
foo <- function() {}
df <- data.frame()


test_that('zeros inserted into character', {
  expect_identical(insert_zeros(char), c('10', '20', '10', '20'))
  expect_identical(insert_zeros(char), insert_zeros(char, zeros = 'auto', after = 1))
  expect_identical(insert_zeros(char, zeros = 2), c('100', '200', '1000', '2000'))
  expect_identical(insert_zeros(char, zeros = 'auto', after = 0), c('01', '02', '10', '20'))
  expect_identical(insert_zeros(charNA), c(insert_zeros(char), NA_character_))
  expect_identical(insert_zeros(charNA, after = 0), c(insert_zeros(char, after = 0), NA_character_))
})

test_that('other methods work', {
  expect_identical(insert_zeros(char), insert_zeros(fact))
  expect_identical(insert_zeros(char), insert_zeros(intg))
  expect_identical(insert_zeros(intg), insert_zeros(doub))
})

test_that('default method works', {
  expect_error(insert_zeros(foo))
  expect_error(insert_zeros(df))
  expect_error(insert_zeros(doub2))
})

char0 <- insert_zeros(char, zeros = 0)
test_that('test_defenses', {
  expect_message(insert_zeros(char, zeros = 0))
  expect_identical(char0, char)
  expect_message(insert_zeros(intg))
  expect_warning(insert_zeros(fact))
  expect_error(insert_zeros(char, after = -1))
  expect_error(insert_zeros(char, after = 2))
})

test_that('primary job is done properly', {
  expect_identical(insert_zeros(wells), c('A01', 'A02', 'A10', 'A20'))
})
