
v <- 1:3
vzFF <- c(-1, 0, 1)
vzFT <- c(1, 2, 3)
vzTF <- vzFF  / 1.4826
vzTT <- vzFT / 1.4826

vr <- 1:4
vzrFF <- c(-1, 0, 1, 2)
vzrTF <- c(-1, 0, 1, 2) / 1.4826

test_that("regular zscores are computed", {
  # raw data
  expect_equal(zscore(v, robust = F, deviations = F), vzFF)
  # normalized data (deviations from baseline)
  expect_equal(zscore(v, robust = F, deviations = T), vzFT)
  # with reference group
  expect_equal(zscore(vr, robust = F, deviations = F, reference = c(T, T, T, F)), vzrFF)
})

test_that("robust zscores are computed", {
  # raw data
  expect_equal(zscore(v, robust = T, deviations = F), vzTF)
  # normalized data (deviations from baseline)
  expect_equal(zscore(v, robust = T, deviations = T), vzTT)
  # with reference group
  expect_equal(zscore(vr, robust = T, deviations = F, reference = c(T, T, T, F)), vzrTF)
})

test_that("wrong arguments throw errors", {
  expect_error(zscore(letters[1:3]))
})


df1 <- data.frame(well = 1:3, row = rep('A', 3), column = 1:3,
                  var1 = 1:3, var2 = 1:3)
df1zFF <- data.frame(well = 1:3, row = rep('A', 3), column = 1:3,
                     var1 = 1:3, var2 = 1:3,
                     var1_zscore = c(-1, 0, 1), var2_zscore = c(-1, 0, 1))
df1zFT <- data.frame(well = 1:3, row = rep('A', 3), column = 1:3,
                     var1 = 1:3, var2 = 1:3,
                     var1_zscore = c(1, 2, 3), var2_zscore = c(1, 2, 3))
df1zTF <- data.frame(well = 1:3, row = rep('A', 3), column = 1:3,
                     var1 = 1:3, var2 = 1:3,
                     var1_zscore = c(-1, 0, 1) / 1.4826, var2_zscore = c(-1, 0, 1) / 1.4826)
df1zTT <- data.frame(well = 1:3, row = rep('A', 3), column = 1:3,
                     var1 = 1:3, var2 = 1:3,
                     var1_zscore = c(1, 2, 3) / 1.4826, var2_zscore = c(1, 2, 3) / 1.4826)
# for reference well %in% 1:3
df2 <- data.frame(well = 1:4, row = rep('A', 4), column = 1:4,
                  var1 = 1:4, var2 = 1:4)
df2zFF <- data.frame(well = 1:4, row = rep('A', 4), column = 1:4,
                     var1 = 1:4, var2 = 1:4,
                     var1_zscore = c(-1, 0, 1, 2), var2_zscore = c(-1, 0, 1, 2))

test_that("data.frame method works", {
  # regular z scores
  expect_equal(zscore(df1, F, F, var = c('var1', 'var2')), df1zFF)
  # on deviations
  expect_equal(zscore(df1, F, T, var = c('var1', 'var2')), df1zFT)
  # robust z scores
  expect_equal(zscore(df1, T, F, var = c('var1', 'var2')), df1zTF)
  # on deviations
  expect_equal(zscore(df1, T, T, var = c('var1', 'var2')), df1zTT)

  # variables not declared
  expect_equal(zscore(df1, F, F), df1zFF)

  # with reference by logical
  expect_equal(zscore(df2, F, F, reference = c(T, T, T, F)), df2zFF)
  # with reference by predicate
  expect_equal(zscore(df2, F, F, reference = 'well %in% 1:3'), df2zFF)
  expect_equal(zscore(df2, F, F, reference = well %in% 1:3), df2zFF)
})


test_that("errors in cals to data.frame method", {
  expect_error(zscore(df1, variables = 1:2))
  expect_error(zscore(df1, variables = 'var3'))
  expect_error(zscore(df1, variables = 'row'))
})


df3 <- df1
df3$row <- 'B'
df3 <- rbind(df1, df3)
df3 <- dplyr::group_by(df3, row)

df3zFF <- rbind(df1, within(df1, row <- 'B'))
df3zFF$var1_zscore <- vzFF
df3zFF$var2_zscore <- vzFF
df3zFF <- dplyr::group_by(df3zFF, row)

test_that("grouped_df method works", {
  expect_equal(zscore(df3, F, F), df3zFF)
})
