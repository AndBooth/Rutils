
context("Testing utility functions")


test_that("Test all_na function", {

  expect_true(all_na(rep(NA, 3)))

  expect_error(all_na(matrix(data = NA, nrow = 3, ncol = 3)))

  expect_false(all_na(c(1, 2, NA)))

  nadf <- data.frame(x = rep(NA, 3), y = c(1 ,2, 3))

  expect_true(any(purrr::map_lgl(nadf, ~ all_na(.))))
  expect_false(all(purrr::map_lgl(nadf, ~ all_na(.))))

})


test_that("Test percent_empty function", {

  expect_equal(percent_empty(c(1, NA, 1, 1)), 25)

  expect_error(percent_empty(matrix(2, 2)))

  nadf <- data.frame(x = rep(NA, 3), y = c(1 ,2, 3))
  percents <- purrr::map_dbl(nadf, ~ percent_empty(.))
  expect_true(all(percents == c(100, 0)))

})


test_that("Test repeating_column function", {

  expect_true(repeating_column(rep(1, 5)))
  expect_false(repeating_column(c(1, 1, 1, 2)))

  lowvarcol <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1.0001)

  expect_false(repeating_column(lowvarcol))
  expect_true(repeating_column(lowvarcol, roundx = 8))
})


test_that("Test sample_df function", {

  testdf <- data.frame(x = seq(1, 10, 1), y = rnorm(10))

  expect_error(sample_df(testdf, 11))

  testdf_sample <- sample_df(testdf, 5)

  expect_s3_class(testdf_sample, "data.frame")

  expect_lt(nrow(testdf_sample), nrow(testdf))
})


test_that("Test order_df function", {

  expect_error(order_df(mtcars))

  dec <- order_df(mtcars, ~ mpg, decreasing = T)
  inc <- order_df(mtcars, ~ mpg)

  expect_true(identical(inc[nrow(inc),], dec[1,]))

})


test_that("Test group_proportions function",{

  test_df <- data.frame(x = c("A","A","B","B","C","C"), y = c("1","2","3","4","1","1"))

  props1 <- group_proportions(test_df, "x")
  props2 <- group_proportions(test_df, "x", "y")

  expect_equal(nrow(props1), 3)
  expect_equal(nrow(props2), 5)
  expect_length(props1, 3)
  expect_length(props2, 4)

  expect_error(group_proportions(test_df, "z"))
})

