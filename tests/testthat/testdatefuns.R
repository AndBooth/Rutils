
context("Testing date functions")

test_that("days of week function", {

  date_seq <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), 1)

  expect_true(length(unique(days_of_week(date_seq))) == 7)

  date_seq_fac <- days_of_week(date_seq, first_day = "Thursday")

  expect_true(levels(date_seq_fac)[1] == "Thursday")

})


test_that("months of year function", {

  date_seq <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), 1)

  expect_true(length(unique(months_of_year(date_seq))) == 12)

  date_seq_fac <- months_of_year(date_seq, first_month = "July")

  expect_true(levels(date_seq_fac)[1] == "July")

})
