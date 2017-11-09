
context("Testing plotting functions")
library(ggplot2)


test_that("Plotting function works correctly", {

  plot1 <- ggplot(mtcars, aes(cyl, mpg))
  save_plot_png(plot1, "testplot")

  expect_true(file.exists("./Plots/testplot.png"))

})
