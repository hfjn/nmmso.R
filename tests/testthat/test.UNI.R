context("UNI")
test_that("UNI", {
  x1 <- (1:5)
  x2 <- (6:10)
  result <- UNI(x1, x2)
  expect_true(any(result$x_c > 5) && any(result$x_d < 6))
})