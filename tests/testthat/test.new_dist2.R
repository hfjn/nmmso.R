context("new_dist2")
test_that("new_dist2", {
  x = matrix(c(1, 2, 3, 4, 5, 6, 7, 8 ,9), 3)
  c = matrix(c(9, 8, 7, 6, 5, 4, 3, 2, 1), 3)

  r = matrix(c(84, 75, 72, 75, 72, 75, 72, 75, 84), 3)
  expect_true(all.equal(new_dist2(x, c), r))


})