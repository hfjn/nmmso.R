context("new_dist2")
test_that("new_dist2", {
  x = rbind(c(1, 2, 3), 
            c(4, 5, 6),
            c(7, 8 ,9))

  c = rbind(c(9, 8, 7), 
            c(6, 5, 4), 
            c(3, 2, 1))

  r = rbind(c(116, 35,  8),
            c(35, 8,  35),
            c(8, 35, 116))

  expect_true(new_dist2(x, c) == r)

})