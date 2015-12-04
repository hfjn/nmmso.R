context("uniform_sphere_points")
test_that("uniform sphere points is working correctly", {
  Y <- uniform_sphere_points(10, 10)
  expect_equal(length(Y), 100)
  expect_equal(typeof(Y), "double")
})