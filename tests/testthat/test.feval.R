contex("feval")
test_that("feval is working",{
  expect_equal(sqrt(10), feval(sqrt, 10))
})