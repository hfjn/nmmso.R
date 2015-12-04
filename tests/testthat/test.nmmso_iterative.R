context("nmmso_iterative")
library(pracma)

gens = c(50000 * matrix(1, 1, 5), 20000, 20000, 40000, 40000, 20000*matrix(1,1, 4), 40000 * matrix(1, 1, 7))

nopt = c(2, 5, 1, 4, 2, 18, 36, 81, 216, 12, 6, 8, 6, 6, 8, 6, 8, 6, 6, 8)

dims = c(1, 1, 1, 2, 2, 2, 2, 3, 3, 2, 2, 2, 2, 3, 3, 5, 5, 10, 10, 20)

mn = list(0, 0, 0, c(-6, -6), c(-1.9, -1.9), c(-10, -10), c(0.25, 0.25), c(-10, -10, -10), c(0.25, 0.25, 0.25), c(0,0), -5*matrix(1, 1, 2), -5*matrix(1, 1, dims[12]), -5*matrix(1, 1, dims[13]), -5*matrix(1, 1, dims[14]), -5*matrix(1, 1, dims[15]), -5*matrix(1, 1, dims[16]), -5*matrix(1, 1, dims[17]), -5*matrix(1, 1, dims[18]), -5*matrix(1, 1, dims[19]), -5*matrix(1, 1, dims[20]))

mx = list(30, 1, 1, c(6, 6), c(1.9, 1.1), c(10,10), c(10, 10), c(10, 10, 10), c(10, 10, 10), c(1, 1), 5*matrix(1,1,2), 5*matrix(1, 1, dims[12]), 5*matrix(1, 1, dims[13]), 5*matrix(1, 1, dims[14]), 5*matrix(1,1,dims[15]), matrix(1,1,dims[16]), 5*matrix(1,1,dims[17]), 5*matrix(1,1,dims[18]), 5*matrix(1,1,dims[19]), 5*matrix(1,1,dims[20]))

max_evaluations = 100

nmmso_state = list()
fit <- function(x) sin(5 * pi * x)^6

# preallocate matrices for speed
nmmso_state$X <- matrix(0, 100 + 500, length(mx))
nmmso_state$Y <- matrix(0, 100 + 500, 1)
nmmso_state$index = 1
nmmso_state$converged_modes = 0

# initialize active modes as a list and give the sub "Modes" lists aswell
nmmso_state = list(X = matrix(0, max_evaluations + 500, length(mx)), Y = matrix(0, max_evaluations + 500, 1))
nmmso_state$index = 1
nmmso_state$converged_modes = 0

# initialize active modes as a list and give the sub "Modes" lists aswell
nmmso_state$swarms <- list(list(swarm = list()))





#'
#'
#'
test_that("feval is working",{
  expect_equal(sqrt(10), feval(sqrt, 10))
})


#'
#'
#'
test_that("nmmso_iterative main function is working correctly", {
  expect_error(NMMSO_iterative())
  # Test that evaluations can not be < 0 
  expect_error(NMMSO_iterative(100, sqrt, 100, max_evaluations = 100, mn = 1, mx = 2, evaluations = -1))
})

#'
#'
#'
test_that("random_new", {

})

#'
#'
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


#'
#'
#'
test_that("UNI", {
  x1 <- (1:5)
  x2 <- (6:10)
  result <- UNI(x1, x2)
  expect_true(any(result$x_c > 5) && any(result$x_d < 6))
})

#'
#'
#'
test_that("get_initial_locations",{
  nmmso_state = get_initial_locations(nmmso_state, as.numeric(mn[2]), as.numeric(mx[2]))
  str(nmmso_state)
  expect_true(nmmso_state$swarms[[1]]$new_location[1,1] > 0)
  expect_true(nmmso_state$swarms_changed == 1)
})

test_that("evaluate_first",{
  #result <- evaluate_first(nmmso_state$swarms[1], fit, nmmso_state, swarm_size = 10*length(mx[2]), as.numeric(mn[2]), as.numeric(mx[2]))
  #nmmso_state = result$nmmso_state
  #swarm = result$swarm

  #swarm$mode_location
})
