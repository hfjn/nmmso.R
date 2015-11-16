context("nmmso_iterative")
("../R/cec_2015_problem_data.R")

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
test_that("add_row", {
  
})

#'
#'
test_that("uniform sphere points is working correctly", {
  Y <- uniform_sphere_points(10, 10)
  expect_equal(length(Y), 100)
  expect_equal(typeof(Y), "double")
})

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
  
  # Test that max_evol can not be <= 0
  expect_error(NMMSO_iterative(100, sqrt, 100, max_evaluations = 100, mn = 1, mx = 2, evaluations = 0, max_evol = -1))
})

#'
#'
#'
test_that("random_new", {
  
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
  expect_true(length(nmmso_state$swarms[1]$new_location[1,1]) > 0)
  expect_true(nmmso_state$swarms_changed == 1)
})

test_that("evaluate_first",{
  result <- evaluate_first(nmmso_state$swarms[1], fit, nmmso_state, swarm_size = 10*length(mx[2]), as.numeric(mn[2]), as.numeric(mx[2]))
  #nmmso_state = result$nmmso_state
  #swarm = result$swarm
  
  #swarm$mode_location
})

nmmso_state = result$nmmso_state
swarm = result$swarm
nmmso_state$swarms[1] = swarm

# track number of evaluations taken
evaluations = 1

# keep modes in matrices for efficiency on some computations
nmmso_state$mode_locations = nmmso_state$swarms[1]$mode_location
nmmso_state$mode_values = nmmso_state$swarms[1]$mode_value
nmmso_state$tol_val = 10^-6

result = merge_swarms(nmmso_state, fit, as.numeric(mn[2]), as.numeric(mx[2]))
#result
