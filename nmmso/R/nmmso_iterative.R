NMMSO_iterative(
  swarm_size, problem_function, problem_function_params, max_evaluations, mn, mx, evaluations, nmmso_state, max_evol, tol_val
) {
  
  # test if all variables are correctly initialized
  # TODO: Test this
  if(evaluations < 0){
    stop('A algorithm can only be run a positive number of times')
  }
  
  # test if max_evol has a value otherwise assign one
  # TODO: Test this
  if(is.null(max_evol)){
    print('default max_evol used, set at 100')
    max_evol = 100
  }
  
  # test if max_evol is smaller than 0, which is not usable
  # TODO: Test this
  if(max_evol <= 0){
    print('Max_eval cannot be nagative or zero, default max_eval used, set at 100')
    max_evol = 100;
  }
  
  # test if tol_val is existing, otherwise create it
  if(is.null(tol_val)){
    tol_val = 10^-6
  }
  
  
  
  if(evaluations == 0){
    # preallocate matrices for speed
    nnmso_state$X <- matrix(0, max_evaluations + 500, length(mx))
    nnmso_state$Y <- matrix(0, max_evaluations + 500, 1)
    nmmso_state$index = 1
    nmmso_state$converged_modes = 0
    
    # get initial locations
    nnmso_state = get_initial_locations(nmmso_state, mn, mx)
    # the active modes function is never implemented in matlab. is it a list?
    nnmso_state$active_modes <- list()
    nnmso_state$active_modes[1] <- list()
    result <- evaluate_first(problem_function, problem_function_params, nnmso_state, swarm_size, mn, mx)
    nnmso_state$active_modes[1]$swarm = matrix(0, 1,1)
  }
  
  
  
}

# extracts the modes from the given nmmso_state
extract_modes <- function(nmmso_state) {
  RES = matrix(0, length(nnmso_state$active_modes, length(nnmso_state$active_modes[1]$swarm$mode_location)))
  RES_Y = matrix(0, length(nmmso_state$active_modes), 1)
  
  for(i in 1:length(nmmso_state$active_modes)){
    RES(i, ) = nnmso_state$active_modes(i)$swarm$mode_location
    RES_Y(i) = nnmso_state$active_modes(i)$swarm$mode_value
  }
  
  return(list(RES, RES_Y))
}

# calculates the initial locations of the algorithm
get_initial_locations(nmmso_state, mn, mx){
  #point wise product as new locations
  nnmso_state$active_modes[1]$swarm$new_location = rand(length(mx))*(mx-mn) + mn
  nmmso_state$active_modes_changed[1] = 1
}

evaluate_first(swarm, problem_function, problem_function_params, nmmso_state, swarm_size, mn, mx){
  
  # from original:
  ## new location is the only solution thus far in mode, so by definition is
  ## also the mode estimate, and the only history thus far
  y = feval(problem_func, swarm$new_location, problem_func_params)
  #gbest location
  swarm$mode_location = swarm$new_location
  
  #gbest value
  swarm.mode_value = y
  
  # intialize containers for swarm elements
  
  # current locations of swarm
  swarm$history_locations = matrix(0, length(swarm$mode_location))
  swarm$history_values = matrix(1, swarm_size, 1) * -Inf
  
  swarm$pbest_locations = matrix(0, swarm_size, length(swarm$mode_location))
  
}

merge_swarms <- function(nmmso_state, problem_function, problem_function_params, mn, mx){
  
}

evaluate <- function(nmmso_state, chg, problem_function, problem_function_params){
  
}

merge_swarms_together <- function(swarm1, swarm2){
  
}

evaluate_mid <- function(nmmso_state, chg, problem_function, test_function_params){
  
}

increment_swarm <- function(nmmso_state, chg, mn, mx, swarm_size){
  
}

evaluate_new_locations <- function(nmmso_state, problem_function_params, I){
  
}

evolve <- function(nmmso_state, problem_function, mn, mx, problem_function_params, max_evol, swarm_size){
  
}

hive <- function(nmmso_state, problem_function, mn, mx, problem_function_params, max_evol, swarm_size){
  
}

random_new <- function(nmmso_satte, problem_function, mn, mx, problem_function_params, swarm_size){
  
}

UNI <- function(x1, x2){
  
}

uniform_sphere_points <- function(n,d){
  
}
# helper functions which imitates the behavior of the Matlab feval
feval <-function(f,...){f(...)}