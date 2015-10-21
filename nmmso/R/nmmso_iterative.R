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
              
}