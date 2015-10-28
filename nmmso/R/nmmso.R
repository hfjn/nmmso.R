NMMSO = function(swarm_size, problem_function, problem_function_params, max_evaluations, mn, mx, max_evol = 100, tol_val = 10 ^ -6) {
  if(max_evol <= 0) {
    sprintf('Max_eval cannot be negative or zero, default max_eval used, set at 100')
    max_evol = 100
  }
  
  # At start no evaluations used, and NMMSO state is empty
  mode_loc_after = list()
  mode_y_after = list()
  evaluations_after = list()
  nmmso_state = list()
  evaluations_after = 0
  
  while(evaluations_after < max_evaluations) {
    mode_loc_before = mode_loc_after
    mode_y_before = mode_y_before
    evaluations_before = evaluations_after
    
    nmmso_iterative = NMMSO_iterative(swarm_size, problem_function, problem_function_params, max_evaluations, mn, mx, evaluations_after, nmmso_state, max_evol, tol_val)
    mode_loc_after = nmmso_iterative$mode_loc
    mode_y_after = nmmso_iterative$mode_y
    evaluations_after = nmmso_iterative$evaluations
    nmmso_state = nmmso_iterative$nmmso_state
  }
  
  list("mode_loc_before" = mode_loc_before, "mode_y_before" = mode_y_before, "evaluations_before" = evaluations_before, 
       "nmmso_state" = nmmso_state, 
       "mode_loc_after" = mode_loc_after, "mode_y_after" = mode_y_after, "evaluations_after" = evaluations_after)
}