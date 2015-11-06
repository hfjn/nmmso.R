#' @title evaluate
#' 
#' @param nmmso_state Structure holding state of swarm.
#' @param chg 
#' @param problem_function String containing name of function to be optimised.
#' @param problem_function_params Meta-parameters needed by problem function.
#' @return 
#' nmmso_state = Structure holding state of swarm.
#' mode_shift =
#' y = 
#' 
#' @export
evaluate <-
  function(nmmso_state, chg, problem_function) {
    print(nmmso_state$active_modes[[1]])
    y = feval(
      problem_function, nmmso_state$active_modes[[chg]]$swarm$new_location
    )
    mode_shift = 0
    
    if (y > nmmso_state$active_modes[[chg]]$swarm$mode_value) {
      nmmso_state$active_modes[[chg]]$swarm$mode_location = nmmso_state$active_modes[[chg]]$swarm$new_location
      nmmso_state$active_modes[[chg]]$swarm$mode_value = y
      mode_shift = 1
    }
    
    nmmso_state$active_modes[[chg]]$swarm$history_location[nmmso_state$active_modes[[chg]]$swarm$shifted_loc,] = nmmso_state$active_modes[[chg]]$swarm$new_location
    nmmso_state$active_modes[[chg]]$swarm$history_vaues[nmmso_state$active_modes[[chg]]$swarm$shifted_loc] = y
    
    # if better than personal best for swarm member - then replace
    if (y > nmmso_state$active_modes[[chg]]$swarm$pbest_values[nmmso_state$active_modes[[chg]]$swarm$shifted_loc]) {
      nmmso_state$active_modes[[chg]]$swarm$pbest_values[nmmso_state$active_modes[[chg]]$swarm$shifted_loc] = y
      nmmso_state$active_modes[[chg]]$swarm$pbest_location[nmmso_state$active_modes[[chg]]$swarm$shifted_loc,] = nmmso_state$active_modes[[chg]]$swarm$new_location
    }
    
    # change the x and y of the curren active mode
    nmmso_state$active_modes[[chg]]$X[nmmso_state$index,] = nmmso_state$active_modes[[chg]]$swarm$new_location
    nmmso_state$active_modes[[chg]]$Y[nmmso_state$index] = y
    nmmso_state$index = nmmso_state$index + 1
    
    # return the result
    list("nmmso_state" = nmmso_state, "mode_shift" = mode_shift, "y" = y)
  }
