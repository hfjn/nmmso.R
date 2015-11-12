#' @title evaluate
#' 
#' @param nmmso_state Structure holding state of swarm.
#' @param chg 
#' @param problem_function String containing name of function to be optimised.
#' @param problem_function_params Meta-parameters needed by problem function.
#' @return 
#' nmmso_state = Structure holding state of swarm.
#' mode_shift = Numeric flag that determines the number of evaluations done for future locations.
#' y = Result of evaluating the problem_function.
#' 
#' @export
evaluate <-
  function(nmmso_state, chg, problem_function) {
    y = feval(
      problem_function, nmmso_state$active_modes[[chg]]$swarm$new_location
    )
    mode_shift = 0
    
    if (y > nmmso_state$active_modes[[chg]]$swarm$mode_value) {
      nmmso_state$active_modes[[chg]]$swarm$mode_location = nmmso_state$active_modes[[chg]]$swarm$new_location
      nmmso_state$active_modes[[chg]]$swarm$mode_value = y
      mode_shift = 1
    }

    nmmso_state$active_modes[[chg]]$swarm$history_locations = add_row(nmmso_state$active_modes[[chg]]$swarm$history_locations, nmmso_state$active_modes[[chg]]$swarm$shifted_loc, nmmso_state$active_modes[[chg]]$swarm$new_location)
    nmmso_state$active_modes[[chg]]$swarm$history_values[nmmso_state$active_modes[[chg]]$swarm$shifted_loc] = y
    
    # if better than personal best for swarm member - then replace
    # # str(nmmso_state$active_modes[[chg]]$swarm$pbest_values)
    if (y > nmmso_state$active_modes[[chg]]$swarm$pbest_values[nmmso_state$active_modes[[chg]]$swarm$shifted_loc,]) {
      nmmso_state$active_modes[[chg]]$swarm$pbest_values[nmmso_state$active_modes[[chg]]$swarm$shifted_loc, ] = y
      nmmso_state$active_modes[[chg]]$swarm$pbest_locations <- add_row(nmmso_state$active_modes[[chg]]$swarm$pbest_locations, nmmso_state$active_modes[[chg]]$swarm$shifted_loc, nmmso_state$active_modes[[chg]]$swarm$new_location)
    }
    
    # change the x and y of the current active mode
    # check back with matlab
    nmmso_state$X[nmmso_state$index,] = nmmso_state$active_modes[[chg]]$swarm$new_location
    nmmso_state$Y[nmmso_state$index] = y
    nmmso_state$index = nmmso_state$index + 1
    
    # return the result
    list("nmmso_state" = nmmso_state, "mode_shift" = mode_shift, "y" = y)
  }
