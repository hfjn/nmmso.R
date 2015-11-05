#' @title evaluate_mid
#' 
#' @param nmmso_state
#' @param chg
#' @param problem_function
#' @param test_function_params
#' @return 
#' nmmso_state =
#' mode_shift = 
#' y = 
#' 
#' @export
evaluate_mid <-
  function(nmmso_state, chg, problem_function) {
    # comment from original:
    # new_location is the only solution thus far in mode, so by definition is
    # also the mode estimate, and the only history thus far
    
    y = feval(
      problem_function, list(nmmso_state$active_modes[[chg]]$swarm$new_location)
    )
    mode_shift = 0
    
    if (y > nmmso_state$active_modes[[chg]]$swarm$mode_value) {
      nmmso_state$active_modes[[chg]]$swarm$mode_location = nmmso_state$active_modes[[chg]]$swarm$new_location
      nmmso_state$active_modes[[chg]]$swarm$mode_value = y
      mode_shift = mode_shift + 1
    }
    
    nmmso_state$X[nmmso_state$index,] = nmmso_state$active_modes[[chg]]$swarm$new_location
    nmmso_state$Y[nmmso_state$index] = y
    nmmso_state$index = nmmso_state$index + 1
    
    # return the values
    list("nmmso_state" = nmmso_state, "mode_shift" = mode_shift, "y" = y)
  }
