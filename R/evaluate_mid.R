#' @title evaluate_mid
#' 
#' @param nmmso_state Structure holding state of swarm.
#' @param chg current index
#' @param problem_function String containing name of function to be optimised.
#' @param test_function_params Meta-parameters needed by problem function.
#' @return 
#' nmmso_state = Structure holding state of swarm.
#' mode_shift = Numeric flag that determines the number of evaluations done for future locations.
#' y = Result of evaluating the problem_function.
#' 
#' @export
evaluate_mid <- function(nmmso_state, chg, problem_function) {
    # new_location is the only solution thus far in mode, so by definition is
    # also the mode estimate, and the only history thus far
    
    y = feval(problem_function, nmmso_state$swarms[[chg]]$new_location)
    mode_shift = 0
    
    if (y > nmmso_state$swarms[[chg]]$mode_value) {
      nmmso_state$swarms[[chg]]$mode_location = nmmso_state$swarms[[chg]]$new_location
      nmmso_state$swarms[[chg]]$mode_value = y
      mode_shift = mode_shift + 1
    }
    
    nmmso_state$X[nmmso_state$index,] = nmmso_state$swarms[[chg]]$new_location
    nmmso_state$Y[nmmso_state$index] = y
    nmmso_state$index = nmmso_state$index + 1
    
    # return the values
    list("nmmso_state" = nmmso_state, "mode_shift" = mode_shift, "y" = y)
  }
