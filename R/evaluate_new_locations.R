#' @title evaluate_new_locations
#'
#' @param nmmso_state Structure holding state of swarm.
#' @param problem_function_params Meta-parameters needed by problem function.
#' @param I Vector of indixes for new locations.
#' @return
#' nmmso_state = Structure holding state of swarm.
#' number_of_new_location = The length of the I vector.
#'
#' @export
evaluate_new_locations <- function(nmmso_state, problem_function, I) {
    # at this point should be unflagged
    nmmso_state$swarms_changed = matrix(0, length(nmmso_state$swarms), 1)
    for (i in 1:length(I)) {
      evaluate = evaluate(nmmso_state, I[i], problem_function)
      nmmso_state = evaluate$nmmso_state
      mode_shift = evaluate$mode_shift
      if (mode_shift == 1) {
        nmmso_state$swarms_changed[I[i]] = 1
        nmmso_state$mode_locations <- add_row(nmmso_state$mode_locations, I[i], nmmso_state$swarms[[I[i]]]$new_location)
        nmmso_state$mode_values[I[i]] = nmmso_state$swarms[[I[i]]]$mode_value
        nmmso_state$swarms[[I[i]]]$less_fit_move = 0
      }
    }
    print("number_of_new_locations")
    
    # return the values
    list("nmmso_state" = nmmso_state, "number_of_new_locations" = length(I))
  }

