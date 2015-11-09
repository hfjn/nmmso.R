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
    nmmso_state$active_modes_changed = matrix(0, length(nmmso_state$active_modes), 1)
    for (i in 1:length(I)) {
      evaluate = evaluate(nmmso_state, I[i], problem_function)
      nmmso_state = evaluate$nmmso_state
      mode_shift = evaluate$mode_shift
      if (mode_shift == 1) {
        nmmso_state$active_modes_changed[I[i]] = 1
        nmmso_state$M_loc[I[i], ] = nmmso_state$active_modes[[I[i]]]$swarm$new_location
        nmmso_state$V_loc[I[i]] = nmmso_state$active_modes[[I[i]]]$swarm$mode_value
        nmmso_state$active_modes[[1]]$swarm$less_fit_move = 0
      }
    }
    
    # return the values
    list("nmmso_state" = nmmso_state, "number_of_new_location" = length(I))
  }

