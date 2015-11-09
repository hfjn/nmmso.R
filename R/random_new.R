#' @title random_new
#'
#' @param nmmso_state Structure holding state of swarm.
#' @param problem_function String containing name of function to be optimised.
#' @param mn Minimum design parameter values (a vector with param_num elements).
#' @param mx Maximum design parameter values (a vector with param_num elements).
#' @param problem_function_params Meta-parameters needed by problem function.
#' @param swarm_size Maximum number of elements (particles) per swarm.
#' @return
#' nmmso_state = Structure holding state of swarm.
#' number_rand_modes = Number of random modes (Apparently always 1).
#'
#' @export
random_new <- function(nmmso_state, problem_function, mn, mx, swarm_size) {
    number_rand_modes = 1

    x = matrix(runif(size(mx)[1]*size(mx)[2]), size(mx)[1]) * (mx - mn) + mn

    nmmso_state$active_modes_changed = rbind(nmmso_state$active_modes_changed, matrix(1, number_rand_modes, 1))
    nmmso_state$converged_modes = rbind(nmmso_state$converged_modes, matrix(0, number_rand_modes, 1))
    
    #create new swarm
    swarm <- list("new_location" = x[1,])

    result = evaluate_first(swarm, problem_function, nmmso_state, swarm_size, mn, mx)

    swarm = result$swarm
    nmmso_state = result$nmmso_state
    
    nmmso_state$active_modes = rbind(nmmso_state$active_modes, list(list("swarm" = swarm)))

    nmmso_state$mode_locations[nmmso_state$mode_locations_index,] = x
    nmmso_state$mode_locations_index = nmmso_state$mode_locations_index + 1

    nmmso_state$mode_values[nmmso_state$mode_locations_index] = nmmso_state$active_modes[[length(nmmso_state$active_modes)]]$swarm$mode_value
    nmmso_state$mode_values_index = nmmso_state$mode_values_index + 1

    list("nmmso_state" = nmmso_state, "number_rand_modes" = number_rand_modes)
  }