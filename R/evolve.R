#' @title evolve
#'
#' @param nmmso_state Structure holding state of swarm.
#' @param problem_function String containing name of function to be optimised.
#' @param mn Minimum design parameter values (a vector with param_num elements).
#' @param mx Maximum design parameter values (a vector with param_num elements).
#' @param problem_function_params Meta-parameters needed by problem function.
#' @param max_evol Maximum number of swarms to update in a generation. If not provided this is set at 100.
#' @param swarm_size Maximum number of elements (particles) per swarm.
#' @return
#' nmmso_state = Structure holding state of swarm.
#'
#' @export
evolve <- function(nmmso_state, problem_function, mn, mx,  max_evol, swarm_size) {
    n = length(nmmso_state$active_modes)
    
    if (n > max_evol) {
      if (runif(1) < 0.5) {
        I = sort(nmmso_state$V_loc, decreasing = TRUE)
      } else {
        I = 1:n
      }
      I = I[i:max_evol]
      n = max_evol
    } else {
      I = 1:n
    }
    
    II = sample(n)
    # uniform crossover of two mode elements, either fittest two, or random two
    R = UNI(
      nmmso_state$active_modes[[I[II[1]]]]$swarm$mode_location, nmmso_state$active_modes[[I[II[2]]]]$swarm$mode_location
    )
    
    nmmso_state.M_loc = matrix(nmmso_state$M_loc, R)
    
    swarm$new_location = R
    evaluate_first = evaluate_first(swarm, problem_function,  nmmso_state, swarm_size, mn, mx)
    swarm = evaluate_first$swarm
    nmmso_state = evaluate_first$nmmso_state
    
    nmmso_state$V_loc = matrix(nmmso_state$V_loc, swarm$mode_value)
    nmmso_state$active_modes[[length(nmmso_state$active_modes) + 1]]$swarm = swarm
    
    # Mark these as new
    nmmso_state$active_modes_changed = matrix(nmmso_state$active_modes_changed, 1)
    nmmso_state$converged_modes = matrix(nmmso_state$converged_modes, 0)
    number_of_new_modes = 1
    
    # return values
    list("nmmso_state" = nmmso_state)
  }
    