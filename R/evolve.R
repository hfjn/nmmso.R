#' @title
#' @param nmmso_state
#' @param problem_function
#' @param mn
#' @param mx
#' @param problem_function_params
#' @param max_evol
#' @param swarm_size
evolve <-
  function(nmmso_state, problem_function, mn, mx,  max_evol, swarm_size) {
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
    list("nmmso_state" = nmmso_state,"number_of_new_modes" = number_of_new_modes)
  }
    