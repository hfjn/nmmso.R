#' @title 
#' @param nmmso_state
#' @param problem_function
#' @param mn
#' @param mx
#' @param problem_function_params
#' @param swarm_size
random_new <-
  function(nmmso_state, problem_function, mn, mx, swarm_size) {
    number_rand_modes = 1
    x = runif(size(mx)) * (mx - mn) + mn
    nmmso_state$active_modes_changed = rbind(nmmso_state$active_modes_changed, matrix(1, number_rand_modes, 1))
    nmmso_state$converged_modes = rbind(nmmso_state$converged_modes, matrix(0, number_rand_modes, 1))
    
    #create new swarm
    swarm <- list("new_location" = x)

    result = evaluate_first(swarm, problem_function, nmmso_state, swarm_size, mn, mx)

    swarm = result$swarm
    nmmso_state = result$nmmso_state
    
    nmmso_state$active_modes = rbind(nmmso_state$active_modes, list("swarm" = swarm))
    nmmso_state$M_loc = rbind(nmmso_state$M_loc, x)
    nmmso_state$V_loc = rbind(nmmso_state$V_loc, nmmso_state$active_modes[[end]]$swarm$mode_value)
    
    list("nmmso_state" = nmmso_state, "number_rand_modes" = number_rand_modes)
  }