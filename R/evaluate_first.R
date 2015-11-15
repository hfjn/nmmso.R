#' @title evaluate_first
#' @description Calculates the first evaluation of the swarms.
#' 
#' @param swarm Swarm object
#' @param problem_function String containing name of function to be optimised.
#' @param problem_function_params Meta-parameters needed by problem function.
#' @param nmmso_state Structure holding state of swarm.
#' @param swarm_size Maximum number of elements (particles) per swarm.
#' @param mn Minimum design parameter values (a vector with param_num elements).
#' @param mx Maximum design parameter values (a vector with param_num elements).
#' @return 
#' swarm = Swarm with new locations and values.
#' nmmso_state = Structure with the new locations after evaluation.
#' 
#' @export
evaluate_first <- function(swarm, problem_function, nmmso_state, swarm_size, mn, mx) {
  # from original:
  ## new location is the only solution thus far in mode, so by definition is
  ## also the mode estimate, and the only history thus far
  y = feval(problem_function, as.numeric(swarm$new_location))

  #gbest location
  swarm$mode_location = swarm$new_location
  #gbest value
  swarm$mode_value = y
  # intialize containers for swarm elements
  
  ## current locations of swarm
  swarm$history_locations = matrix(0, swarm_size, length(swarm$mode_location))
  swarm$history_values = matrix(1, swarm_size, 1) * -Inf
  
  ## pbest locations
  swarm$pbest_locations = matrix(0, swarm_size, length(swarm$mode_location))
  swarm$pbest_values = matrix(1, swarm_size, 1) * -Inf
  
  ## velocities
  swarm$velocities = matrix(runif(size(mx)[1]*size(mx)[2]), size(mx)[1]) * (mx - mn) + mn
  swarm$number_of_particles = 1

  ## History Locations
  swarm$history_locations[1,] = swarm$mode_location
  swarm$history_values[1,] = y
  
  ## pbest Locations
  swarm$pbest_locations[1,] = swarm$mode_location
  swarm$pbest_values[1,] = y
  
  ## TODO: Different to original. So we preallocate this.
  ## dist
  swarm$dist = min(mx - mn)

  # track all the changes
  nmmso_state$X[nmmso_state$index,] = swarm$new_location
  nmmso_state$Y[nmmso_state$index] = y
  nmmso_state$index = nmmso_state$index + 1
  # return the result
  list("nmmso_state" = nmmso_state, "swarm" = swarm)
}