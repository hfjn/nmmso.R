#' @title get_initial_locations
#' @description Calculates the initial locations of the algorithm.
#' 
#' @param nmmso_state Structure holding state of swarm.
#' @param mn Minimum design parameter values (a vector with param_num elements).
#' @param mx Maximum design parameter values (a vector with param_num elements).
#' @return nmmso_state = Structure with the new locations.
#' 
#' @export
get_initial_locations <- function(nmmso_state, mn, mx) {
  #point wise product as new locations  
  new_location = matrix(runif(size(mx)[1]*size(mx)[2]), size(mx)[1]) * (mx - mn) + mn
  # TODO there was an assignment think about
  # [1]
  # create first stage of swarm
  nmmso_state$active_modes = list(list("swarm" = list("new_location" = new_location)))
  nmmso_state$active_modes_changed[1] = 1
  
  return(nmmso_state)
}