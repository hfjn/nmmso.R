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
  #nmmso_state$active_modes[1]$swarm <- list()
  
  new_location = (matrix(runif(length(mx)^2 , length(mx))) * (mx - mn) + mn)[1]
  # create first stage of swarm
  nmmso_state$active_modes = list(list(swarm = list(new_location = new_location)))
  nmmso_state$active_modes_changed[1] = 1
  
  return(nmmso_state)
}