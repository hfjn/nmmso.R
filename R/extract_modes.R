#' @title extract_modes
#' @description Get the mode estimate locations.
#' 
#' @param nmmso_state Structure holding state of swarm.
#' @return 
#' RES = Modes from the given nmmso_state for the X-axis.
#' RES_Y = Modes from the given nmmso_state for the Y-axis.
#' 
#' @export
extract_modes <- function(nmmso_state) {
  RES = matrix(0, length(nmmso_state$active_modes), length(nmmso_state$active_modes[[1]]$swarm$mode_location))
  RES_Y = matrix(0, length(nmmso_state$active_modes), 1)
  for (i in 1:length(nmmso_state$active_modes)) {
  	str(nmmso_state$active_modes[[i]])
    RES[i] = nmmso_state$active_modes[[i]]$swarm$mode_location
    RES_Y[i] = nmmso_state$active_modes[[i]]$swarm$mode_value
  }
  
  list("RES" = RES, "RES_Y" = RES_Y)
}
