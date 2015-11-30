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
	
	RES = matrix(0, length(nmmso_state$swarms), length(nmmso_state$swarms[[1]]$mode_location))
	RES_Y = matrix(0, length(nmmso_state$swarms), 1)

	for (i in 1:length(nmmso_state$swarms)) {
		RES <- add_row(RES, i, nmmso_state$swarms[[i]]$mode_location)
		RES_Y[i] = nmmso_state$swarms[[i]]$mode_value
	}

	nmmso_state$mode_values = RES_Y
	nmmso_state$mode_locations = RES
	list("RES" = RES, "RES_Y" = RES_Y)
}
