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
  n = length(nmmso_state$swarms)

  if (n > max_evol) {
    if (runif(1) < 0.5) {
      I = sort(nmmso_state$mode_values, decreasing = TRUE, index.return = TRUE)
      I = I$ix
    } else {
      I = 1:n
    }
    I = I[1:max_evol]
    n = max_evol
  } else {
    I = 1:n
  }

  II = sample(n)
  # uniform crossover of two mode elements, either fittest two, or random two

  R = UNI(
    nmmso_state$swarms[[I[II[1]]]]$mode_location, nmmso_state$swarms[[I[II[2]]]]$mode_location
    )

  R = R$x_c
  nmmso_state$mode_locations = rbind(nmmso_state$mode_locations, as.numeric(R))
  swarm = list()
  swarm$new_location = R
  evaluate_first = evaluate_first(swarm, problem_function, nmmso_state, swarm_size, mn, mx)
  swarm = evaluate_first$swarm
  nmmso_state = evaluate_first$nmmso_state
  print("evolve")
  print(max(nmmso_state$mode_values))
  nmmso_state$mode_values = c(nmmso_state$mode_values, swarm$mode_value)
  print(max(nmmso_state$mode_values))
  nmmso_state$swarms[[length(nmmso_state$swarms) + 1]] = swarm

    # Mark these as new
  nmmso_state$swarms_changed = add_row(nmmso_state$swarms_changed , size(nmmso_state$swarms_changed)[1] + 1, 1)
  nmmso_state$converged_modes = add_row(nmmso_state$converged_modes, size(nmmso_state$converged_modes)[1] + 1, 0)
  number_of_new_modes = 1

    # return values
  list("nmmso_state" = nmmso_state, "number_of_new_modes" = number_of_new_modes)
}
