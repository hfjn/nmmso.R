# maximum swarm_size = 10*D (D is number of design parameters)
# max_inc = 100
# c1, c2 = 2.0
# X = 1.0

#' @title NMMSO
#' @description Implementation of the Niching Migratory Multi-Swarm Optimser.
#'
#' @param swarm_size Maximum number of elements (particles) per swarm.
#' @param problem_function String containing name of function to be optimised.
#' @param max_evaluations Maximum number of evaluations to be taken through the problem function.
#' @param mn Minimum design parameter values (a vector with param_num elements).
#' @param mx Maximum design parameter values (a vector with param_num elements).
#' @param max_evoluations Maximum number of swarms to update in a generation. If not provided this is set at 100.
#' @param tol_val Tolerance value for merging automatically (default 10^-6).
#' @return
#' mode_loc_before = Design space location of current mode estimates (swarm gbests), note that at least one is likely to be very poor due to the 
#' new swarm spawning at the end of each generation, and that these will be a combination of both global and local mode estimate.
#' mode_y_before = Function evalutions corresponding to the mode estimates.
#' evaluations_before = Number of problem function evaluations until this point.
#' nmmso_state = Structure holding the state of the swarms. Unless you want to pick apart the details of how the algorithm searchs the space, 
#' then the only two elements you will probably be interested in are X and Y which are preallocated matrices to hold all locations visited 
#' (therefore nmmso_state.X(1:evaluations,:) will hold all the design space locations visited by the optimiser thus far.
#' mode_loc_after = Design space location of mode estimates at end.
#' mode_y_after = Function evalutions corresponding to the mode estimates.
#' evaluations_after = Number of problem function evaluationsat end.
#'
#' @export
NMMSO = function(swarm_size, problem_function, max_evaluations, mn, mx, max_evol = 100, tol_val = 10 ^ -6) {
  
  if(max_evol <= 0) {
    cat("Max_eval cannot be negative or zero, default max_eval used, set at 100 \n")
    max_evol = 100
  }
  
  # At start no evaluations used, and NMMSO state is empty
  mode_loc_after = list()
  mode_y_after = list()
  evaluations_after = list()
  nmmso_state = list()
  evaluations_after = 0
  
  while(evaluations_after < max_evaluations) {
    cat("Evaluation: ", evaluations_after, "\n")
    mode_loc_before = mode_loc_after
    mode_y_before = mode_y_after
    evaluations_before = evaluations_after
    

    nmmso_iterative = NMMSO_iterative(swarm_size, problem_function, max_evaluations, mn, mx, evaluations_after, nmmso_iterative$nmmso_state, max_evol, tol_val)
    mode_loc_after = nmmso_iterative$mode_loc
    mode_y_after = nmmso_iterative$mode_y
    #print(mode_loc_after)
    #print(mode_y_after)
    evaluations_after = nmmso_iterative$evaluations
    #print(object_size(nmmso_iterative$nmmso_state))
  }
  
  list("mode_loc_before" = mode_loc_before, "mode_y_before" = mode_y_before, "evaluations_before" = evaluations_before, 
       "nmmso_state" = nmmso_iterative$nmmso_state, 
       "mode_loc_after" = mode_loc_after, "mode_y_after" = mode_y_after, "evaluations_after" = evaluations_after)
}