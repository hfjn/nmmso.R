library(flexclust)
library(pracma)

#' @title NMMSO_iterative
#' @description Implementation of the Niching Migratory Multi-Swarm Optimser.
#'
#' @param swarm_size Maximum number of elements (particles) per swarm.
#' @param problem_function String containing name of function to be optimised.
#' @param max_evaluations Maximum number of evaluations to be taken through the problem function.
#' @param mn Minimum design parameter values (a vector with param_num elements).
#' @param mx Maximum design parameter values (a vector with param_num elements).
#' @param evaluations Number of evaluations expended already. If set at 0, this will initialise the swarm_state structure and run the 
#' algorithm for a single generation, otherwise it will run it for a single generation from the evaluations number inputted.
#' @param nmmso_state Structure holding state of swarm. Can be empty or omitted if evaluations set at zero, must be provided if evals > 0.
#' @param max_evol Maximum number of swarms to update in a generation. If not provided this is set at 100.
#' @param tolerance_value Tolerance value for merging automatically (default 10^-6).
#' @return
#' mode_loc = Design space location of current mode estimates (swarm gbests), note that at least one is likely to be very poor due to the 
#' new swarm spawning at the end of each generation, and that these will be a combination of both global and local mode estimate.
#' mode_y = Function evalutions corresponding to the mode estimates.
#' evaluations = Number of problem function evaluations until this point.
#' nmmso_state = Structure holding the state of the swarms. Unless you want to pick apart the details of how the algorithm searchs the space, 
#' then the only two elements you will probably be interested in are X and Y which are preallocated matrices to hold all locations visited 
#' (therefore nmmso_state.X(1:evaluations,:) will hold all the design space locations visited by the optimiser thus far.
#'
#' @export
NMMSO_iterative <- function(swarm_size, problem_function, max_evaluations, mn, mx, evaluations, nmmso_state, max_evol = 100, tolerance_value = (10 ^ -6)) {
  
  # test if all variables are correctly initialized
  if (evaluations < 0) {
    stop('A algorithm can only be run a positive number of times')
  }
  
  # test if max_evol is smaller than 0, which is not usable
  if (max_evol <= 0) {
    cat("Max_eval cannot be negative or zero, default max_eval used, set at 100 \n")
    max_evol = 100
  }
  
  if (evaluations == 0) {
    upperBound = max_evaluations + 2000
    # preallocate matrices for speed
    nmmso_state <- list()
    nmmso_state$X = matrix(0, upperBound, length(mx))
    nmmso_state$Y = matrix(0, upperBound, 1)
    nmmso_state$index = 1
    nmmso_state$converged_modes = 0
    #nmmso_state$mode_locations = 0
    nmmso_state$mode_values = 0
    
    # get initial locations
    nmmso_state = get_initial_locations(nmmso_state, mn, mx)

    # get first evaluation
    result = evaluate_first(nmmso_state$swarms[[1]], problem_function, nmmso_state, swarm_size, mn, mx)
    nmmso_state = result$nmmso_state
    swarm = result$swarm
    nmmso_state$swarms = list(swarm)
    
    # track number of evaluations taken
    evaluations = 1
    
    # keep modes in matrices for efficiency on some computations
    nmmso_state$mode_locations = nmmso_state$swarms[[1]]$mode_location
    nmmso_state$mode_values = nmmso_state$swarms[[1]]$mode_value
    nmmso_state$tolerance_value = tolerance_value
  }
  
  # only run when limited evaluations is not already done
  if(evaluations < max_evaluations){
    # first see if modes should be merged together
    number_of_mid_evals = 0
    while(sum(nmmso_state$swarms_changed) > 0){
      result = merge_swarms(nmmso_state, problem_function, mn, mx)
      nmmso_state = result$nmmso_state
      merge_evals = result$number_of_merge_evals
      # track function evals used
      number_of_mid_evals = number_of_mid_evals + merge_evals 
    }
    # Now increment the swarms
    # if we have more than max_evol, then only increment a subset
    limit = min(max_evol, length(nmmso_state$swarms))
    # have to select a subset
    if (limit > max_evol){
      # select fittest
      if (runif(1) < 0.5){
        result = sort(nmmso_state$mode_values, decreasing = TRUE, index.return = TRUE)
        indices = result$ix
      }      
      # select at random
      else{
        indices = sample(length(nmmso_state$mode_values))
      }      
    }else{
      # can increment all
      indices = 1:limit
    }
    I2 = indices[1:limit]
    # increment  
    for(jj in 1:length(I2)){
      result = increment_swarm(nmmso_state, I2[jj], mn, mx, swarm_size)
      nmmso_state = result$nmmso_state
    }

    cs = result$cs
    # evaluate new member / new locations of swarm member
    result = evaluate_new_locations(nmmso_state, problem_function,  I2)
    nmmso_state = result$nmmso_state
    number_of_new_locations = result$number_of_new_locations
    
    # attempt to split off a member of one of the swarms to seed a new swarm (if detected to be on another peak)
    result = hive(nmmso_state, problem_function, mn, mx,  max_evol, swarm_size)
    nmmso_state = result$nmmso_state
    number_of_hive_samples = result$number_of_new_samples
    # create speculative new swarm, either at random in design space, or via crossover
    if(runif(1) < 0.5 || length(nmmso_state$swarms) == 1 || length(mx) == 1){
      number_of_evol_modes = 0
      result = random_new(nmmso_state, problem_function, mn, mx, swarm_size)
      nmmso_state = result$nmmso_state
      number_rand_modes = result$number_rand_modes
    }else{
      number_rand_modes = 0
      result = evolve(nmmso_state, problem_function, mn, mx,  max_evol, swarm_size)
      nmmso_state = result$nmmso_state
      number_of_evol_modes = result$number_of_new_modes
    }
    
    # update the total number of function evaluations used, with those required at each of the algorithm stages
    evaluations = sum(evaluations, number_of_mid_evals, number_of_new_locations, number_of_evol_modes, number_rand_modes, number_of_hive_samples, na.rm = TRUE)
    cat("Number of swarms", length(nmmso_state$swarms)," evals ", evaluations, " max mode est. ", max(nmmso_state$mode_values))

    # just to be sure

    cat(" index ", nmmso_state$index, "\n")

  }else{
    cat("Evaluations taken already exhausted! \n")
  }
  
  result = extract_modes(nmmso_state)
  mode_loc = result$RES
  mode_y = result$RES_Y
  list("mode_loc" = mode_loc, "mode_y" = mode_y, "evaluations" = evaluations, "nmmso_state" = nmmso_state)
}