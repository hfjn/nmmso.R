library(flexclust)
library(pracma)

source("./R/evaluate.R")
source("./R/evaluate_first.R")
source("./R/evaluate_mid.R")
source("./R/evaluate_new_locations.R")
source("./R/evolve.R")
source("./R/extract_modes.R")
source("./R/feval.R")
source("./R/get_initial_locations.R")
source("./R/hive.R")
source("./R/increment_swarm.R")
source("./R/merge_swarms.R")
source("./R/merge_swarms_together.R")
source("./R/random_new.R")
source("./R/uniform_sphere_points.R")
source("./R/UNI.R")

#' @title Implementation of the Niching Migratory Multi-Swarm Optimser.
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
#' @param tol_val Tolerance value for merging automatically (default 10^-6).
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
NMMSO_iterative <- function(swarm_size, problem_function,  max_evaluations, mn, mx, evaluations, nmmso_state, max_evol = 100, tol_val = (10 ^ -6)) {

  # test if all variables are correctly initialized
  if (evaluations < 0) {
    stop('A algorithm can only be run a positive number of times')
  }
  
  # test if max_evol is smaller than 0, which is not usable
  if (max_evol <= 0) {
    sprintf('Max_eval cannot be negative or zero, default max_eval used, set at 100')
    max_evol = 100
  }
  
  if (evaluations == 0) {
    # preallocate matrices for speed
    nmmso_state = list(X = matrix(0, max_evaluations + 500, length(mx)), Y = matrix(0, max_evaluations + 500, 1))
    nmmso_state$index = 1
    nmmso_state$converged_modes = 0

    ## TODO: This isn't exactly nice. But so far M_loc is never created
    nmmso_state$M_loc = matrix(0, max_evaluations + 500, length(mx))
    
    # initialize active modes as a list and give the sub "Modes" lists aswell
    nmmso_state$active_modes <- list(list(swarm = list()))
    # nmmso_state$active_modes[1] <- list()
    
    # nmmso_state$active_modes[1]$swarm <- list()
    
    # get initial locations
    nmmso_state = get_initial_locations(nmmso_state, mn, mx)
    
    # get first evaluation
    result = evaluate_first(nmmso_state$active_modes[[1]]$swarm, problem_function, nmmso_state, swarm_size, mn, mx)
    nmmso_state = result$nmmso_state
    swarm = result$swarm
    nmmso_state$active_modes[[1]]$swarm = swarm
    
    # track number of evaluations taken
    evaluations = 1
    
    # keep modes in matrices for efficiency on some computations
    nmmso_state$M_loc = nmmso_state$active_modes[1]$swarm$mode_location
    nmmso_state$V_loc = nmmso_state$active_modes[1]$swarm$mode_value
    nmmso_state$tol_val = tol_val
  }
  
  # only run when limited evaluations is not already done
  if(evaluations < max_evaluations){

    # first see if modes should be merged together
    number_of_mid_evals = 0
    while(sum(nmmso_state$active_modes_changed) > 0){
      result = merge_swarms(nmmso_state, problem_function, mn, mx)
      nmmso_state = result$nmmso_state
      merge_evals = result$merge_evals
      # track function evals used
      number_of_mid_evals = number_of_mid_evals + merge_evals 
    }
    
    # Now increment the swarms
    # if we have more than max_evol, then only increment a subset
    limit = min(max_evol, length(nmmso_state$active_modes))
    
    # have to select a subset
    if (limit > max_evol){
      # select fittest
      if (runif(1) < 0.5){
        result = sort(nmmso_state$V_loc, decreasing = TRUE, index.return = TRUE)
        indices = result$ix
      }
      
      # select at random
      else{
        indices = sample(length(nmmso_state$V_loc))
      }
      
    }else{
      # increment all
      indices = 1:limit
    }
    I2 = indices[1:limit]

    ######################
    # increment
    for(jj in 1:length(I2)){
      result = increment_swarm(nmmso_state, I2[jj], mn, mx, swarm_size)
    }
    
    nmmso_state = result$nmmso_state
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
    if(runif(1) < 0.5 || length(nmmso_state$active_modes) == 1 || length(mx) == 1){
      number_of_evol_modes = 0
      result = random_new(nmmso_state, problem_function, mn, mx, swarm_size)
      nmmso_state = result$nmmso_state
      number_rand_modes = result$number_rand_modes
    }else{
      number_rand_modes = 0
      result = evolve(nmmso_state, problem_function, mn, mx,  max_evol, swarm_size)
      nmmso_state = result$nmmso_state
      number_of_evol_modes = result$number_of_evol_modes
    }
    
    # update the total number of function evaluations used, with those required at each of the algorithm stages
    print(number_of_mid_evals)
    print(number_of_new_locations)
    print(number_of_evol_modes)
    print(number_rand_modes)
    print(number_of_hive_samples)
    evaluations = sum(evaluations, number_of_mid_evals, number_of_new_locations, number_of_evol_modes, number_rand_modes, number_of_hive_samples, na.rm = TRUE)
    print(evaluations)
    sprintf("Number of swarms %s, evals %s, max mode est. %s", length(nmmso_state$active_modes), evaluations, max(nmmso_state$V_loc))
    
    
  }else{
    sprintf("Evaluations taken already exhausted!")
  }
  
  result = extract_modes(nmmso_state)
  mode_loc = result$RES
  mode_y = result$RES_Y
  
  list("mode_loc" = mode_loc, "mode_y" = mode_y, "evaluations" = evaluations, "nmmso_state" = nmmso_state)
}


























