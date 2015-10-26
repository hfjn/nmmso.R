library(flexclust)

NMMSO_iterative(
  swarm_size, problem_function, problem_function_params, max_evaluations, mn, mx, evaluations, nmmso_state, max_evol, tol_val
) {
  
  # test if all variables are correctly initialized
  # TODO: Test this
  if(evaluations < 0){
    stop('A algorithm can only be run a positive number of times')
  }
  
  # test if max_evol has a value otherwise assign one
  # TODO: Test this
  if(is.null(max_evol)){
    print('default max_evol used, set at 100')
    max_evol = 100
  }
  
  # test if max_evol is smaller than 0, which is not usable
  # TODO: Test this
  if(max_evol <= 0){
    print('Max_eval cannot be nagative or zero, default max_eval used, set at 100')
    max_evol = 100;
  }
  
  # test if tol_val is existing, otherwise create it
  if(is.null(tol_val)){
    tol_val = 10^-6
  }
  
  
  
  if(evaluations == 0){
    # preallocate matrices for speed
    nnmso_state$X <- matrix(0, max_evaluations + 500, length(mx))
    nnmso_state$Y <- matrix(0, max_evaluations + 500, 1)
    nmmso_state$index = 1
    nmmso_state$converged_modes = 0
    
    # get initial locations
    nnmso_state = get_initial_locations(nmmso_state, mn, mx)
    # the active modes function is never implemented in matlab. is it a list?
    nnmso_state$active_modes <- list()
    nnmso_state$active_modes[1] <- list()
    result <- evaluate_first(problem_function, problem_function_params, nnmso_state, swarm_size, mn, mx)
    nnmso_state$active_modes[1]$swarm = matrix(0, 1,1)
  }
  
  
  
}

# extracts the modes from the given nmmso_state
extract_modes <- function(nmmso_state) {
  RES = matrix(0, length(nnmso_state$active_modes, length(nnmso_state$active_modes[1]$swarm$mode_location)))
  RES_Y = matrix(0, length(nmmso_state$active_modes), 1)
  
  for(i in 1:length(nmmso_state$active_modes)){
    RES(i, ) = nnmso_state$active_modes(i)$swarm$mode_location
    RES_Y(i) = nnmso_state$active_modes(i)$swarm$mode_value
  }
  
  return(list(RES, RES_Y))
}

# calculates the initial locations of the algorithm
get_initial_locations(nmmso_state, mn, mx){
  #point wise product as new locations
  nnmso_state$active_modes[1]$swarm$new_location = rand(length(mx))*(mx-mn) + mn
  nmmso_state$active_modes_changed[1] = 1
}

evaluate_first(swarm, problem_function, problem_function_params, nmmso_state, swarm_size, mn, mx){
  
  # from original:
  ## new location is the only solution thus far in mode, so by definition is
  ## also the mode estimate, and the only history thus far
  y = feval(problem_func, swarm$new_location, problem_func_params)
  #gbest location
  swarm$mode_location = swarm$new_location
  
  #gbest value
  swarm.mode_value = y
  
  # intialize containers for swarm elements
  
  # current locations of swarm
  swarm$history_locations = matrix(0, length(swarm$mode_location))
  swarm$history_values = matrix(1, swarm_size, 1) * -Inf
  
  swarm$pbest_locations = matrix(0, swarm_size, length(swarm$mode_location))
  
}

merge_swarms <- function(nmmso_state, problem_function, problem_function_params, mn, mx){
  I = which(nmmso_state$active_modes_changed == 1)
  nmmso_state$active_modes_changed = nmmso_state$active_modes_changed * 0 # reset
  
  n = length(I)
  number_of_mid_evals = 0
  if(n >= 1 && (length(nmmso_state$active_modes) > 1)) {
    to_compare = matrix(0, 2)
    to_compare[, 1] = I
    for(i in 1:n) {
      d = dist2(nmmso_state$M_loc[I[i], ], nmmso_state$M_loc)
      d[I[i]] = Inf
      tmp = which(to_compare[i, 2] == min(to_compare[i, 2]))
      nmmso_state$active_modes[I[i]]$swarm$dist = sqrt(tmp)
      
      if(nmmso_state$active_modes[I[i]]$swarm$number_of_particles == 1) {
        reject = 0
        temp_vel = mn - 1
        while(sum(temp_vel < mn) > 0 || sum(temp_vel > mx) > 0) {
          temp_vel = uniform_sphere_points(1, length(nmmso_state$active_modes[I[i]]$swarm$new_location)) 
                   * (nmmso_state$active_modes[I[i]]$swarm$dist / 2)
          reject = reject + 1
          if(reject > 20) {
            size = dim(nmmso_state$active_modes[I[i]]$swarm$new_location)
            temp_vel = matrix(runif(size, min = (mx - mn), max = mn), size)
          }
        }
        nmmso_state$active_modes[I[i]]$swarm$velocities[1, ] = temp_vel
      }
    }
    
    to_compare = t(apply(to_compare, 1, sort))
    to_compare = apply(to_compare, 2, sort)
    
    # Remove duplicates
    for(i in seq(n, -1, 2)) {
      I = which(to_compare[, 1] == to_compare[i, 1])
      repeat_matrix = kronecker(matrix(1, length(I), 1, to_compare[i, ]))
      if (# TODO: check the repeat_matrix with Matlab) {
        to_compare[i, ] = matrix()
      }
    }
    
    # Check for merging
    n = length(to_compare)
    to_merge = matrix()
    number_of_mid_evals = 0
    
    for(i in 1:n) {
      if(sqrt(dist2(nmmso_state$active_modes(to_compare[i, 1])$swarm$mode_location, nmmso_state$active_modes[to_compare[i, 2]]$swarm$mode_location)) 
         < nmmso_state$tol_val) {
        to_merge = matrix(to_merge, i)
      } else {
        mid_loc = 0.5 
              * (nmmso_state$active_modes[to_compare[i, 1]]$swarm$mode_location - nmmso_state$active_modes[to_compare[i, 2]]$swarm$mode_location)
              + nmmso_state$active_modes[to_compare[i, 2]]$swarm$mode_location
        
        if(sum(mid_loc < mn) > 0 || sum(mid_loc > mx) > 0) {
          stop('Mid point out of range!')
        }
        
        nmmso_state$active_modes[to_compare[i, 2]]$swarm$new_location = mid_loc
        #evaluate_mid
        
        if(mode_shift == 1) {
          nmmso_state$M_loc[to_compare[i, 2], ] = nmmso_state$active_modes[to_compare[i, 2]]$swarm$mode_location
          nmmso_state$V_loc[to_compare[i, 2]] = nmmso_state$active_modes[to_compare[i, 2]]$swarm$mode_value
          to_merge = matrix(to_merge, i)
          nmmso_state$active_modes_changed[to_compare[i, 2]] = 1
        } else if(nmmso_state$active_modes[to_compare[i, 2]]$swarm$mode_value < y) {
          to_merge = matrix(to_merge, i) 
        }
        
        number_of_mid_evals = number_of_mid_evals + 1
      }
    }
    
    delete_index = matrix(0, dim(to_merge))
    for(i in 1:length(to_merge)) {
      if(to_compare[to_merge[i], 2] == to_compare[to_merge[i], 1]) {
        stop('Indices should not be equal')
      }
      if(nmmso_state$active_modes[to_compare[to_merge[i], 1]]$swarm$mode_value > nmmso_state$active_modes[to_compare[to_merge[i], 2]]$swarm$mode_value) {
        delete_index[i] = to_compare[to_merge[i], 2]
        nmmso_state$active_modes[to_compare[to_merge[i], 1]]$swarm = merge_swarms_together(nmmso_state$active_modes[to_compare[to_merge[i], 1]]$swarm, 
                                                                                           nmmso_state$active_modes[to_compare[to_merge[i], 2]]$swarm)
        nmmso_state$active_modes_changed[to_compare[i, 1]] = 1
      } else {
        delete_index[i] = to_compare[to_merge[i], 1]
        nmmso_state$active_modes[to_compare[to_merge[i], 2]]$swarm = merge_swarms_together(nmmso_state$active_modes[to_compare[to_merge[i], 2]]$swarm,
                                                                                           nmmso_state$active_modes[to_compare[to_merge[i], 1]]$swarm)
        nmmso_state$active_modes_changed[to_compare[i, 2]] = 1
      }
    }
    
    prev_merge = -1
    delete_index = apply(delete_index, 2, sort)
    for(i in seq(length(delete_index), -1, 1)) {
      if(delete_index[i] != prev_merge) {
        prev_merge = delete_index[i]
        nmmso_state$active_modes[delete_index[i]] = matrix()
        nmmso_state$M_loc[delete_index[i], ] = matrix()
        nmmso_state$V_loc[delete_index[i]] = matrix()
        nmmso_state$converged_modes[delete_index[i]] = matrix()
        nmmso_state$active_modes_changed[delete_index[i]] = matrix()
      }
    }
  } 
  
  if(length(nmmso_state$active_modes) == 1) {
    nmmso_state$active_modes[1]$swarm$dist = apply(mx - mn, 2, min)
  }
}

evaluate <- function(nmmso_state, chg, problem_function, problem_function_params){
  
}

merge_swarms_together <- function(swarm1, swarm2){
  
}

evaluate_mid <- function(nmmso_state, chg, problem_function, test_function_params){
  
}

increment_swarm <- function(nmmso_state, chg, mn, mx, swarm_size){
  
}

evaluate_new_locations <- function(nmmso_state, problem_function_params, I){
  
}

evolve <- function(nmmso_state, problem_function, mn, mx, problem_function_params, max_evol, swarm_size){
  
}

hive <- function(nmmso_state, problem_function, mn, mx, problem_function_params, max_evol, swarm_size){
  
}

random_new <- function(nmmso_satte, problem_function, mn, mx, problem_function_params, swarm_size){
  
}

UNI <- function(x1, x2){
  
}

uniform_sphere_points <- function(n,d){
  
}
# helper functions which imitates the behavior of the Matlab feval
feval <-function(f,...){f(...)}