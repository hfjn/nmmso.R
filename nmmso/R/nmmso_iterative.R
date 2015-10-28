library(flexclust)
library(pracma)

#' @title Holds the general program
#'
#' @param swarm_size
#' @param problem_function
#' @param problem_function_params
#' @param max_evaluations
#' @param mn
#' @param mx
#' @param evaluations
#' @param nmmso_state
#' @param max_evol
#' @param tol_val
#' @return
#'
NMMSO_iterative <- function(swarm_size, problem_function, problem_function_params, max_evaluations, mn, mx, evaluations, nmmso_state, max_evol, tol_val) {
  # test if all variables are correctly initialized
  # TODO: Test this
  if (evaluations < 0) {
    stop('A algorithm can only be run a positive number of times')
  }
  
  # test if max_evol has a value otherwise assign one
  # TODO: Test this
  if (is.null(max_evol)) {
    print('default max_evol used, set at 100')
    max_evol = 100
  }
  
  # test if max_evol is smaller than 0, which is not usable
  # TODO: Test this
  if (max_evol <= 0) {
    print('Max_eval cannot be nagative or zero, default max_eval used, set at 100')
    max_evol = 100;
  }
  
  # test if tol_val is existing, otherwise create it
  if (is.null(tol_val)) {
    tol_val = 10 ^ -6
  }
  
  
  
  if (evaluations == 0) {
    # preallocate matrices for speed
    nnmso_state$X <- matrix(0, max_evaluations + 500, length(mx))
    nnmso_state$Y <- matrix(0, max_evaluations + 500, 1)
    nmmso_state$index = 1
    nmmso_state$converged_modes = 0
    
    # get initial locations
    nnmso_state = get_initial_locations(nmmso_state, mn, mx)
    
    # initialize active modes as a list and give the sub "Modes" lists aswell
    nnmso_state$active_modes <- list()
    nnmso_state$active_modes[1] <- list()
    
    # get first evaluation
    result <-
      evaluate_first(problem_function, problem_function_params, nnmso_state, swarm_size, mn, mx)
    swarm = result$swarm
    nmmso_state = result$nmmso_state
    nmmso_state$active_modes[1]$swarm = swarm
    
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
      result = merge_swarms(nmmso_state, problem_function, problem_function_params, mn, mx)
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
    I2 = indice[1:limit]
    
    # increment
    for(jj in 1:length(I2)){
      nmmso_state = increment_swarm(nmmso_state, I2[jj], mn, mx, swarm, swarm_size)
    }
    
    # evaluate new member / new locations of swarm member
    result = evaluate_new_locations(nmmso, problem_function, problem_function_params, I2)
    nmmso_state = result$nmmso_state
    number_of_new_locations = result$number_of_new_locations
    
    # attempt to split off a member of one of the swarms to seed a new swarm (if detected to be on another peak)
    result = hive(nmmso_state, problem_function, mn, mx, problem_function_params, max_evol, swarm_size)
    nmmso_state = result$nmmso_state
    number_of_hive_samples = result$number_of_hive_sample
    
    # create speculative new swarm, either at random in design space, or via crossover
    if(runif(1) < 0.5 || length(nmmso_state$active_modes) == 1 || length(mx) == 1){
      number_of_evol_modes = 0
      result = random_new(nmmso,state,problem_function, mn, mx, problem_function_params, swarm_size)
      nmmso_state = result$nmmso_state
      number_rand_modes = result$number_rand_modes
    }else{
      number_rand_modes = 0
      result = evolve(nmmso_state, problem_function, mn, mx, problem_function_params, max_evol, swarm_size)
      nmmso_state = result$nmmso_state
      number_of_evol_modes = result$number_of_evol_modes
    }
    
    # update the total number of function evaluations used, with those required at each of the algorithm stages
    evaluations = evalutions + number_of_mid_evals + number_of_new_locations + number_of_evol_modes + number_rand_modes + number_of_hive_samples
    
    sprintf("Number of swarms %s, evals %s, max mode est. %s", length(nmmso_state$active_modes), evaluations, max(nmmso_state$V_loc))
    
    
  }else{
    sprintf("Evaluations taken already exhausted!")
  }
  
  result = extract_modes(nmmso_state)
  mode_loc = result$mode_loc
  mode_y = result$mode_y
  
  list("mode_loc" = mode_loc, "mode_y" = mode_y, "evaluations" = evaluations, "nmmso_state" = nmmso_state)
}

# extracts the modes from the given nmmso_state
extract_modes <- function(nmmso_state) {
  RES = matrix(0, length(
    nnmso_state$active_modes, length(nnmso_state$active_modes[1]$swarm$mode_location)
  ))
  RES_Y = matrix(0, length(nmmso_state$active_modes), 1)
  
  for (i in 1:length(nmmso_state$active_modes)) {
    RES(i,) = nnmso_state$active_modes(i)$swarm$mode_location
    RES_Y(i) = nnmso_state$active_modes(i)$swarm$mode_value
  }
  
  list("RES" = RES, "RES_Y" = RES_Y)
}

# calculates the initial locations of the algorithm
get_initial_locations <- function(nmmso_state, mn, mx) {
  #point wise product as new locations
  nnmso_state$active_modes[1]$swarm$new_location = rand(length(mx)) * (mx - mn) + mn
  nmmso_state$active_modes_changed[1] = 1
  
  return(nmmso_state)
}

evaluate_first <- function(swarm, problem_function, problem_function_params, nmmso_state, swarm_size, mn, mx) {
  # from original:
  ## new location is the only solution thus far in mode, so by definition is
  ## also the mode estimate, and the only history thus far
  y = feval(problem_func, swarm$new_location, problem_func_params)
  #gbest location
  swarm$mode_location = swarm$new_location
  
  #gbest value
  swarm.mode_value = y
  
  # intialize containers for swarm elements
  
  ## current locations of swarm
  swarm$history_locations = matrix(0, length(swarm$mode_location))
  swarm$history_values = matrix(1, swarm_size, 1) * -Inf
  
  ## pbest locations
  swarm$pbest_locations = matrix(0, swarm_size, length(swarm$mode_location))
  swarm$pbest_values = matrix(1, swarm_size, 1) * -Inf
  
  ## velocities
  swarm$velocities = rand(size(mx)) * (mx - mn) + mn
  swarm$number_of_particles = 1
  
  ## History Locations
  swarm$history_locations[1,] = swarm$mode_location
  swarm$history_valus[1] = y
  
  ## pbest Locations
  swarm$pbest_locations[1,] = swarm.mode_location
  swarm$pbest_values[1] = y
  
  
  # track all the changes
  nmmso_state$X[nmmso_state$index,] = swarm$new_location
  nmmso_state$Y[nmmso_state$index,] = y
  nmmso_state$index = nmmso_state$index + 1
  
  # return the result
  list("nmmso_state" = nmmso_state, "swarm" = swarm)
}

merge_swarms <-
  function(nmmso_state, problem_function, problem_function_params, mn, mx) {
    I = which(nmmso_state$active_modes_changed == 1)
    nmmso_state$active_modes_changed = nmmso_state$active_modes_changed * 0 # reset
    
    n = length(I)
    number_of_mid_evals = 0
    if (n >= 1 && (length(nmmso_state$active_modes) > 1)) {
      to_compare = matrix(0, 2)
      to_compare[, 1] = I
      for (i in 1:n) {
        d = dist2(nmmso_state$M_loc[I[i],], nmmso_state$M_loc)
        d[I[i]] = Inf
        tmp = which(to_compare[i, 2] == min(to_compare[i, 2]))
        nmmso_state$active_modes[I[i]]$swarm$dist = sqrt(tmp)
        
        if (nmmso_state$active_modes[I[i]]$swarm$number_of_particles == 1) {
          reject = 0
          temp_vel = mn - 1
          while (sum(temp_vel < mn) > 0 || sum(temp_vel > mx) > 0) {
            temp_vel = uniform_sphere_points(1, length(nmmso_state$active_modes[I[i]]$swarm$new_location))*(nmmso_state$active_modes[I[i]]$swarm$dist / 2)
            reject = reject + 1
            if (reject > 20) {
              size = dim(nmmso_state$active_modes[I[i]]$swarm$new_location)
              temp_vel = matrix(runif(size, min = (mx - mn), max = mn), size)
            }
          }
          nmmso_state$active_modes[I[i]]$swarm$velocities[1,] = temp_vel
        }
      }
      
      to_compare = t(apply(to_compare, 1, sort))
      to_compare = apply(to_compare, 2, sort)
      
      # Remove duplicates
      for (i in seq(n,-1, 2)) {
        I = which(to_compare[, 1] == to_compare[i, 1])
        repeat_matrix = kronecker(matrix(1, length(I), 1, to_compare[i,]))
        if (# TODO: check the repeat_matrix with Matlab
          ) {
          to_compare[i,] = matrix()
      }
    }
    
    # Check for merging
    n = length(to_compare)
    to_merge = matrix()
    number_of_mid_evals = 0
    
    for (i in 1:n) {
      if (sqrt(
        dist2(
          nmmso_state$active_modes(to_compare[i, 1])$swarm$mode_location, nmmso_state$active_modes[to_compare[i, 2]]$swarm$mode_location
        )
      ) < nmmso_state$tol_val) {
        to_merge = matrix(to_merge, i)
      } else {
        mid_loc = 0.5
        * (
          nmmso_state$active_modes[to_compare[i, 1]]$swarm$mode_location - nmmso_state$active_modes[to_compare[i, 2]]$swarm$mode_location
        )
        + nmmso_state$active_modes[to_compare[i, 2]]$swarm$mode_location
        
        if (sum(mid_loc < mn) > 0 || sum(mid_loc > mx) > 0) {
          stop('Mid point out of range!')
        }
        
        nmmso_state$active_modes[to_compare[i, 2]]$swarm$new_location = mid_loc
        nmmso_state = evaluate_mid(nmmso_state, to_compare[i, 2], problem_function, problem_function_params)[1]
        mode_shift = evaluate_mid(nmmso_state, to_compare[i, 2], problem_function, problem_function_params)[2]
        y = evaluate_mid(nmmso_state, to_compare[i, 2], problem_function, problem_function_params)[3]
        
        if (mode_shift == 1) {
          nmmso_state$M_loc[to_compare[i, 2],] = nmmso_state$active_modes[to_compare[i, 2]]$swarm$mode_location
          nmmso_state$V_loc[to_compare[i, 2]] = nmmso_state$active_modes[to_compare[i, 2]]$swarm$mode_value
          to_merge = matrix(to_merge, i)
          nmmso_state$active_modes_changed[to_compare[i, 2]] = 1
        } else if (nmmso_state$active_modes[to_compare[i, 2]]$swarm$mode_value < y) {
          to_merge = matrix(to_merge, i)
        }
        
        number_of_mid_evals = number_of_mid_evals + 1
      }
    }
    
    delete_index = matrix(0, dim(to_merge))
    for (i in 1:length(to_merge)) {
      if (to_compare[to_merge[i], 2] == to_compare[to_merge[i], 1]) {
        stop('Indices should not be equal')
      }
      if (nmmso_state$active_modes[to_compare[to_merge[i], 1]]$swarm$mode_value > nmmso_state$active_modes[to_compare[to_merge[i], 2]]$swarm$mode_value) {
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
    for (i in seq(length(delete_index),-1, 1)) {
      if (delete_index[i] != prev_merge) {
        prev_merge = delete_index[i]
        nmmso_state$active_modes[delete_index[i]] = matrix()
        nmmso_state$M_loc[delete_index[i],] = matrix()
        nmmso_state$V_loc[delete_index[i]] = matrix()
        nmmso_state$converged_modes[delete_index[i]] = matrix()
        nmmso_state$active_modes_changed[delete_index[i]] = matrix()
      }
    }
    if (length(nmmso_state$active_modes) == 1) {
      nmmso_state$active_modes[1]$swarm$dist = apply(mx - mn, 2, min)
    }
    # return the values
    list("nmmso_state" = nmmso_state, "number_of_mid_evals" = number_of_mid_evals)
  }



evaluate <-
  function(nmmso_state, chg, problem_function, problem_function_params) {
    y = feval(
      problem_func, nmmso_state$active_modes(chg$swarm$new_location, problem_function_params)
    )
    mode_shift = 0
    
    if (y > nmmso_state$active_modes[chg]$swarm$mode_value) {
      nmmso_state$active_modes[chg]$swarm$mode_location = nmmso_state$active_modes[chg]$swarm$new_location
      nmmso_state$active_modes[chg]$swarm$mode_value = y
      mode_shift = 1
    }
    
    nmmso_state$active_modes[chg]$swarm$history_location[nmmso_state$active_modes[chg]$swarm$shifted_loc,] = nmmso_state$active_modes[chg]$swarm$new_location
    nnmso_state$active_modes[chg]$swarm$history_vaues[nmmso_state$active_modes[chg]$swarm$shifted_loc] = y
    
    # if better than personal best for swarm member - then replace
    if (y > nmmso_state$active_modes[chg]$swarm$pbest_values[nmmso_state$active_modes[chg]$swarm$shifted_loc]) {
      nmmso_state$active_modes[chg]$swarm$pbest_values[nmmso_state$active_modes[chg]$swarm$shifted_loc] = y
      nmmso_state$active_modes[chg]$swarm$pbest_location[nmmso_state$active_modes[chg]$swarm$shifted_loc,] = nmmso_state$active_modes[chg].swarm.new_location
    }
    
    # change the x and y of the curren active mode
    nmmso_state$active_modes[chg]$X[nmmso_state$index,] = nmmso_state$active_modes[chg]$swarm$new_location
    nmmso_state$active_modes[chg]$Y[nmmso_state$index] = y
    nmmso_state$index = nmmso_state$index + 1
    
    # return the result
    list("nmmso_state" = nmmso_state, "mode_shift" = mode_shift, "y" = y)
  }

evaluate_mid <-
  function(nmmso_state, chg, problem_function, test_function_params) {
    # comment from original:
    # new_location is the only solution thus far in mode, so by definition is
    # also the mode estimate, and the only history thus far
    
    y = feval(
      problem_function, nmmso_state$active_modes[chg]$swarm$new_location, test_function_params
    )
    mode_shift = 0
    
    if (y > nmmso_state$active_modes[chg]$swarm$mode_value) {
      nmmso_state$active_modes[chg]$swarm$mode_location = nmmso_state$active_modes[chg]$swarm$new_location
      nmmso_state$active_modes[chg]$swarm$mode_value = y
      mode_shift = mode_shift + 1
    }
    
    nmmso_state$X[nmmso_state$index,] = nmmso_state$active_modes[chg]$swarm$new_location
    nmmso_state$Y[nmmso_state$index] = y
    nmmso_state$index = nmmso_state$index + 1
    
    # return the values
    list("nmmso_state" = nmmso_state, "mode_shift" = mode_shift, "y" = y)
  }

merge_swarms_together <- function(swarm1, swarm2) {
  # merge swarm 1 and 2, while keeping best elements of both
  
  n1 = swarm1$number_of_particles
  n2 = swarm2$number_of_particles
  max_size = size(swarm1$history_location, 1)
  
  if (n1 + n2 <= max_size) {
    swarm1$number_of_particles = n1 + n2
    
    # simples situation, where the combined active members of both
    # populations are below the total size they can grow to
    
    swarm1$history_location[(n1 + 1):(n1 + n2),] = swarm2$history_location[1:n2,] # current location of swarm
    swarm1$history_values[(n1 + 1):(n1 + n2)] = swarm2$history_values[1:n2] # current values of swarm
    
    swarm1$pbest_locations[(n1 + 1):(n1 + n2),] = swarm2$pbest_locations[1:2] # current best locations of swarm
    swarm1$pbest_values[(n1 + 1):n1 + n2] = swarm2$pbest_values[1:n2] # current best locations of swarm
    
    swarm1$velocities[(n1 + 1):(n1 + n2),] = swarm2$velocities[1:n2,] # current velocities of swarm
  }else{
    # select best out of combined population, based on current location
    
    swarm1$number_of_particles = max_size
    temp_h_loc = rbind(swarm$history_lcoation[1:n1,], swarm2$history_locations[1:n2,])
    temp_h_v = rbind(swarm1$history_values[1:n1,], swarm2$history_values[1:n2])
    
    temp_p_loc = rbind(swarm$pbest_location[1:n1,], swarm2$pbest_locations[1:n2,])
    temp_p_v = rbind(swarm$pbest_values[1:n1], swarm$pbest_values[1:n2])
    temp_vel = rbind(swarm1$velocities[1:n1,], swarm2$velocities[1:n2,])
    
    result <- sort(temp_h_v, decreasing = TRUE, index.return = TRUE)
    swarm1$history_locations = temp_h_loc(result$ix(1:max_size),)
    swarm1$history_values = temp_h_v(result$ix(1:max_size),)
    swarm1$pbest_location = temp_p_loc(results$ix(1:max_size),)
    swarm1$pbest_values = temp_p_v(results$ix(1:max_size),)
    swarm$velocities = temp_vel(results$ix(1:max_size),)
    
  }
  return(swarm1)
}


increment_swarm <- function(nmmso_state, chg, mn, mx, swarm_size) {
  cs = 0
  new_location = mn - 1
  d = nmmso_state$active_modes[chg]$swarm$dist
  
  shifted = 0
  omega = 0.1
  reject = 0
  
  # create a random particle
  r = sample(swarm_size)
  
  while (sum(new_location < mn) > 0 ||
         sum (new_location > mx) > 0) {
    # if swarm not at maximum capacity add a new particle
    if (nmmso_state$active_modes[chg]$swarm$number_of_particles < swarm_size) {
      new_location = nmmso_state$active_modes[chg]$swarm$mode_location + uniform_sphere_points(1, length(new_location)) * (d /
                                                                                                                             2)
    }else{
      #otherwise move and existing particle
      shifted = 1
      nmmso_state$active_modes(chg)$swarm$shifted_loc = r(1)
      
      temp_velocity = omega * nmmso_state$active_modes[chg]$swarm$velocities[nmmso_state$active_modes[chg]$swarm$shifted_loc,] + 2.0
      * matrix(
        runif(size(new_location ^ 2),size(new_location)) * (
          nmmso_state$active_modes[chg]$swarm$mode_location - nnmso_state$active_modes[chg]$swarm$history_locations[nnmso_state$active_modes[chg]$swarm$shifted_loc,]
          + 2.0 * matrix(size(new_location ^ 2), size(new_location)) * (
            nnmso_state$active_modes[chg]$swarm$pbest_location[nnmso_state$active_modes[chg]$swarm$shifted_loc,] - nnmso_state$active_modes(chg)$swarm$history_locations$[nnmso_state$active_modes(chg)$chg$swarm$shifted_loc,]
          )
          
          if (reject > 20) {
            I_max = which(((
              nnmso_state$active_modes[chg]$swarm$history_locations[nmmso_state$active_modes[chg]$swarm$shifted_loc,] + temp_velocity
            ) > mx
            ) == 1)
            I_min = which(((
              nnmso_state$active_modes[chg]$swarm$history_locations[nmmso_state$active_modes[chg]$swarm$shifted_loc,] + temp_velocity
            ) < mn
            ) == 1)
            
            if (length(I_max) >= 0) {
              temp_velocity(I_max) = runif(1, length(I_max)) * (mx[I_max] - nmmso_state$active_modes[chg]$swarm$history_locations[nnmso_state$active_modes[chg]$swarm$shifted_loc, I_max])
            }
            if (length(I_min) >= 0) {
              temp_velocity(I_min) = runif(1, length(I_min)) * ((
                nnmso_state$active_modes[chg]$swarm$history_locations[nmmso_state$active_modes[chg]$swarm$shifted_loc,I_min] -
                  mn(I_min)
              ) * -1)
            }
            new_location = nmmso_state$active_modes[chg]$swarm$history_locations[nmmso_state$active_modes[chg]$swarm$shifted_loc,] + temp_velocity
            reject = reject + 1
          }
          
          reject = 0
          if (shifted == 1) {
            #if moved, update velocity with that used
            nmmso_state$active_modes[chg]$swarm$velocities[nmmso_state$active_modes[chg]$swarm$shifted_loc,] = temp_velocity
          }else{
            nmmso_state$active_modes[chg]$swarm$number_of_particles = nmmso_state$active_modes[chg]$swarm_number_of_particles + 1
            nmmso_state$active_modes[chg]$swarm$shifted_loc = nmmso_state$active_modes[chg]$swarm$number_of_particles
            temp_vel = mn - 1
            while (sum(temp_vel < mn) > 0 ||
                   sum(temp_vel > mx) > 0) {
              temp_vel = uniform_sphere_points(1,length(new_location)) * (d / 2);
              reject = reject + 1;
              if (reject > 20) {
                # resolve if keep rejecting
                temp_vel = rand(size(new_location)) * (mx - mx) + mn;
              }
            }
            nmmso_state$active_modes[chg]$swarm$velocites[nnms_state$active_modes[chg]$swarm$shifted_loc,] = temp_vel
          }
          nmmso_state$active_modes[chg]$swarm$new_location = new_location
          
          list("nmmso_state" = nmmso_state, "cs" = cs)
          
    }
    
  }
}

evaluate_new_locations <-
  function(nmmso_state, problem_function_params, I) {
    nmmso_state$active_modes_changed = matrix(0, length(nmmso_state$active_modes), 1)
    for (i in 1:length(I)) {
      nmmso_state = evaluate(nmmso_state, I[i], problem_function, problem_function_params)[1]
      mode_shift = evaluate(nmmso_state, I[i], problem_function, problem_function_params)[2]
      if (mode_shift == 1) {
        nmmso_state$active_modes_changed[I[i]] = 1
        nmmso_state$M_loc[I[i],] = nmmso_state$active_modes[I[i]]$swarm$new_location
        nmmso_state$V_loc[I[i]] = nmmso_state$active_modes[I[i]]$swarm$mode_value
        nmmso_state$active_modes[I[i]]$swarm$less_fit_move = 0
      }
    }
    
    # return the values
    list("nmmso_state" = nmmso_state, "number_of_new_location" = length(I))
  }

evolve <-
  function(nmmso_state, problem_function, mn, mx, problem_function_params, max_evol, swarm_size) {
    n = length(nmmso_state$active_modes)
    
    if (n > max_evol) {
      if (runif(1) < 0.5) {
        I = sort(nmmso_state$V_loc, decreasing = TRUE)
      } else {
        I = 1:n
      }
      I = I[i:max_evol]
      n = max_evol
    } else {
      I = 1:n
    }
    
    II = sample(n)
    R = UNI(
      nmmso_state$active_modes[I[II[1]]]$swarm$mode_location, nmmso_state$active_modes[I[II[2]]]$swarm$mode_location
    )
    
    nmmso_state.M_loc = matrix(nmmso_state$M_loc, R)
    
    swarm$new_location = R
    swarm = evaluate_first(swarm, problem_function, problem_function_params, nmmso_state, swarm_size, mn, mx)[1]
    nmmso_state = evaluate_first(swarm, problem_function, problem_function_params, nmmso_state, swarm_size, mn, mx)[2]
    
    nmmso_state$V_loc = matrix(nmmso_state$V_loc, swarm$mode_value)
    nmmso_state$active_modes[length(nmmso_state$active_modes) + 1]$swarm = swarm
    
    # Mark these as new
    nmmso_state$active_modes_changed = matrix(nmmso_state$active_modes_changed, 1)
    nmmso_state$converged_modes = matrix(nmmso_state$converged_modes, 0)
    number_of_new_modes = 1
    
    # return values
    list("nmmso_state" = nmmso_state,"number_of_new_modes" = number_of_new_modes)
  }



hive <-
  function(nmmso_state, problem_function, mn, mx, problem_function_params, max_evol, swarm_size) {
    number_of_new_samples = 0
    LL = length(nmmso_state$active_modes)
    fit_I = sample(LL)
    
    limit = min(max_evol, LL)
    
    I2 = fit_I[1:limit]
    CI = rep(0, length(I2))
    
    # first identify those swarms who are at capacity, and therefore maybe considered for splitting off a member
    for (i in 1:length(I2)) {
      if (nmmso_state$active_modes[i]$swarm$number_of_particles >= swarm_size) {
        CI[i] = 1
      }
    }
    
    CI = which(CI == 1)
    # only check on full swarms
    if (length(CI) == 0) {
      # select swarm at random
      r = sample(length(CI))
      r = CI(r[1])
      
      # select and active swarm member at random
      k = sample(nmmso_state$active_models[r]$swarm$number_of_particles)
      k = k[1]
      R = nmmso_state$active_modes[r]$swarm$history_locations[k,]
      R_v = nmmso_state$active_modes[r]$swarm$history_values[k,]
      
      # only look at splitting off member who is greater than tol_value
      # distance away; otherwise will be merged riht in aigain at the next iteration
      
      # might need better distance function which takes two separate matrices
      if (sqrt(dist(
        R, nnmso_state$active_modes[r]$swarm$mode_location, method = "euclidean"
      )) > nnmso_state$tol_val) {
        mid_loc = 0.5 * (nnmso_state$active_modes[r]$swarm$mode_location - R) +
          R
        
        swarm$new_location = mid_loc
        result = evaluate_first(
          swarm, problem_function, problem_function_params, nnmso_state, swarm_size, mn, mx
        )
        swarm = result$swarm
        nmmso_state = result$nmmso_state
        mid_loc_val = swarm$mode_value
        
        # if valley between, then hive off the old swarm member to create new swarm
        
        if (swarm$mode_value < R_v) {
          reject = 0
          # allocate new swarm
          swarm$mode_location = R # gbest location
          swarm$mode_value = R_v # gbest value
          
          swarm$history_location(1,) = R
          swarm$history_values[1] = R_v
          
          swarm$pbest_locations[1,] = R
          swarm$pbest_values[1] = R_v
          
          nmmso_state$M_loc = rbind(nmmso_state$M_loc, R)
          nmmso_state$V_loc = rbind(nmmso_state$V_loc, R_v)
          
          nmmso_state$active_modes[end + 1]$swarm = swarm_size
          
          nmmso_state$active_modes_changed = rbind(nmmso_state$active_modes_changed, 1)
          nmmso_state$converged_modes = rbind(nmmso_state$converged_modes, 0)
          
          # remove from existing swarm and replace with mid eval
          # see above, probably not the right distance function
          d = sqrt(dist(nmmso_state$active_modes[r]$swarm$mode_location, R), method =
                     "euclidean")
          
          nmmso_state$active_modes[r]$swarm$history_location[k,] = mid_loc
          nmmso_state$active_modes[r]$swarm$history_values[k,] = mid_loc_val
          
          nmmso_state$active_modes[r]$swarm$pbest_locations[k,] = mid_loc
          nmmso_state$active_modes[r]$swarm$pbest_values[k,] = mid_loc_val
          
          temp_vel = mn - 1
          while (sum(temp_vel < mn) > 0)
            || sum(temp_vel > mx) > 0) {
          temp  -  vel = uniform_sphere_pints(1, length(R)) * (d  /  2)
          reject = reject +  1
          if (reject > 20) {
            temp_vel = runif(size(R)) * (mx  -  mn) + mn
          } # resolve repeated rejection
        }
        nmmso_state$active_modes[r]$swarm$velocities[k,] = temp_velocity

        }else{
          if (swarm$mode_value > nmmso_state$active_modes[r]$swarm$mode_value) {
            # discovered better than original, so replace more accordingly
            nmmso_state$active_modes[r]$swarm$mode_value = swarm$mode_value
            nmmso_state$active_modes[r]$swarm$mode_location = swarm$mode_location
          }
        }
        number_of_new_samples = number_of_new_samples + 1
      }
      list("nmmso_state" = mmso_state,"number_of_new_samples" number_of_new_samples)
    }
  }
}




random_new <-
  function(nmmso_state, problem_function, mn, mx, problem_function_params, swarm_size) {
    number_rand_modes = 1
    x = runif(size(mx)) * (mx - mn) + mn
    
    nmmso_state$active_modes_changed = rbind(nmmso_state$active_modes_changed, matrix(1, number_rand_modes, 1))
    nnmso_state$converged_modes = rbind(nmmso_state$converged_modes, matrix(0, number_rand_modes, 1))
    
    result = evaluate_first(swarm, problem_function, problem_function_params, nmmso_state, swarm_size, mn, mx)
    swarm = result$swarm
    nmmso_state = result$nmmso_state
    
    nmmso_state$active_modes[end + 1]$swarm = swarm
    nmmso_state$M_loc = rbind(nmmso_state$M_loc, x)
    nmmso_state$V_loc = rbind(nmmso_state$V_loc, nnmso_state$active_modes[end]$swarm$mode_value)
    
    list("nmmso_state" = nmmso_state, "number_rand_modes" = number_rand_modes)
  }

# simulates binary crossover
UNI <- function(x1, x2) {
  l = length(x1)
  x_c = x1
  x_d = x2
  r = which(runif(l,1) > 0.5)
  if (length(r) >= 0) {
    r = sample(1)
    r = r[1]
    print(r)
  }
  x_c[r] = x2[r]
  x_d[r] = x1[r]
  list("x_c" = x_c, "x_d" = x_d)
}

# Refer to: http://stackoverflow.com/questions/33350190/pointwise-multiplication-and-right-matrix-division
uniform_sphere_points <- function(n,d) {
  # function generates n points uniformly within the unit sphere in d dimensions
  z <- matrix(rnorm(n * d), nrow = n, ncol = d)
  z * (runif(n) ^ (1 / d) / sqrt(rowSums(z ^ 2)))
}
# helper functions which imitates the behavior of the Matlab feval
feval <- function(f,...) {
  f(...)
}