library(pracma)

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
  
  ## current locations of swarm
  swarm$history_locations = matrix(0, length(swarm$mode_location))
  swarm$history_values = matrix(1, swarm_size, 1) * -Inf
  
  ## pbest locations
  swarm$pbest_locations = matrix(0, swarm_size, length(swarm$mode_location))
  swarm$pbest_values = matrix(1, swarm_size, 1) * -Inf
  
  ## velocities
  swarm$velocities = rand(size(mx))*(mx-mn)+mn
  swarm$number_of_particles = 1
  
  ## History Locations
  swarm$history_locations[1, ] = swarm$mode_location
  swarm$history_valus[1] = y
  
  ## pbest Locations
  swarm$pbest_locations[1,] = swarm.mode_location
  swarm$pbest_values[1] = y
  
  
  # track all the changes
  nmmso_state$X[nmmso_state$index, ] = swarm$new_location
  nmmso_state$Y[nmmso_state$index, ] = y
  nmmso_state$index = nmmso_state$index + 1
  
  # return the result
  list(nmmso_state, swarm)
}

merge_swarms <- function(nmmso_state, problem_function, problem_function_params, mn, mx){
  
}

evaluate <- function(nmmso_state, chg, problem_function, problem_function_params){
  y = feval(problem_func, nmmso_state$active_modes(chg$swarm$new_location, problem_function_params))
  mode_shift = 0
  
  if(y > nmmso_state$active_modes[chg]$swarm$mode_value){
    nmmso_state$active_modes[chg]$swarm$mode_location = nmmso_state$active_modes[chg]$swarm$new_location
    nmmso_state$active_modes[chg]$swarm$mode_value = y
    mode_shift = 1
  }
  
  nmmso_state$active_modes[chg]$swarm$history_location[nmmso_state$active_modes[chg]$swarm$shifted_loc,] = nmmso_state$active_modes[chg]$swarm$new_location
  nnmso_state$active_modes[chg]$swarm$history_vaues[nmmso_state$active_modes[chg]$swarm$shifted_loc] = y
  
  # if better than personal best for swarm member - then replace
  if(y > nmmso_state$active_modes[chg]$swarm$pbest_values[nmmso_state$active_modes[chg]$swarm$shifted_loc]){
    nmmso_state$active_modes[chg]$swarm$pbest_values[nmmso_state$active_modes[chg]$swarm$shifted_loc] = y
    nmmso_state$active_modes[chg]$swarm$pbest_location[nmmso_state$active_modes[chg]$swarm$shifted_loc, ] = nmmso_state$active_modes[chg].swarm.new_location 
  }
  
  # change the x and y of the curren active mode
  nmmso_state$active_modes[chg]$X[nmmso_state$index,] = nmmso_state$active_modes[chg]$swarm$new_location
  nmmso_state$active_modes[chg]$Y[nmmso_state$index] = y
  nmmso_state$index = nmmso_state$index + 1
  
  # return the result
  list(nmmso_state, mode_shift, y)
}

evaluate_mid <- function(nmmso_state, chg, problem_function, test_function_params){
  
  # comment from original:
  # new_location is the only solution thus far in mode, so by definition is
  # also the mode estimate, and the only history thus far
  
   y = feval(problem_function, nmmso_state$active_modes[chg]$swarm$new_location, test_function_params)
   mode_shift = 0
   
   if (y > nmmso_state$active_modes[chg]$swarm$mode_value){
     nmmso_state$active_modes[chg]$swarm$mode_location = nmmso_state$active_modes[chg]$swarm$new_location
     nmmso_state$active_modes[chg]$swarm$mode_value = y
     mode_shift = mode_shift + 1
   }
   
   nmmso_state$X[nmmso_state$index,] = nmmso_state$active_modes[chg]$swarm$new_location
   nmmso_state$Y[nmmso_state$index] = y
   nmmso_state$index = nmmso_state$index + 1
   
   # return the values
   list(nmmso_state, mode_shift, y)
}

merge_swarms_together <- function(swarm1, swarm2){
  # merge swarm 1 and 2, while keeping best elements of both
  
  n1 = swarm1$number_of_particles
  n2 = swarm2$number_of_particles
  max_size = size(swarm1$history_location, 1)
  
  if(n1 + n2 <= max_size){
    swarm1$number_of_particles = n1 + n2
    
    # simples situation, where the combined active members of both
    # populations are below the total size they can grow to
    
    swarm1$history_location[(n1+1):(n1+n2),] = swarm2$history_location[1:n2,] # current location of swarm
    swarm1$history_values[(n1 + 1) : (n1+n2)] = swarm2$history_values[1:n2] # current values of swarm
    
    swarm1$pbest_locations[(n1+1) : (n1 + n2),] = swarm2$pbest_locations[1:2] # current best locations of swarm
    swarm1$pbest_values[(n1+1) : n1+n2] = swarm2$pbest_values[1:n2] # current best locations of swarm
    
    swarm1$velocities[(n1 + 1): (n1+n2), ] = swarm2$velocities[1:n2,] # current velocities of swarm
  }else{
    # select best out of combined population, based on current location
    
    swarm1$number_of_particles = max_size
    temp_h_loc = rbind(swarm$history_lcoation[1:n1,], swarm2$history_locations[1:n2,])
    temp_h_v = rbind(swarm1$history_values[1:n1, ], swarm2$history_values[1:n2])
    
    temp_p_loc = rbind(swarm$pbest_location[1:n1,], swarm2$pbest_locations[1:n2,])
    temp_p_v = rbind(swarm$pbest_values[1:n1], swarm$pbest_values[1:n2])
    temp_vel = rbind(swarm1$velocities[1:n1,], swarm2$velocities[1:n2,])

    result <- sort(temp_h_v, decresing = TRUE, index.return = TRUE)
    swarm1$history_locations = temp_h_loc(result$ix(1:max_size),)
    swarm1$history_values = temp_h_v(result$ix(1:max_size),)
    swarm1$pbest_location = temp_p_loc(results$ix(1:max_size),)
    swarm1$pbest_values = temp_p_v(results$ix(1:max_size), )
    swarm$velocities = temp_vel(results$ix(1:max_size),)
    
  }
}


increment_swarm <- function(nmmso_state, chg, mn, mx, swarm_size){
  cs = 0
  new_location = mn -1
  d = nmmso_state$active_modes[chg]$swarm$dist
  
  shifted = 0
  omega = 0.1
  reject = 0
  
  # create a random particle
  r = sample(swarm_size)
  
  while (sum(new_location < mn) > 0 || sum (new_location > mx) > 0){
    # if swarm not at maximum capacity add a new particle
    if(nmmso_state$active_modes[chg]$swarm$number_of_particles < swarm_size){
      new_location = nmmso_state$active_modes[chg]$swarm$mode_location + uniform_sphere_points(1, length(new_location)) * (d/2)
    }else{
      #otherwise move and existing particle
      shifted = 1
      nmmso_state$active_modes(chg)$swarm$shifted_loc = r(1)
      
      temp_velocity = omega*nmmso_state$active_modes[chg]$swarm$velocities[nmmso_state$active_modes[chg]$swarm$shifted_loc,] + 2.0
      * matrix(runif(size(new_location^2),size(new_location)) * (nmmso_state$active_modes[chg]$swarm$mode_location - nnmso_state$active_modes[chg]$swarm$history_locations[nnmso_state$active_modes[chg]$swarm$shifted_loc,]
      + 2.0 * matrix(size(new_location^2), size(new_location)) * (nnmso_state$active_modes[chg]$swarm$pbest_location[nnmso_state$active_modes[chg]$swarm$shifted_loc,]- nnmso_state$active_modes(chg)$swarm$history_locations$[nnmso_state$active_modes(chg)$chg$swarm$shifted_loc,])
      
      if(reject > 20){
        I_max = which(((nnmso_state$active_modes[chg]$swarm$history_locations[nmmso_state$active_modes[chg]$swarm$shifted_loc, ] + temp_velocity) > mx) == 1) 
        I_min = which(((nnmso_state$active_modes[chg]$swarm$history_locations[nmmso_state$active_modes[chg]$swarm$shifted_loc, ] + temp_velocity) < mn) == 1) 
        
        if(length(I_max) >= 0){
          temp_velocity(I_max) = runif(1, length(I_max))*(mx[I_max]-nmmso_state$active_modes[chg]$swarm$history_locations[nnmso_state$active_modes[chg]$swarm$shifted_loc, I_max])
        }
        if(length(I_min) >= 0){
          temp_velocity(I_min) = runif(1, length(I_min))*((nnmso_state$active_modes[chg]$swarm$history_locations[nmmso_state$active_modes[chg]$swarm$shifted_loc,I_min]-mn(I_min))*-1)
        }
        new_location = nmmso_state$active_modes[chg]$swarm$history_locations[nmmso_state$active_modes[chg]$swarm$shifted_loc,] + temp_velocity
        reject=reject +1
      }
      
      reject = 0
      if(shifted == 1){ #if moved, update velocity with that used
       nmmso_state$active_modes[chg]$swarm$velocities[nmmso_state$active_modes[chg]$swarm$shifted_loc,] = temp_velocity
       }else{
         nmmso_state$active_modes[chg]$swarm$number_of_particles = nmmso_state$active_modes[chg]$swarm_number_of_particles + 1
         nmmso_state$active_modes[chg]$swarm$shifted_loc = nmmso_state$active_modes[chg]$swarm$number_of_particles
         temp_vel = mn-1
         while (sum(temp_vel < mn)> 0 || sum(temp_vel > mx)>0){
           temp_vel = uniform_sphere_points(1,length(new_location))*(d/2);
           reject = reject+1;
         if (reject>20){ # resolve if keep rejecting
          temp_vel = rand(size(new_location))*(mx-mx) + mn;
         }
         }
         nmmso_state$active_modes[chg]$swarm$velocites[nnms_state$active_modes[chg]$swarm$shifted_loc,] = temp_vel
       }
      nmmso_state$active_modes[chg]$swarm$new_location = new_location
      
      list(nmmso_state, cs)
          
    }
    
  }
}

evaluate_new_locations <- function(nmmso_state, problem_function_params, I){
  
}

evolve <- function(nmmso_state, problem_function, mn, mx, problem_function_params, max_evol, swarm_size){
  
}

hive <- function(nmmso_state, problem_function, mn, mx, problem_function_params, max_evol, swarm_size){
  
}

random_new <- function(nmmso_satte, problem_function, mn, mx, problem_function_params, swarm_size){
  
}

# simulates binary crossover
UNI <- function(x1, x2){
  l = length(x1)
  x_c = x1
  x_d = x2
  r = which(runif(l,1)>0.5)
  if(length(r) >= 0){
    r = sample(1)
    r= r[1]
    print(r)
  }
  x_c[r] = x2[r]
  x_d[r] = x1[r]
  list(x_c, x_d)   
}

# Refer to: http://stackoverflow.com/questions/33350190/pointwise-multiplication-and-right-matrix-division
uniform_sphere_points <- function(n,d){
  # function generates n points uniformly within the unit sphere in d dimensions
  z <- matrix(rnorm(n*d), nrow=n, ncol=d)
  z * (runif(n)^(1/d) / sqrt(rowSums(z^2)))
}
# helper functions which imitates the behavior of the Matlab feval
feval <-function(f,...){f(...)}