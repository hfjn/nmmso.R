#' @title 
#' @param nmmso_state
#' @param problem_function
#' @param mn
#' @param mx
#' @param problem_function_params
#' @param max_evol
#' @param swarm_size
#' @return 
hive <-
  function(nmmso_state, problem_function, mn, mx,  max_evol, swarm_size) {
    number_of_new_samples = 0
    LL = length(nmmso_state$active_modes)
    fit_I = sample(LL)
    
    limit = min(max_evol, LL)
    
    I2 = fit_I[1:limit]
    CI = matrix(0, length(I2))
    
    # first identify those swarms who are at capacity, and therefore maybe considered for splitting off a member
    for (i in 1:length(I2)) {
      if (nmmso_state$active_modes[[i]]$swarm$number_of_particles >= swarm_size) {
        CI[i] = 1
      }
    }
    CI = which(CI == 1)
    # only check on full swarms
    if (length(CI) != 0) {
      # select swarm at random
      r = sample(length(CI))
      r = CI[r[1]]
      
      # select and active swarm member at random
      k = sample(nmmso_state$active_models[[r]]$swarm$number_of_particles)
      k = k[1]
      R = nmmso_state$active_modes[[r]]$swarm$history_locations[k,]
      R_v = nmmso_state$active_modes[[r]]$swarm$history_values[k,]
      
      # only look at splitting off member who is greater than tol_value
      # distance away; otherwise will be merged riht in aigain at the next iteration
      
      # might need better distance function which takes two separate matrices
      if (sqrt(dist2(
        R, nmmso_state$active_modes[[r]]$swarm$mode_location
      )) > nmmso_state$tol_val) {
        mid_loc = 0.5 * (nmmso_state$active_modes[[r]]$swarm$mode_location - R) +
          R
        
        swarm$new_location = mid_loc
        result = evaluate_first(
          swarm, problem_function,  nmmso_state, swarm_size, mn, mx
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
          
          swarm$history_location[1,] = R
          swarm$history_values[1] = R_v
          
          swarm$pbest_locations[1,] = R
          swarm$pbest_values[1] = R_v
          
          nmmso_state$M_loc = rbind(nmmso_state$M_loc, R)
          nmmso_state$V_loc = rbind(nmmso_state$V_loc, R_v)
          
          nmmso_state$active_modes[[end + 1]]$swarm = swarm_size
          
          nmmso_state$active_modes_changed = rbind(nmmso_state$active_modes_changed, 1)
          nmmso_state$converged_modes = rbind(nmmso_state$converged_modes, 0)
          
          # remove from existing swarm and replace with mid eval
          # see above, probably not the right distance function
          d = sqrt(dist2(nmmso_state$active_modes[[r]]$swarm$mode_location, R))
          
          nmmso_state$active_modes[[r]]$swarm$history_location[k,] = mid_loc
          nmmso_state$active_modes[[r]]$swarm$history_values[k,] = mid_loc_val
          
          nmmso_state$active_modes[[r]]$swarm$pbest_locations[k,] = mid_loc
          nmmso_state$active_modes[[r]]$swarm$pbest_values[k,] = mid_loc_val
          
          temp_vel = mn - 1
          while (sum(temp_vel < mn) > 0 || sum(temp_vel > mx) > 0) {
            temp  -  vel = uniform_sphere_pints(1, length(R)) * (d  /  2)
            reject = reject +  1
            if (reject > 20) {
              temp_vel = runif(size(R)) * (mx  -  mn) + mn
            } # resolve repeated rejection
          }
          nmmso_state$active_modes[[r]]$swarm$velocities[k,] = temp_velocity
          
        }else{
          if (swarm$mode_value > nmmso_state$active_modes[[r]]$swarm$mode_value) {
            # discovered better than original, so replace more accordingly
            nmmso_state$active_modes[[r]]$swarm$mode_value = swarm$mode_value
            nmmso_state$active_modes[[r]]$swarm$mode_location = swarm$mode_location
          }
        }
        number_of_new_samples = number_of_new_samples + 1
      }      
    }
    list("nmmso_state" = nmmso_state, "number_of_new_samples" = number_of_new_samples)
  }
