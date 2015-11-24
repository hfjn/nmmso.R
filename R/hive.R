#' @title hive
#'
#' @param nmmso_state Structure holding state of swarm.
#' @param problem_function String containing name of function to be optimised.
#' @param mn Minimum design parameter values (a vector with param_num elements).
#' @param mx Maximum design parameter values (a vector with param_num elements).
#' @param problem_function_params Meta-parameters needed by problem function.
#' @param max_evol Maximum number of swarms to update in a generation. If not provided this is set at 100.
#' @param swarm_size Maximum number of elements (particles) per swarm.
#' @return 
#' nmmso_state = Structure holding state of swarm after calculations.
#' number_of_new_samples = Number of samples created after hive.
#'
#' @export
hive <- function(nmmso_state, problem_function, mn, mx,  max_evol, swarm_size) {
  number_of_new_samples = 0
  LL = length(nmmso_state$swarms)
  fit_I = sample(LL)
  
  limit = min(max_evol, LL)
  
  I2 = fit_I[1:limit]
  CI = matrix(0, length(I2))
  
  # first identify those swarms who are at capacity, and therefore maybe considered for splitting off a member
  for (i in 1:length(I2)) {
    if (nmmso_state$swarms[[i]]$number_of_particles >= swarm_size) {
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
    k = sample(nmmso_state$swarms[[r]]$number_of_particles)
    k = k[1]
    R = nmmso_state$swarms[[r]]$history_locations[k,]
    R_v = nmmso_state$swarms[[r]]$history_values[k,]
    
    # only look at splitting off member who is greater than tol_value
    # distance away; otherwise will be merged riht in aigain at the next iteration
    if (sqrt(dist2(rbind(R), nmmso_state$swarms[[r]]$mode_location)) > nmmso_state$tol_val) {
      mid_loc = 0.5 * (nmmso_state$swarms[[r]]$mode_location - R) + R        
      swarm = list("new_location" = mid_loc)
      result = evaluate_first(swarm, problem_function,  nmmso_state, swarm_size, mn, mx)
      swarm = result$swarm
      nmmso_state = result$nmmso_state
      mid_loc_val = swarm$mode_value
      
      # if valley between, then hive off the old swarm member to create new swarm
      
      if (swarm$mode_value < R_v) {
        reject = 0
        # allocate new swarm into the nmmso_state
        swarm$mode_location = rbind(R) # gbest location
        swarm$mode_value = R_v # gbest value
        
        swarm$history_locations = add_row(swarm$history_locations, 1, R)
        swarm$history_values[1,] = R_v
        
        swarm$pbest_locations = add_row(swarm$pbest_locations, 1, R)
        swarm$pbest_values[1,] = R_v
        nmmso_state$mode_locations = rbind(nmmso_state$mode_locations, R)
        
        nmmso_state$mode_values = add_row(nmmso_state$mode_values, size(nmmso_state$mode_values)[1] + 1, R_v)
        
        nmmso_state$swarms = c(nmmso_state$swarms, list(swarm))

        nmmso_state$swarms_changed = add_row(nmmso_state$swarms_changed, size(nmmso_state$swarms_changed)[1] + 1, 1L)
        nmmso_state$converged_modes = c(nmmso_state$converged_modes, 0L)
        
        
        # remove from existing swarm and replace with mid eval
        # see above, probably not the right distance function
        d = sqrt(dist2(rbind(nmmso_state$swarms[[r]]$mode_location), R))
        nmmso_state$swarms[[r]]$history_locations =  add_row(nmmso_state$swarms[[r]]$history_locations, k, mid_loc)
        nmmso_state$swarms[[r]]$history_values = add_row(nmmso_state$swarms[[r]]$history_values, k, mid_loc_val)
        
        nmmso_state$swarms[[r]]$pbest_locations[k,] = mid_loc
        nmmso_state$swarms[[r]]$pbest_values[k,] = mid_loc_val
        
        temp_vel = mn - 1
        while (sum(temp_vel < mn) > 0 || sum(temp_vel > mx) > 0) {
          temp_vel = uniform_sphere_points(1, length(R)) * (as.numeric(d)  /  2)
          reject = reject +  1
          if (reject > 20) {
            temp_vel = runif(size(R)) * (mx  -  mn) + mn
          } # resolve repeated rejection
        }
        nmmso_state$swarms[[r]]$velocities = add_row(nmmso_state$swarms[[r]]$velocities, k, temp_vel)
      }else{
        if (swarm$mode_value > nmmso_state$swarms[[r]]$mode_value) {
          # discovered better than original, so replace more accordingly
          nmmso_state$swarms[[r]]$mode_value = swarm$mode_value
          nmmso_state$swarms[[r]]$mode_location = swarm$mode_location
        }
      }
      number_of_new_samples = number_of_new_samples + 1
    }     
  }
  list("nmmso_state" = nmmso_state, "number_of_new_samples" = number_of_new_samples)
}
