#' @title 
#' @param nmmso_state
#' @param chg
#' @param mn
#' @param swarm_size
increment_swarm <- function(nmmso_state, chg, mn, mx, swarm_size) {
  cs = 0
  new_location = mn - 1
  d = nmmso_state$active_modes[[chg]]$swarm$dist
  
  shifted = 0
  omega = 0.1
  reject = 0
  
  # create a random particle
  r = sample(swarm_size)
  
  while (sum(new_location < mn) > 0 ||
         sum (new_location > mx) > 0) {
    # if swarm not at maximum capacity add a new particle
    if (nmmso_state$active_modes[[chg]]$swarm$number_of_particles < swarm_size) {
      new_location = nmmso_state$active_modes[[chg]]$swarm$mode_location + uniform_sphere_points(1, length(new_location)) * (d / 2)
    }else{
      #otherwise move an existing particle
      shifted = 1
      nmmso_state$active_modes[[chg]]$swarm$shifted_loc = r(1)
      
      temp_velocity = omega * nmmso_state$active_modes[[chg]]$swarm$velocities[nmmso_state$active_modes[[chg]]$swarm$shifted_loc,] + 2.0 * matrix(
        runif(size(new_location ^ 2),size(new_location)) * (
          nmmso_state$active_modes[[chg]]$swarm$mode_location - nmmso_state$active_modes[[chg]]$swarm$history_locations[nmmso_state$active_modes[[chg]]$swarm$shifted_loc,] + 2.0 * matrix(size(new_location ^ 2), size(new_location)) * (nmmso_state$active_modes[[chg]]$swarm$pbest_location[nmmso_state$active_modes[[chg]]$swarm$shifted_loc,] - nmmso_state$active_modes[[chg]]
                                                                                                                                                                                                                                          $swarm$history_locations[nmmso_state$active_modes[[chg]]$swarm$shifted_loc,]
          )))
      if (reject > 20) {
        I_max = which(((nmmso_state$active_modes[[chg]]$swarm$history_locations[nmmso_state$active_modes[[chg]]$swarm$shifted_loc,] + temp_velocity) > mx) == 1)
        I_min = which(((nmmso_state$active_modes[[chg]]$swarm$history_locations[nmmso_state$active_modes[[chg]]$swarm$shifted_loc,] + temp_velocity) < mn) == 1)
        
        if (length(I_max) >= 0) {
          temp_velocity(I_max) = runif(1, length(I_max)) * (mx[I_max] - nmmso_state$active_modes[[chg]]$swarm$history_locations[nmmso_state$active_modes[[chg]]$swarm$shifted_loc, I_max])
        }
        if (length(I_min) >= 0) {
          temp_velocity(I_min) = runif(1, length(I_min)) * ((nmmso_state$active_modes[[chg]]$swarm$history_locations[nmmso_state$active_modes[[chg]]$swarm$shifted_loc,I_min] - mn(I_min)) * -1)
        }
        new_location = nmmso_state$active_modes[[chg]]$swarm$history_locations[nmmso_state$active_modes[[chg]]$swarm$shifted_loc,] + temp_velocity
        reject = reject + 1
      }
      
      reject = 0
      if (shifted == 1) {
        #if moved, update velocity with that used
        nmmso_state$active_modes[[chg]]$swarm$velocities[nmmso_state$active_modes[chg]$swarm$shifted_loc,] = temp_velocity
      }else{
        nmmso_state$active_modes[[chg]]$swarm$number_of_particles = nmmso_state$active_modes[chg]$swarm_number_of_particles + 1
        nmmso_state$active_modes[[chg]]$swarm$shifted_loc = nmmso_state$active_modes[chg]$swarm$number_of_particles
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
        nmmso_state$active_modes[[chg]]$swarm$velocites[nnms_state$active_modes[[chg]]$swarm$shifted_loc,] = temp_vel
      }
      nmmso_state$active_modes[[chg]]$swarm$new_location = new_location
      
      list("nmmso_state" = nmmso_state, "cs" = cs)
      
    }
    
  }
}