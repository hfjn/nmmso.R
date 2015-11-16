#' TODO: This need to be documented extensively
#' @title increment_swarm
#'
#' @param nmmso_state Structure holding state of swarm.
#' @param index index of the current
#' @param mn Minimum design parameter values (a vector with param_num elements).
#' @param swarm_size Maximum number of elements (particles) per swarm.
#' @return
#' nmmso_state = Structure holding state of swarm.
#' 
#' @export
increment_swarm <- function(nmmso_state, index, mn, mx, swarm_size) {
  cs = 0
  new_location = mn - 1
  d = nmmso_state$swarms[[index]]$dist
  
  shifted = 0
  omega = 0.1
  reject = 0
  
  # select a particle at randome to move
  r = sample(swarm_size)
  
  while (sum(new_location < mn) > 0 || sum (new_location > mx) > 0) {
    # # #print("i")
    # if swarm not at maximum capacity add a new particle
    if (nmmso_state$swarms[[index]]$number_of_particles < swarm_size) {
      new_location = nmmso_state$swarms[[index]]$mode_location + uniform_sphere_points(1, length(new_location)) * (d / 2)
    }else{
      # otherwise move an existing particle
      shifted = 1
      nmmso_state$swarms[[index]]$shifted_loc = r[1]
      # #print(r[1])
      # str(nmmso_state$swarms[[index]]$velocities)
      # splitted up the crazy temp_velocity calculation in 6 parts
      x1 = nmmso_state$swarms[[index]]$velocities[nmmso_state$swarms[[index]]$shifted_loc,]
      x2 = matrix(runif(size(new_location)[1]*size(new_location)[2]), size(new_location)[1])
      x3 = nmmso_state$swarms[[index]]$mode_location
      x4 = nmmso_state$swarms[[index]]$history_locations[nmmso_state$swarms[[index]]$shifted_loc,]
      x5 = nmmso_state$swarms[[index]]$pbest_locations[nmmso_state$swarms[[index]]$shifted_loc,]
      x6 = nmmso_state$swarms[[index]]$history_locations[nmmso_state$swarms[[index]]$shifted_loc,]

      temp_velocity = omega * x1 + 2.0 * x2 * (x3 - x4) + 2.0 * matrix(length(new_location)^2, length(new_location)) * (x5 - x6)
      reject = reject + 1

      if (reject > 20) {
        # if we keep rejecting, then put at extreme any violating design patterns
        I_max = which(((nmmso_state$swarms[[index]]$history_locations[nmmso_state$swarms[[index]]$shifted_loc,] + temp_velocity) > mx) == 1)
        I_min = which(((nmmso_state$swarms[[index]]$history_locations[nmmso_state$swarms[[index]]$shifted_loc,] + temp_velocity) < mn) == 1)          
        if (length(I_max) >= 0) {
          temp_velocity[I_max] = runif(1, length(I_max)) * (mx[I_max] - nmmso_state$swarms[[index]]$history_locations[nmmso_state$swarms[[index]]$shifted_loc, I_max])
        }
        if (length(I_min) >= 0) {
          temp_velocity[I_min] = runif(1, length(I_min)) * ((nmmso_state$swarms[[index]]$history_locations[nmmso_state$swarms[[index]]$shifted_loc,I_min] - mn[I_min]) * -1)
        }
        new_location = nmmso_state$swarms[[index]]$history_locations[nmmso_state$swarms[[index]]$shifted_loc,] + temp_velocity
        reject = reject + 1
      }
    }
  }
  reject = 0
  
  if (shifted == 1) {
    #if moved, update velocity with that used
    nmmso_state$swarms[[index]]$velocities = add_row(nmmso_state$swarms[[index]]$velocities, nmmso_state$swarms[[index]]$shifted_loc, temp_velocity)
  }else{
    # otherwise initialise velocity in sphere based on distance from gbest to next closest mode
    number_of_particles = nmmso_state$swarms[[index]]$number_of_particles
    nmmso_state$swarms[[index]]$number_of_particles = (number_of_particles + 1)
    nmmso_state$swarms[[index]]$shifted_loc = nmmso_state$swarms[[index]]$number_of_particles
    temp_vel = mn - 1
    while (sum(temp_vel < mn) > 0 || sum(temp_vel > mx) > 0){
      temp_vel = uniform_sphere_points(1,length(new_location)) * (d / 2)
      reject = reject + 1;
      if (reject > 20) {
        # resolve if keep rejecting
        temp_vel = matrix(runif(size(new_location)[1]*size(new_location)[2]), size(new_location)[1]) * (mx - mx) + mn
      }
    }
    nmmso_state$swarms[[index]]$velocities = add_row(nmmso_state$swarms[[index]]$velocities, nmmso_state$swarms[[index]]$shifted_loc, temp_vel)
  }
  nmmso_state$swarms[[index]]$new_location = new_location

  # return value
  list("nmmso_state" = nmmso_state)
} 
  


