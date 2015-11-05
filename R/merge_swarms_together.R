#' @title merge_swarms_together
#' @description Joins two swarms into one.
#' 
#' @param swarm1 The first swarm.
#' @param swarm2 The second swarm.
#' @return The joined swarm
#' 
#' @export
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
    swarm1$history_locations = temp_h_loc[result$ix(1:max_size),]
    swarm1$history_values = temp_h_v[result$ix(1:max_size),]
    swarm1$pbest_location = temp_p_loc[results$ix(1:max_size),]
    swarm1$pbest_values = temp_p_v[results$ix(1:max_size),]
    swarm$velocities = temp_vel[results$ix(1:max_size),]
    
  }
  return(swarm1)
}