#' @title merge_swarms
#' 
#' @param nmmso_state Structure holding state of swarm.
#' @param problem_function String containing name of function to be optimised.
#' @param problem_function_params Meta-parameters needed by problem function.
#' @param mn Minimum design parameter values (a vector with param_num elements).
#' @param mx Maximum design parameter values (a vector with param_num elements).
#' @return 
#' nmmso_state = Structure holding state of swarm.
#' number_of_mid_evals = Number of evaluations done for the merging.
#' 
merge_swarms <- function(nmmso_state, problem_function, mn, mx) {
  # only concern ourselves with modes that have actually shifted, or are new
  # since the last generation, as no need to check others
  I = which(nmmso_state$active_modes_changed == 1)
  nmmso_state$active_modes_changed = nmmso_state$active_modes_changed * 0 # reset
  
  n = length(I)
  number_of_mid_evals = 0
  # only compare if there is a changed mode, and more than on mode in system
  if (n >= 1 && (length(nmmso_state$active_modes) > 1)) {
    to_compare = matrix(0, n, 2)
    to_compare[, 1] = I
    for (i in 1:n) {      
      # calculate euclidean distance      
      d = dist2(nmmso_state$M_loc[I[i],], nmmso_state$M_loc)      
      # will be closes to itself, so need to get second closest
      d[I[i]] = Inf
      tmp = min(d)
      to_compare[i, 2] = which.min(d)
      # track euclidean distance to nearest neighbour mode
      nmmso_state$active_modes[[I[i]]]$swarm$dist = sqrt(tmp)
      
      if (nmmso_state$active_modes[[I[i]]]$swarm$number_of_particles == 1) {
        reject = 0
        # in situation where a new swarm, and therefore distance to neighbor swarm now calculated
        # so set the initial velocity at a more reasonable value for the first particle, rather than using the uniform in design space
        temp_vel = mn - 1
        while (sum(temp_vel < mn) > 0 || sum(temp_vel > mx) > 0) {
          temp_vel = uniform_sphere_points(1, length(nmmso_state$active_modes[[I[i]]]$swarm$new_location))*(nmmso_state$active_modes[[I[i]]]$swarm$dist / 2)
          reject = reject + 1
          
          # rejecting lots, so likely in a corner of design space where a significant volume of the sphere lies outside
          # the bounds, so will make do with a random legal velocity in bounds
          if (reject > 20) {
            size = dim(nmmso_state$active_modes[[I[i]]]$swarm$new_location)
            temp_vel = matrix(runif(size, min = (mx - mn), max = mn), size)
          }
        }
        nmmso_state$active_modes[[I[i]]]$swarm$velocities[1,] = temp_vel
      }
    }
    
    # to_compare now contains the pairs of indices of closest modes, where at least one mode has shifted location / is new
    # since last generation. However, there may be duplicated pairs (through reversals), so need to omit these.
    # now sort it so that first column elements are always smaller than second
    to_compare = t(apply(to_compare, 1, sort))
    # now sort it so that first column is sorted smallest to highest
    to_compare = apply(to_compare, 2, sort)
    
    # column elements on same row
    
    # Remove duplicates
    for (i in seq(n,-1, -2)) {
      # get indices of all with first index element same
      I = which(to_compare[, 1] == to_compare[i, 1])
      repeat_matrix = repmat(to_compare[i, ], length(I), 1)
      # if more than one vector duplication
      if (sum(sum(repeat_matrix == to_compare[I, ]) == 2) > 1) {
        to_compare[i,] = matrix()
      }
    }
    
    # Check for merging
    n = nrow(to_compare)
    to_merge = matrix()
    number_of_mid_evals = 0
    
    for (i in 1:n) {
      # merge if sufficiently close
      if (sqrt(dist2(nmmso_state$active_modes[[to_compare[i, 1]]]$swarm$mode_location, nmmso_state$active_modes[[to_compare[i, 2]]]$swarm$mode_location)) < nmmso_state$tol_val) {
        # can't preallocate, as don't know the size
        to_merge = matrix(to_merge, i)
      } else {
        # evaluate exact mid point between modes, and add to mode 2
        # history
        mid_loc = 0.5 * (
          nmmso_state$active_modes[[to_compare[i, 1]]]$swarm$mode_location - nmmso_state$active_modes[[to_compare[i, 2]]]$swarm$mode_location
        )
        + nmmso_state$active_modes[[to_compare[i, 2]]]$swarm$mode_location
        
        # little sanity check
        if (sum(mid_loc < mn) > 0 || sum(mid_loc > mx) > 0) {
          sprintf('Mid point out of range!')
        }
        
        nmmso_state$active_modes[[to_compare[i, 2]]]$swarm$new_location = mid_loc
        evaluate_mid = evaluate_mid(nmmso_state, to_compare[i, 2], problem_function)
        nmmso_state = evaluate_mid$nmmso_state
        mode_shift = evaluate_mid$mode_shift
        y = evaluate_mid$y
        
        if (mode_shift == 1) {
          nmmso_state$M_loc[to_compare[i, 2],] = nmmso_state$active_modes[[to_compare[i, 2]]]$swarm$mode_location
          nmmso_state$V_loc[to_compare[i, 2]] = nmmso_state$active_modes[[to_compare[i, 2]]]$swarm$mode_value
          to_merge = matrix(to_merge, i)
          # track that the mode value has improved
          nmmso_state$active_modes_changed[to_compare[i, 2]] = 1
          #better than mode 1 current mode, so merge
        } else if (nmmso_state$active_modes[[to_compare[i, 2]]]$swarm$mode_value < y) {
          to_merge = matrix(to_merge, i)
        }
        
        number_of_mid_evals = number_of_mid_evals + 1
      }
    }
    # merge those marked pairs, and flag the lower one for deletion
    delete_index = matrix(0, dim(to_merge))
    for (i in 1:length(to_merge)) {
      str(to_compare)
      str(to_merge)
      # little sanity check
      if (to_compare[to_merge[i], 2] == to_compare[to_merge[i], 1]) {
        stop('Indices should not be equal')
      }
      # if peak of mode 1 is higher than mode 2, then replace
      if (nmmso_state$active_modes[[to_compare[to_merge[i], 1]]]$swarm$mode_value > nmmso_state$active_modes[to_compare[to_merge[i], 2]]$swarm$mode_value) {
        delete_index[i] = to_compare[to_merge[i], 2]
        nmmso_state$active_modes[[to_compare[to_merge[i], 1]]]$swarm = merge_swarms_together(nmmso_state$active_modes[[to_compare[to_merge[i], 1]]]$swarm, nmmso_state$active_modes[[to_compare[to_merge[i], 2]]]$swarm)
        # track that the mode value has merge and should be compared again
        nmmso_state$active_modes_changed[[to_compare[i, 1]]] = 1
      } else {
        delete_index[i] = to_compare[to_merge[i], 1]
        nmmso_state$active_modes[[to_compare[to_merge[i], 2]]]$swarm = merge_swarms_together(nmmso_state$active_modes[[to_compare[to_merge[i], 2]]]$swarm, nmmso_state$active_modes[[to_compare[to_merge[i], 1]]]$swarm)
        # track that the mode value has merge and should be compared again
        nmmso_state$active_modes_changed[to_compare[i, 2]] = 1
      }
    }
    
    # remove one of the merged pair
    prev_merge = -1
    delete_index = apply(delete_index, 2, sort)
    for (i in seq(length(delete_index),-1, -1)) {
      if (delete_index[i] != prev_merge) {
        prev_merge = delete_index[i]
        nmmso_state$active_modes[[delete_index[i]]] = matrix()
        nmmso_state$M_loc[delete_index[i],] = matrix()
        nmmso_state$V_loc[delete_index[i]] = matrix()
        nmmso_state$converged_modes[delete_index[i]] = matrix()
        nmmso_state$active_modes_changed[delete_index[i]] = matrix()
      }
    }
    
  }
  # only one mode, so choose dist for it (smallest design dimension)
  if (length(nmmso_state$active_modes) == 1) {
    nmmso_state$active_modes[[1]]$swarm$dist = min(mx - mn)
  }
  # return the values
  list("nmmso_state" = nmmso_state, "number_of_mid_evals" = number_of_mid_evals)
}