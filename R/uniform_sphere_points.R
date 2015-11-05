#' @title 
#' @param n
#' @param
#' @return NMMSO
# Refer to: http://stackoverflow.com/questions/33350190/pointwise-multiplication-and-right-matrix-division
uniform_sphere_points <- function(n,d) {
  # function generates n points uniformly within the unit sphere in d dimensions
  z <- matrix(rnorm(n * d), nrow = n, ncol = d)
  z * (runif(n) ^ (1 / d) / sqrt(rowSums(z ^ 2)))
}