#' @title uniform_sphere_points
#' @descripction Function that generates n points uniformly within the unit sphere in d dimensions.
#'
#' @param n
#' @param d
#' @return X = the matrix generated
#'
#' @export
# Refer to: http://stackoverflow.com/questions/33350190/pointwise-multiplication-and-right-matrix-division
uniform_sphere_points <- function(n, d) {
  z <- matrix(rnorm(n * d), nrow = n, ncol = d)
  return(z * (runif(n) ^ (1 / d) / sqrt(rowSums(z ^ 2))))
}