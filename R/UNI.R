#' @title UNI
#' @description Simulates a binary crossover.
#'
#' @param x1 Vector number 1
#' @param x2 Vector number 2
#' @return
#' x_c = x1 crossovered
#' x_d = x2 crossovered 
#'
#' @export
UNI <- function(x1, x2) {
  l = length(x1)
  x_c = x1
  x_d = x2
  r = which(runif(l) > 0.5)

  # ensures at least one swapped
  if (!(length(r) >= 0)) {
    r = sample(1)
    r = r[1]
  }
  x_c[r] = x2[r]
  x_d[r] = x1[r]

  # return values
  return(list("x_c" = x_c, "x_d" = x_d))
}