#' @title 
#' @param 
#' @param 
#' @return 
# simulates binary crossover
UNI <- function(x1, x2) {
  l = length(x1)
  x_c = x1
  x_d = x2
  r = which(runif(l,1) > 0.5)
  if (length(r) >= 0) {
    r = sample(1)
    r = r[1]
  }
  x_c[r] = x2[r]
  x_d[r] = x1[r]
  return(list("x_c" = x_c, "x_d" = x_d))
}