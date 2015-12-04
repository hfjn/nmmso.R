#' @title feval
#' @description Helper functions which imitates the behavior of the Matlab feval.
#'
#' @param f Function given to be run.
#' @param ... Passing arguments on to other functions.
#'
#' @export
feval <- function(f,...) {
  f(...)
}