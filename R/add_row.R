#' @title add_col
#' @description Function that adds new rows into a matrix/vector (depending the case). 
#' It works simulating the behavior of a matrix in Matlab. 
#' Everytime you want to add a row or a full matrix in an out of bounds index, automatically is created with the row given.
#' It internally calls the add_col function to do this.
#' 
#' @param original Initial variable where the final matrix will be stored, it can be already matrix.
#' @param row_index Numeric index representing the row where the new line should be inserted.
#' @param new_object Row or matrix to be inserted in the original one.
#' @return The matrix with the changes done after the function was run.
#' 
#' @export


add_row <- function(original, index, new_object){
	t(add_col(t(original), index, t(new_object)))
}
