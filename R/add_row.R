#' @title add_row
#' @description Function that adds new rows into a matrix/vector (depending the case). 
#' It works simulating the behavior of a matrix in Matlab. 
#' Everytime you want to add a row or a full matrix in an out of bounds index, automatically is created with the row given.
#' 
#' @param original Initial variable where the final matrix will be stored, it can be already matrix.
#' @param row_index Numeric index representing the row where the new line should be inserted.
#' @param new_object Row or matrix to be inserted in the original one.
#' @return The matrix with the changes done after the function was run.
#' 
#' @export


add_row <- function(original, row_index, new_object){

	nr = NROW(original)
    nc = if(row_index > NCOL(original)) row_index else NCOL(original)

    i1 = nr * (row_index - 1) + 1
    i2 = i1 + length(new_object) - 1
    original[i1:i2] = new_object

    length(original) = nr * nc
    dim(original) = c(nr, nc)

    return(original)
}