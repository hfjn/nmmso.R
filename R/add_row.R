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

	# just like in matlab break in case column dimension do not match
	if(size(original)[2] != size(new_object)[2])
		stop("Column dimensions do not match.")

	# otherwise add dependent on case
	if(row_index <= size(original)[2]) {
		  original[row_index, ] <- new_object
	} else if(row_index >= 2) {
		while(size(original)[2] < row_index - 1) {
		  original <- rbind(original, 0)
		}
	  original <- rbind(original, new_object)
	}
	else if(row_index == 2){
		original <- rbind(original, new_object)
	}	
	else{
	  original <- new_object
	}
	return(original);	
}