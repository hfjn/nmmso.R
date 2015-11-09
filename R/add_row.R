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
	if(!is.null(nrow(original))) {
		if(row_index <= nrow(original)) {
		  original[row_index, ] <- new_object
		} else if(row_index >= 2) {
			while(nrow(original) < row_index - 1) {
			  original <- rbind(original, 0)
			}
		  original <- rbind(original, new_object)
		}
	} else if(row_index == 2){
	  original <- rbind(original, new_object)
	}	else{
	  original <- new_object
	}
	return(original);	
}