add_row <- function(original, index, new_object){
	if(index == 1 && NROW(original) == 1){
		t(t(rbind(new_object)))	
	}
	else
		t(add_col(t(original), index, t(new_object)))
}
