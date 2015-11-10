source("./R/add_col.R")

add_row <- function(original, index, new_object){
	t(add_col(t(original), index, t(new_object)))
}
