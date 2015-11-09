add_row <- function(x, n, a){
	if(n != 2){
		while(nrow(x) < n - 1 || is.null(nrow(x))){
			x <- rbind(x, 0)
		}
	}
	rbind(x, a)
}

x <- 1:5
test <- add_row(x, 10, x)