add_row <- function(x, n, a){
	if(!is.null(nrow(x))) {
		if(n <= nrow(x)) {
			x[n, ] <- a
		}
		else if(n >= 2) {
			while(nrow(x) < n - 1) {
				x <- rbind(x, 0)
			}
			x <- rbind(x, a)
		}
	}
	else if(n == 2){
		x <- rbind(x, a)
	}	
	else{
		x <- a
	}
	return(x);	
}