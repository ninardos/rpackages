#' A Function to calculate the moving average
#'
#' Calculate the moving average of a vector
#' @param v: the vector over which to calculate the moving average. 
#' @param n: the interval to consider.
#' @keywords moving average
#' @export
#' @examples
#' moving_avg(1:10, n=7) 
#' [1] NA NA NA NA NA NA  4  5  6  7

moving_avg <- function(v, n = 7) 
{
    if (length(v) < n) return(rep(NA, length(v)))
    f <- rep(1/n, n)
    mv_avg <- filter(v, f, sides = 1)
	mv_avg <- as.numeric(mv_avg)
	return(as.numeric(mv_avg))
}


