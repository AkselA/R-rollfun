#' Trapepezoid windows
#' 
#' Create isosceles (symmetric) trapezoid windows
#' 
#' @param l integer; length of the window
#' @param prop numeric; proportion of the length covered by slopes
#' @param l.slopes integer; length of the slopes. Overrides \code{prop}
#' @param ground logical; should the legs of the trapezoid be 'grounded'
#' @export
#' @examples
#' x <- 100
#' plot(0, type="n", xlim=c(0, x), ylim=c(0, 2/x))
#' for (i in (1:20)/20){
#'     lines(trapezwin(x, prop=i))
#' }
#' 
#' x <- 10000
#' plot(0, type="n", xlim=c(0, x), ylim=c(0, 2/x))
#' for (i in ceiling(seq(1, x %/% 2, length.out=20))){
#'     lines(trapezwin(x, l.slopes=i))
#' }
#' 
trapezwin <- function(l=10, prop=0.5, l.slopes) {
	tri <- 1 - abs(seq(-1, 1, length.out=l))
	if (!missing(l.slopes)) {
		if (l.slopes < 1 || l.slopes > l %/% 2) {
			stop("l.slopes needs to be in [1, l %/% 2]")
		}
	    tri[(l.slopes+1):(l-l.slopes)] <- tri[l.slopes+1]
	} else {
		tri[tri > prop] <- prop
	}
	tri <- tri/sum(tri)
	tri[c(1, l)] <- tri[2]/l/10
	tri/sum(tri)
}


