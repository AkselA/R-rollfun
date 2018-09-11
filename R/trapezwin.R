#' Trapepezoid windows
#' 
#' Create isosceles (symmetric) trapezoid windows
#' 
#' @param l integer; length of the window
#' @param prop numeric; proportion of the length covered by slopes
#' @param l.slopes integer; length of the slopes. Overrides \code{prop}
#' @param scale character; how the window should be scaled. Divide by window sum (default),
#' max or mean
#' @param step logical; should the first and last point be lifted a small step from zero?
#' 
#' @export
#' @examples
#' # Specifying shape using prop
#' x <- 99
#' plot(0, type="n", xlim=c(0, x), ylim=c(0, 2/x))
#' for (i in (1:20)/20){
#'     lines(trapezwin(x, prop=i))
#' }
#' 
#' # Specifying shape using l.slopes
#' x <- 99
#' plot(0, type="n", xlim=c(0, x), ylim=c(0, 2/x))
#' for (i in ceiling(seq(1, x %/% 2, length.out=20))){
#'     lines(trapezwin(x, l.slopes=i))
#' }
#' 
#' # Spectral response of windows tuned for minimum first side-lobe.
#' # Convolution using a trapezoid window con be thought of as convolving twice with
#' # rectangular windows of different size. If the relative sizes are chosen with
#' # care the null of the second window can be placed in the middle of the first
#' # side lobe of the first window, and thereby reducing it.
#' set.seed(1)
#' w <- 59
#' x <- rnorm(2e4)
#' par(mfcol=c(3, 1), mar=c(1.5, 1.5, 0.5, 0.5), mgp=c(0, 0.6, 0))
#' s <- c(0.80, 0.82, 0.84)
#' for (i in s) {
#'     win <- trapezwin(w, prop=i)
#'     rol <- rollconv(x, win, partial=TRUE, scale.window=TRUE)
#'     spectrum(rol, main="", xlab="", ylab="", sub="", lwd=0.4, 
#'       ylim=c(1e-12, 1), xlim=c(0, 0.25))
#'     abline(v=(1/w)/0.58, col="#FF000044")
#'     grid(col="#00000022", lty=1)
#'     legend("top", legend=paste("prop =", round(i, 3)), bty="n",
#'       text.col="blue", adj=c(0, 0.5))
#' }

trapezwin <- function(l=10, prop=0.5, l.slopes, scale=c("sum", "max", "mean"), step=TRUE) {
	tri <- 1 - abs(seq(-1, 1, length.out=l))
	if (!missing(l.slopes)) {
		if (l.slopes < 1 || l.slopes > l %/% 2) {
			stop("l.slopes needs to be in [1, l %/% 2]")
		}
	    tri[(l.slopes+1):(l-l.slopes)] <- tri[l.slopes+1]
	} else {
		tri[tri > prop] <- prop
	}
	if (step) {
	    tri[c(1, l)] <- tri[2]/l
	}
    scale <- match.arg(scale)
    fun <- match.fun(scale)
    tri/fun(tri)
}