#' Windowed Convolution Filtering
#' 
#' A windowed rolling mean function based on optimized
#' FFT convolution filtering
#' 
#' @param x a numeric vector
#' @param w integer or numeric vector; the convolution window. \cr
#' Can either be a single integer specifying the \cr
#' width of a rectangular window, or a vector describing \cr
#' a discrete window of any shape
#' @param B prime; the smoothness of the convolution sequences. Default is 7, \cr
#' meaning that the sequences will be padded to a length that has \cr
#' prime factors 1, 2, 3, 5 and 7
#' @param scale.window logical; should the window be scaled so that its values sum to 1?
#'
#' @details
#' This convolution filtering relies on the convolution theorem and the \cr
#' Cooley–Tukey FFT algorithm to ensure efficient computation. \cr
#' The FFT is fastest when the length of the series being transformed is smooth \cr 
#' (i.e., has many factors). If this is not the case, the transform \cr
#' may take a long time to compute and will use a large amount of memory. 
#' 
#' 
#' 
#' @export
#' @examples
#' rollconv(rep(0:1, 3, each=4), 5, fill=NULL)
#'
#'
#' x <- rep(0:1, 2, each=6)
#' plot(x)
#' matlines(cbind(
#'   rollconv(x, w=c(1, 2, 3, 4, 3, 2, 1)),
#'   rollconv(x, w=7),
#'   rollconv(x, w=c(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1)),
#'   rollconv(x, w=11)
#'   ), lty=1, lwd=2)
#' 
#' 
#' opar <- par(no.readonly=TRUE)
#' par(mar=c(2, 2, 1, 1), xaxs="r")
#' 
#' set.seed(1)
#' x <- rollconv(rnorm(400), 5, fill=NULL)
#' 
#' w <- 60
#' win1 <- trapezwin(w, l.slopes=1)
#' win2 <- trapezwin(w, l.slopes=3)
#' win3 <- trapezwin(w, l.slopes=5)
#' win4 <- trapezwin(w, l.slopes=7)
#' 
#' y1 <- rollconv(x, win1)
#' y2 <- rollconv(x, win2)
#' y3 <- rollconv(x, win3)
#' y4 <- rollconv(x, win4)
#' 
#' plot(x, type="l", col="grey", ylim=c(
#'   min(c(y1, y2, y3, y4), na.rm=TRUE)*1.1, 
#'   max(c(y1, y2, y3, y4), na.rm=TRUE)*1.1),
#'   lwd=0.5)
#' lines(y1, col="#FF000088", lwd=2)
#' lines(y2, col="#FFAA0088", lwd=2)
#' lines(y3, col="#00AAFF88", lwd=2)
#' lines(y4, col="#0000FF88", lwd=2)
#' 
#' par(opar)
rollconv <- function(x, w, B=7, scale.window=TRUE, fill=NA) {
	pn <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
	if (B > max(pn)) stop("B is too large")
	lx <- length(x)
	lw <- length(w)
	
	if (lw == 1) {
		lw <- w 
		w <- rep(1, w)
	}
	
	if (scale.window) {
		w <- w/sum(w)	
	}
	
	cn <- nextn(lx+lw, factors=pn[pn <= B])+1
    x <- c(x, rep(0.000001, cn-(lx+lw)))
    
    NA1 <- rep(fill, floor((lw-1)/2))
    NA2 <- rep(fill, ceiling((lw-1)/2))

    z <- convolve(x, w, type="filter")
    z <- c(NA1, z[1:(lx-(lw-1))], NA2)
    z
}

roxcomm()
