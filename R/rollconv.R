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
#' @param scale.window logical; should the window be scaled so that its values
#' sum to 1?
#' @param fill single character or numeric; used for filling out start/end. \cr
#' default is NA. set to NULL for no fill
#' 
#'
#' @details
#' This convolution filtering relies on the convolution theorem and the \cr
#' Cooley–Tukey FFT algorithm to ensure efficient computation. \cr
#' The FFT is fastest when the length of the series being transformed is smooth \cr 
#' (i.e., has many factors). If this is not the case, the transform \cr
#' may take a long time to compute and will use a large amount of memory. \cr
#' There is a trade-off between smoothness and series length. A smoothness of 2 will \cr
#' make calculations more efficient, but will also on average require the series \cr
#' to be padded more, making it longer, and hence require more resources. \cr
#' A smoothness of 5 or 7 is generally OK.
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
#' ### filtering out harmonics
#' x <- rep(c(1, 2, 4, 2), 10, each=3)
#' x <- x + (seq_along(x)/50) * sin(seq_along(x)/20)
#' 
#' # a triangular filter will in general produce a result with less
#' # ringing artefacts than a simple box/rectangular.
#' 
#' # Here the 'steppyness' is filtered out using a triangular filter
#' # of about twice the width of the steps.
#' r1 <- rollconv(x, w=c(1, 2, 3, 2, 1), fill=NA)
#' 
#' # using a rectangular filter retains the triangular shape of the wave
#' r2 <- rollconv(x, w=3, fill=NA)
#' 
#' # A simple box filter will isolate the underlying smooth wave as long as
#' # the width is carefully tuned
#' r3 <- rollconv(x, w=12, fill=NA)
#' r4 <- rollconv(x, w=13, fill=NA)
#' 
#' plot(x, pch=16, cex=0.4)
#' lines(r2, col="skyblue", lwd=2)
#' lines(r1, col="blue", lwd=2)
#' lines(r4, col="green", lwd=2)
#' lines(r3, col="darkgreen", lwd=2)
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
    
    NA1 <- rep(fill[1], floor((lw-1)/2))
    NA2 <- rep(fill[2], ceiling((lw-1)/2))

    z <- convolve(x, w, type="filter")
    z <- c(NA1, z[1:(lx-(lw-1))], NA2)
    z
}

