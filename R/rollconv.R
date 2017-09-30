# A windowed rolling mean function based on 
# optimised convolution filtering

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
    x <- c(x, rep(0.00001, cn-(lx+lw)))
    
    NA1 <- rep(fill, floor((lw-1)/2))
    NA2 <- rep(fill, ceiling((lw-1)/2))

    z <- convolve(x, w, type="filter")
    z <- c(NA1, z[1:(lx-(lw-1))], NA2)
    z
}

# opar <- par(no.readonly=TRUE)
# par(mar=c(2, 2, 1, 1), xaxs="r")

# set.seed(1)
# x <- rollconv(rnorm(400), 5, fill=0)

# w <- 60
# win1 <- trapezwin(w, l.slopes=1)
# win2 <- trapezwin(w, l.slopes=3)
# win3 <- trapezwin(w, l.slopes=5)
# win4 <- trapezwin(w, l.slopes=7)

# y1 <- rollconv(x, win1)
# y2 <- rollconv(x, win2)
# y3 <- rollconv(x, win3)
# y4 <- rollconv(x, win4)

# plot(x, type="l", col="grey", ylim=c(
  # min(c(y1, y2, y3, y4), na.rm=TRUE)*1.1, 
  # max(c(y1, y2, y3, y4), na.rm=TRUE)*1.1),
  # lwd=0.5)
# lines(y1, col="#FF000088", lwd=2)
# lines(y2, col="#FFAA0088", lwd=2)
# lines(y3, col="#00AAFF88", lwd=2)
# lines(y4, col="#0000FF88", lwd=2)

# par(opar)
