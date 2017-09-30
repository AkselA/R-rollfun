trapezwin <- function(l=60, prop=0.5, l.slopes) {
	# creates an isosceles trapezoid window
	# with 'prop' giving the proportion of the
	# width covered by the slopes. 
	# prop=0 => rectangle
	# prop=1 => triangle
	if (missing(l.slopes)) {
		l.slopes <- round(l*prop/2)
	}
	l.flat <- l-2*l.slopes
	if (l.flat < 0) stop("slopes are too long")
	slopes <- (1:l.slopes)/(l.slopes+1)
	flat <- rep(1, l.flat)
	trapez <- c(slopes, flat, rev(slopes))
	trapez/sum(trapez)
}

plot(trapezwin(prop=0.10))