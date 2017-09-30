
rollwinrms <- function(x, w, nf=4) {
	sqrt(rollconv(x*x, w=w, nf=nf))
}

yy <- c(-0.024, -0.01, 0.011, 0.024, 0.026, 0.029, 0.033, 0.027, 0.014, 
	0.01, 0.013, 0.009, 0.002, 0.004, 0.011, 0.019, 0.028, 0.035, 
	0.037, 0.04, 0.048, 0.056, 0.056, 0.052, 0.053, 0.058, 0.062, 
	0.062, 0.06, 0.054, 0.045, 0.033, 0.022, 0.014, 0.017, 0.029, 
	0.037, 0.038, 0.04, 0.044, 0.035, 0.015, 0.004, 0.01, 0.015, 
	0.016, 0.018, 0.022, 0.024, 0.019, 0.014, 0.007, 0.005, 0.008, 
	0.013, 0.02, 0.032, 0.044, 0.042, 0.03, 0.023, 0.022, 0.018, 
	0.017, 0.028, 0.04, 0.04, 0.033, 0.028, 0.024, 0.024, 0.03, 0.037, 
	0.039, 0.042, 0.044, 0.035, 0.017, 0.002, 0, 0.008, 0.021, 0.027, 
	0.02, 0.004, -0.006, -0.007, -0.008, -0.012, -0.012, -0.009, 
	-0.011, -0.016, -0.019, -0.022, -0.028, -0.037, -0.037, -0.027, 
	-0.014)

# yy <- yy-(mean(yy)/2)
# plot(winweights(50, type="parabolic"), ylim=c(0, 0.04), type="l", ann=FALSE)
# lines(winweights(50, type="cosine"))
# lines(winweights(50, type="cosine-smooth"))
# lines(winweights(50, type="hann"))

# w <- 11
# weights.para <- winweights(w, type="parabolic")
# weights.cosi <- winweights(w, type="cosine")
# weights.cosm <- winweights(w, type="cosine-smooth")
# weights.hann <- winweights(w, type="hann")

# plot(yy, col="red", type="l")
# lines(rollwinrms(yy, w))
# lines(rollwinrms(yy, weights.para))
# lines(rollwinrms(yy, weights.cosi))
# lines(rollwinrms(yy, weights.cosm))
# lines(rollwinrms(yy, weights.hann))
