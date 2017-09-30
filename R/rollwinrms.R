
rollwinrms <- function(x, w, nf=4) {
	sqrt(rollconv(x*x, w=w, nf=nf))
}

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
