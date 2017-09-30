
rollrms <- function(x, w=9, na.pad=TRUE) {
    x <- sqrt(TTR::runMean(x^2, w))
    x <- x[!is.na(x)]
    if (na.pad) {
	    NA1 <- rep(NA, floor((w-1)/2))
	    NA2 <- rep(NA, ceiling((w-1)/2))
	    c(NA1, x, NA2)
	} else x
}

set.seed(1)
r <- rnorm(100)
r.rms <- rollrms(r, w=20)
r.rms.m <- mean(r.rms, na.rm=TRUE)
plot(r)
lines(r.rms, col="red", lwd=1.2)
abline(h=r.rms.m, col="blue")
mtext(round(r.rms.m, 3),
  4, at=r.rms.m, las=1, cex=0.6, line=0.1)


v <- NULL
for (i in 4:50) {
    v[i] <- mean(rollrms(r, w=i, na.pad=FALSE))
}
plot(v)