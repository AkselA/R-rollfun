
cummean <- function(x) {
	cumsum(x)/seq_along(x)
}

rollmeanp <- function(x=1:8, n=5) {
	mn <- TTR::runMean(x, n)
	mn <- mn[!is.na(mn)]
    
    f.len <- floor((n-1) / 2)
    b.len <- ceiling((n-1) / 2)
        
    front <- tail(cummean(head(x, n-1)), f.len)
    back <- rev(tail(cummean(rev(tail(x, n-1))), b.len))

	c(front, mn, back)
}

rolliter <- function(x, w=9, iter=5, partial=FALSE, sharp=FALSE) {	
	w <- as.integer(round(w))
    lx <- length(x)
    
    if (!partial) {    	
	    i <- 1
		while (i <= iter & w > 1) {
		    x <- TTR::runMean(x, w)
		    i <- i + 1
		    w <- w - sharp
		}
	    x <- na.omit(x)
		ln <- lx - length(x)
		c(rep(NA, floor(ln / 2)), x, rep(NA, ceiling(ln / 2)))
	
	} else {		
		i <- 1
		while (i <= iter & w > 1) {
		    x <- rollmeanp(x, w)
		    i <- i + 1
		    w <- w - sharp
		}
        x
	}	
}

# set.seed(1)
# r <- rnorm(50)
# w <- 5
# plot(r, type="p", cex=0.5, pch=16)
# points(rolliter(r, w, 2, partial=TRUE), type="o", col="orange", cex=0.5, pch=16)
# points(rolliter(r, w, 3, partial=TRUE), type="o", col="red", cex=0.5, pch=16)
# points(rolliter(r, w, 4, partial=TRUE), type="o", col="purple", cex=0.5, pch=16)
# points(rolliter(r, w, 5, partial=TRUE), type="o", col="blue", cex=0.5, pch=16)

# y <- c(rep(1, 16), 2, rep(1, 16))
# m1 <- rolliter(y, 3, 6)
# m2 <- rolliter(y, 2, 5)

# plot(y)
# lines(m1)
# lines(m2)
# abline(v=which(y == 2))


# y <- c(rep(1, 30), 2, rep(1, 30))
# m1 <- rolliter(y, 9, 2)
# m2 <- rolliter(y, 9, 3)
# m3 <- rolliter(y, 9, 4)
# m4 <- rolliter(y, 9, 5)

# plot(m1, col="red", lwd=2, type="l")
# lines(m2, col="blue", lwd=2)
# lines(m3, col="green", lwd=2)
# lines(m4, col="orange", lwd=2)


# y <- c(rep(1, 30), 2, rep(1, 30))
# m1 <- rolliter(y, 10, 5, FALSE)
# m2 <- rolliter(y, 10, 5, TRUE)
# m3 <- rolliter(y, 9, 5, FALSE)

# plot(m1, col="red", lwd=2, type="l", ylim=c(1, 1.1))
# lines(m2, col="blue", lwd=2)
# lines(m3, col="green", lwd=2)

## sharp=TRUE can in certain cases smooth more while 
## dropping fewer data points at the ends.
# x <- c(rep(0, 11), 1, 0, 0, 0, 0, 1, rep(0, 11))
# r1 <- rolliter(x, 5, iter=6, sharp=TRUE)
# r2 <- rolliter(x, 3, iter=6, sharp=FALSE)
# plot(r1, col="red", type="o", pch=16, cex=0.5)
# points(r2, col="blue", type="o", pch=16, cex=0.5)