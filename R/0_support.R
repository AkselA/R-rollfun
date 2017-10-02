norma <- function(W, c=0, r=2) {
    (W - min(W, na.rm=TRUE)) * (r/(max(W, na.rm=TRUE) - min(W, na.rm=TRUE))) - r/2 + c
}

fitrange <- function(W, lower=-1, upper=1) {
	if(lower>upper) warning("upper bound must be strictly larger than lower bound")
	newrange <- upper - lower
	oldrange <- max(W, na.rm=TRUE) - min(W, na.rm=TRUE)
	(W - min(W, na.rm=TRUE)) * (newrange/oldrange) + lower
}

x <- -3:3
fade <- function(x, fin=c(0, 10, 15), fout=fin) {
	lx <- length(x)
	fin_seq   <- rolliter(c(rep(fin[1], fin[2]), 
	                       rep(1, lx-fin[2])), 
                         fin[3], 5, TRUE, FALSE)
    fin_seq   <- fitrange(fin_seq, fin[1], 1)

	fout_seq <- rolliter(c(rep(1, lx-fout[2]), 
	                       rep(fout[1], fout[2])), 
                         fout[3], 5, TRUE, FALSE)
    fout_seq <- fitrange(fout_seq, fout[1], 1)
    
    x * fout_seq * fin_seq
}

set.seed(1)
r <- rnorm(200)
plot(r, type="l")
lines(fade(r), col="red", lwd=1.5)