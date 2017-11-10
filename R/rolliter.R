
cummean <- function(x) {
	cumsum(x)/seq_along(x)
}

#' Rolling Mean
#'
#' Rolling mean that includes partial results from the beginning and end 
#' 
#' @param x numeric; data vector
#' @param w integer; width of the rolling window
#' @param front logical; when \code{w} is odd, should it be biased to the front?
#' 
#' @details
#' Wrapper/extension for \code{TTR::runMean()}
#' @export
#' @examples
#' xx <- c(3, 8, 5, 9, 6, 3, 8, 2, 5, 1)
#' plot(xx)
#' lines(rollmeanp(xx, w=4, front=TRUE), col="blue")
#' lines(rollmeanp(xx, w=4, front=FALSE), col="red")
#'  

rollmeanp <- function(x, w=5, front=TRUE) {
	mn <- TTR::runMean(x, w)
	mn <- mn[!is.na(mn)]
	
	s <- c(0, 2) + c(front, -front)
    
	f.len <- floor((w-s[1]) / 2)
    b.len <- ceiling((w-s[2]) / 2)
    
    front <- tail(cummean(head(x, w-1)), f.len)
    back <- rev(tail(cummean(rev(tail(x, w-1))), b.len))

	c(front, mn, back)
}

#' Apply Rolling Function
#'
#' Apply a rolling function to the margins of data
#' 
#' @param x numeric; data vector
#' @param w integer; width of the rolling window
#' @param partial logical; should partial results at the beginning/end be calculated?
#' @param sharp logical; experimental option. Should window width be reduced by one \cr
#' for each iteration?
#' 
#' @export
#' @examples
#' # Increasing the number of iterations progressively reduces the ringing
#' set.seed(1)
#' l <- 50
#' r <- rnorm(l) + rpois(l, (1:l/10))
#' w <- 6
#' plot(r, type="p", cex=0.5, pch=16)
#' points(rolliter(r, w, 1, partial=TRUE), type="o", col="orange", cex=0.5, pch=16)
#' points(rolliter(r, w, 2, partial=TRUE), type="o", col="hotpink", cex=0.5, pch=16)
#' points(rolliter(r, w, 3, partial=TRUE), type="o", col="purple", cex=0.5, pch=16)
#' points(rolliter(r, w, 4, partial=TRUE), type="o", col="blue", cex=0.5, pch=16)
#' 
#' 
#' # When w is even and i is odd the smoothed vector is shifted half a position
#' # to the left relative to the input vector
#' y <- c(rep(0, 18), 1, rep(0, 18))
#' col <- rainbow(20, start=0.2)
#' plot(y, ylim=c(0, 0.25))
#' 
#' for (i in 1:length(col)) {
#'     lines(rolliter(y, 4, i, partial=TRUE), col=col[i])
#' }
#' 
#' 
#' # Using the experimental sharp=TRUE appears to give a sharper transition while
#' # still keeping high frequency ringing at a low level
#' par(mfcol=c(4, 2), mar=c(2, 2, 1, 1), oma=c(0, 0, 0.5, 0))
#' set.seed(1)
#' r <- tanh(rnorm(2e4, 0, 0.4))
#' w <- 11
#' it <- c(2, 3, 6, 9)
#' s <- expand.grid(it=it, sharp=c(FALSE, TRUE))
#' 
#' for (i in 1:nrow(s)) {
#' 	ri <- rolliter(r, w, s$it[i], partial=TRUE, sharp=s$sharp[i])
#' 	spectrum(na.omit(ri), xlim=c(0, 0.5), ylim=c(1e-20, 1), xaxs="i",
#' 	  main=ifelse(i %% 4 == 1, paste0("sharp=", s$sharp[i]), ""))
#' 	grid(col="#00000066")
#' 	text(0.005, 1e-20, paste0("w = ", w, "\n", "i = ", s$it[i]), adj=c(0, 0))
#' }
#'  

rolliter <- function(x, w=5, iter=4, partial=FALSE, sharp=FALSE) {	
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
		    x <- rollmeanp(x, w, front=(i %% 2) * !sharp)
		    i <- i + 1
		    w <- w - sharp
		}
        x
	}	
}
