#' Rolling RMS
#'
#' Wrapper for \code{TTR::runMean()} offering a quick rolling RMS
#' 
#' @param x numeric vector; data
#' @param w integer; width of the rolling RMS window
#' @param na.pad logical; should the ends be padded with \code{NA}?
#' @param partial logical; should partial results be returned?
#'
#' @export
#' @examples
#' data(xyz2)
#' r <- xyz2
#' r.rms <- rollrms(r, w=25, partial=TRUE)
#' r.rms.m <- mean(r.rms, na.rm=TRUE)
#' plot(r, main="Rolling RMS")
#' lines(r.rms, col="red", lwd=1.2)
#' abline(h=r.rms.m, col="blue")
#' mtext(round(r.rms.m, 3),
#'   4, at=r.rms.m, las=1, cex=0.6, line=0.1)

rollrms <- function(x, w, na.pad=TRUE, partial=FALSE) {
	if (!partial) {
	    x <- sqrt(TTR::runMean(x^2, w))
	    x <- x[!is.na(x)]
	    if (na.pad) {
		    NA1 <- rep(NA, floor((w-1)/2))
		    NA2 <- rep(NA, ceiling((w-1)/2))
		    c(NA1, x, NA2)
		} else x
    } else {
    	sqrt(rollmeanp(x^2, w))
    }
}

