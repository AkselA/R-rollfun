#' Rolling RMS
#'
#' Wrapper for \code{rollconv()} offering a quick rolling windowed RMS
#' 
#' @param x numeric vector; data
#' @param w integer or numeric vector; width or shape of the rolling RMS window
#' @param na.pad logical; should the ends be padded with \code{NA}?
#' @param B prime; smoothness as used by the FFT algorithm 
#'
#' @export
#' @examples
#' w <- 50
#' weights.para <- winweights(w, type="parabolic")
#' weights.hann <- winweights(w, type="hann")
#' 
#' plot(weights.hann, type="l", ann=FALSE)
#' lines(weights.para)
#' 
#' data(xyz2)
#' yy <- xyz2
#' 
#' plot(yy, col="grey", type="l")
#' lines(rollwinrms(yy, weights.hann, partial=TRUE), col="blue", lwd=2)
#' lines(rollwinrms(yy, weights.para, partial=TRUE), col="green", lwd=2)
#' lines(rollwinrms(yy, w, partial=TRUE), col="red", lwd=2)

rollwinrms <- function(x, w, ...) {
	sqrt(rollconv(x*x, w=w, ...))
}


