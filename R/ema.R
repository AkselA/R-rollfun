ema.na <- function (x, a=2) {
    x <- c(x[1], x)
    a <- 1/a
    for (i in 2:length(x)) {
        x[i] <- (1 - a) * x[i - 1] + a * x[i]
        if (is.na(x[i])) x[i] <- x[i - 1]
    }
    x[-1]
}

#' Exponential Moving Average
#' 
#' \code{ema} returns a vector whose values are the 
#' exponentially smoothed values of the input vector
#' 
#' @param x a numeric vector
#' @param a smoothing factor
#'
#' @details With \eqn{Y_t}{Y[t]} being the input at time \eqn{t},
#' \eqn{S_t}{S[t]} being the output at time \eqn{t}, and
#' \eqn{\alpha = 1/a} \cr – the function can be defined like this:
#' 
#' \deqn{S_t =\left\{\begin{array}{lr}
#' Y_1,                                           & t = 1 \\ 
#' \alpha \cdot Y_t + (1 - \alpha) \cdot S_{t-1}, & t > 1
#' \end{array}\right.}{
#'   S[t] = \{Y[1] for t = 1;
#'   \alpha * Y[t] + (1 - \alpha) * S[t-1] for t > 1\}}
#' 
#' The function is also sometimes referred to as an 
#' exponentially weighted moving average (EWMA).
#' 
#' @seealso \code{\link{dema}}, \code{\link{iema}}
#' 
#' @export
#' 
#' @examples
#' x <- c(rep(0, 4), 1, rep(0, 59))
#' a <- 1:10*5
#' col <- rainbow(length(a), start=0.15)
#' 
#' plot(x, type="p", pch=16, cex=0.4, ylim=c(0, 0.21), xaxs="i",
#'   main="Exponential Moving Average", xlab="Time", ylab="Magnitude")
#' for (i in seq_along(a)) {
#'     lines(ema(x, a[i]), col=col[i])
#' }
#' 
#' mtext("Single impulse of magnitude 1 occuring at time = 5", line=0.2)
#' L <- lapply(seq_along(a), function(x) bquote(alpha ==  1 / .(a[x])))
#' legend("topright", legend=as.expression(L),
#'   bty="n", col=col, lwd=1.2, cex=0.8, y.intersp=1.2)

ema <- function(x, a=2) {
	if (any(is.na(x))) {
		ema.na(x, a)
	} else {
		ar <- round(a)
		TTR::EMA(c(rep(x[1], ar), x), a, wilder=TRUE)[-(1:ar)]
	}
}

#' Double Exponential Moving Average
#' 
#' \code{dema} performs exponential smoothing twice on the same vector. \cr
#' This is not the same as the DEMA typically used in technical trading 
#' (eg. \code{\link{TTR::DEMA}}).
#' 
#' @param x a numeric vector
#' @param a numeric; smoothing factor
#' @param direction character; should the two moving averages be run
#' \itemize{
#'   \item{\code{ff}: forwards both times}
#'   \item{\code{fb}: forwards then backwards}
#'   \item{\code{bf}: backwards then forwards}
#'   \item{\code{av}: both \code{fb} and \code{bf}, returning the average}
#' }
#' 
#' @details The moving averages are done as in \code{ema}, except that 
#' \code{a} is square-rooted, to make the outputs more comparable.
#' 
#' @seealso \code{\link{ema}}, \code{\link{iema}}
#' 
#' @export
#' 
#' @examples
#' s <- c(0, 0, 1, NA, 2, 0, 1, 1, NA, NA, 2, NA, 3, 1, 2, 1, 0, 1, NA, 0, 0, 0)
#' plot(s, type="b", col="#00000088")
#' lines(dema(s, 2, dir="ff"), col=2)
#' lines(dema(s, 2, dir="fb"), col=3)
#' lines(dema(s, 2, dir="bf"), col=4)
#' lines(dema(s, 2, dir="av"), col=5)
 
dema <- function(x, a=2, direction=c("ff", "fb", "bf", "av")) {
	dir <- match.arg(direction)
	a <- sqrt(a)
	switch(dir,
	       ff = ema(ema(x, a), a),
	       fb = rev(ema(rev(ema(x,  a)), a)),
	       bf = ema(rev(ema(rev(x), a)), a),
	       av = (rev(ema(rev(ema(x,  a)), a)) + 
	             ema(rev(ema(rev(x), a)), a)) / 2
	       )
}

#' Iterated Exponential Moving Average
#' 
#' \code{iema} returns a vector whose values are the 
#' iteratively exponentially smoothed values of an input vector.
#' 
#' @param x a numeric vector
#' @param a numeric; smoothing factor
#' @param iter integer; number of iterations
#' @param direction character; should the moving averages be run
#' \itemize{
#'   \item{\code{ff}: forwards only}
#'   \item{\code{fb}: forwards then backwards then forwards \dots}
#'   \item{\code{bf}: backwards then forwards then backwards \dots}
#'   \item{\code{av}: both \code{fb} and \code{bf}, returning the average}
#' }
#' 
#' @details \code{iema} is a generalization of \code{dema} where the
#' exponential moving average can be applied to the vector an arbitrary number
#' of times. Just as the square root of \code{a} is used in \code{dema},
#' here the \code{iter}'th root of \code{a} is used for each iteration.
#' 
#' @seealso \code{\link{ema}}, \code{\link{dema}}, \code{\link{rolliter}}
#' 
#' @export
#' 
#' @examples
#' s <- c(0, 0, 1, NA, 2, 0, 1, 1, NA, NA, 2, NA, 3, 1, 2, 1, 0, 1, NA, 0, 0, 0)
#' plot(s, type="b", col="#00000088")
#' lines(iema(s, 2, dir="ff"), col=2)
#' lines(iema(s, 2, dir="fb"), col=3)
#' lines(iema(s, 2, dir="bf"), col=4)
#' lines(iema(s, 2, dir="av"), col=5)

iema <- function(x, a=2, iter=8, direction=c("ff", "fb", "bf", "av")) {
	dir <- match.arg(direction)
    formals(ema)$a <- a^(1/iter)
    Funcall <- function(f, ...) f(...)
	r <- switch(dir,
        ff = Reduce(Funcall, rep(c(ema), iter), x, right=TRUE),
        fb = Reduce(Funcall, rep(c(rev, ema), iter), x, right=TRUE),
        bf = Reduce(Funcall, rep(c(ema, rev), iter), x, right=TRUE),
        av = (Reduce(Funcall, rep(c(rev, ema), iter), x, right=TRUE) +
              Reduce(Funcall, rep(c(ema, rev), iter), x, right=TRUE)) / 2
        )
    if (iter %% 2 == 1 & dir != "ff") {
    	rev(r)
    } else {
    	r
    }
}
