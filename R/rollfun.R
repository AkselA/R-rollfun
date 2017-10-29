################################################################################
#' Apply Rolling Function
#'
#' Apply a rolling function to the margins of data. \cr
#' functionally very similar to zoo::rollapply()
#' 
#' @param x vector, matrix or data.frame; data representing a series \cr
#'   of observations
#' @param w single integer; width of the rolling window
#' @param FUN closure; function to be applied
#' @param ... optional arguments passed to FUN
#' @param by single integer; step length of the window
#' @param list.out logical; should output be returned as list?
#' @param simplify logical; should output be simplified? \cr
#'   Typically from list to data.frame or matrix
#' 
#' @export
#' @examples
#' # rolling median absolute deviation (mad)
#' rollfun(rnorm(15), 7, mad, constant=1)
#' 
#' 
#' # rolling mad vs rolling sd
#' set.seed(1)
#' r <- rnorm(200, 0, sin(1:200/20) + 2)
#' plot(r)
#' for (i in 1:10) {
#'     lines(rollfun(r, i*2, mad), col="#FF000088")
#'     lines(rollfun(r, i*2, sd) * -1, col="#0000FF88")
#' }
#' 
#' 
#' # plot rolling 5-number statistic
#' data(xyz)
#' summ.df <- rollfun(xyz, 33, summary, list.out=TRUE)
#' 
#' plot(xyz, pch=16, cex=0.4)
#' matlines(summ.df[, -4], type="l", lty=1, lwd=2, 
#'   col=rainbow(5, start=0.5, alpha=0.8))
#' 
#' 
#' # rolling percentiles
#' set.seed(1)
#' s1 <- c(seq(2.5, 0.5, , 1000), rep(0.5, 500))
#' s2 <- sin(1:1500/1500*2*pi) + 1.5
#' xr <- rbeta(length(s1), s1, s2)
#'  
#' probs <- seq(0, 1, , 101)
#' r <- rollfun(xr, 150, list.out=TRUE,
#'   function(z) quantile(z, probs, names=FALSE))
#'
#' matplot(r, type="l", lty=1, lwd=0.6, col=rainbow(ncol(r), start=0.05))
#' 
#' 
#' # rolling mean and pseudo 95% confidence intervals
#' meanAndError <- function(x) {
#'     m <- mean(x)
#'     s <- 1.96*sd(x)/sqrt(w)
#'        c(mean=m,
#'         ci.up=m+s, 
#'       ci.down=m-s)
#' }
#' 
#' set.seed(1)
#' w <- 25
#' l <- 250
#' x <- rnorm(l, 0, seq(1, 20, length.out=l))
#' rf <- rollfun(x, w, meanAndError, list.out=TRUE)
#'     
#' df2 <- data.frame(time=seq_along(x), x, rf)
#' plot(df2[1:2], type="l", lwd=0.5)
#' matlines(df2[,1], df2[,-(1:2)], col=c(1, 2, 3, 3), lty=1, lw=1.5)
#' 
#' 
#' # rolling piecewise linear regression
#' lm.fun <- function(x) {
#' 	lm(x ~ seq_along(x))
#' }
#' 
#' data(xyz2)
#' models <- rollfun(xyz2, 40, by=1, lm.fun, list.out=TRUE, simplify=FALSE)
#' notna <- which(!is.na(models))
#' 
#' plot(xyz2, pch=16, col="red")
#' 
#' for (i in notna) {
#' 	vals <- fitted(models[[i]])
#' 	lines(seq_along(vals) + i - min(notna), vals, lwd=1.5,
#' 	  col="#00000040")
#' }
#' 
#' 
#' # rolling stepped piecewise polynomial regression
#' plm.fun <- function(x, p=4) {
#' 	lm(x ~ poly(seq_along(x), p))
#' }
#' 
#' s <- 80
#' models <- rollfun(xyz2, s, by=63, plm.fun, list.out=TRUE, simplify=FALSE)
#' notna <- which(!is.na(models))
#' 
#' plot(xyz2, pch=16, col="red")
#' 
#' for (i in notna) {
#' 	vals <- fitted(models[[i]])
#' 	lines(seq_along(vals) + i - min(notna), vals, lwd=4,
#' 	  col="#000000A0")
#' }
#' 
#' 
#' # rolling linear regression, plotting slope, intercept and p-value
#' set.seed(1)
#' s1 <- sin((1:300/100)*pi*2) + 
#'   rnorm(300, 0, seq(0.1, 1, length.out=300))
#' 
#' s2 <- sin((1:300/50)*pi*2) + 
#'   rnorm(300) +
#'   tanh(s1/3)
#' 
#' s2 <- ema(s2, 20)
#'   
#' dtf <- data.frame(s2, s1)
#' 
#' lm.fun <- function(x) {
#'     vn <- colnames(x)
#'     form <- as.formula(paste(vn[1], "~", vn[2]))
#' 	mod <- as.data.frame(summary(lm(form, data=x))$coefficients)
#' 	num <- c(mod$Estimate, mod$"Pr(>|t|)")
#' 	names(num) <- rownames(mod)
#' 	num
#' }
#' 
#' models <- rollfun(dtf, 25, lm.fun, list.out=TRUE, simplify=TRUE)
#' plot(as.ts(models), nc=2, mar.multi=c(0, 4.1, 0, 1))

rollfun <- function(x, w, FUN, ..., by=1, list.out=FALSE, simplify=TRUE) {
    w <- w %/% 2
    
    if (ncol(x) == 1 || is.null(dim(x))) {
        z <- seq(w+1, length(x)-w, by=by)
	    if (list.out) {
	        v <- as.list(rep(NA, length(x)))
	        for (i in z) {
		        v[[i]] <- FUN(x[(i - w):(i + w)], ...)
	        }
	        if (simplify) v <- do.call(rbind, v)
	    } else {
	        v <- rep(NA, length(x))
	        for (i in z) {
		        v[i] <- FUN(x[(i - w):(i + w)], ...)
	        }
	    }
	}
	
    if (length(dim(x)) == 2) {
        z <- seq(w+1, nrow(x)-w, by=by)
	    if (list.out) {
	        v <- as.list(rep(NA, nrow(x)))
	        for (i in z) {
		        v[[i]] <- FUN(x[(i - w):(i + w), ], ...)
	        }
	        if (simplify) v <- do.call(rbind, v)
	    } else {
	        v <- rep(NA, nrow(x))
	        for (i in z) {
		        v[i] <- FUN(x[(i - w):(i + w), ], ...)
	        }
	    }
    }
    v
}



