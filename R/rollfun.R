#' Apply Rolling Function
#'
#' Apply a rolling function to the margins of data. \cr
#' 
#' @param x vector, matrix or data.frame; data representing a series \cr
#'   of observations
#' @param w single integer; width of the rolling window
#' @param FUN closure; function to be applied
#' @param ... optional arguments passed to FUN
#' @param by single integer; step length of the window
#' @param front logical; when \code{w} is odd, should the result be biased to the front?
#' @param list.out logical; should output be returned as list?
#' @param simplify logical; should output be simplified? \cr
#'   Typically from list to data.frame or matrix
#' @param partial logical; should partial results at the ends be calculated?
#' 
#' @details
#' \code{rollfun()} is functionally very similar to \code{zoo::rollapply()}, but slightly simplified. \cr
#' \code{w} and \code{by} can only be single positive integers, meaning that window width and step length \cr
#' are always constant. When applied to a multi column object all columns will be passed to \cr
#' \code{FUN}, unless otherwise stated in \code{FUN}. Output can be forced to list format with \cr
#' \code{list.out=TRUE}. If output from \code{FUN} is anything other than scalar, \code{list.out} must be set to \cr
#' \code{TRUE}. \code{simplify=TRUE} will apply \code{do.call(rbind, .)} to the output, meaning that the \cr
#' 'simplest' it can get is a one column matrix. \code{simplify} is ignored if \code{list.out=TRUE}. \cr
#' Output will always be the same length (or nrow) as input. Ends are padded with \code{NA}, except \cr
#' when \code{partial=TRUE}. When \code{by > 1} both ends and empty intervals will be padded with \code{NA}. \cr
#' 
#' @export
#' @examples
#' # rolling median absolute deviation (mad)
#' rollfun(rnorm(15), 7, mad, constant=1)
#' 
#' 
#' # rolling mad vs rolling sd
#' set.seed(1)
#' r <- rnorm(500, 0, sin(1:500/50) + 2) * rexp(500)
#' f <- function(x) c(mad(x), sd(x) * -1)
#' 
#' plot(r, pch=16, cex=0.5)
#' for (i in 1:25) {
#' 	rf <- rollfun(r, i*5, f, list.out=TRUE, simplify=TRUE, partial=TRUE)
#'     matlines(rf, col=c("#FF000066", "#0000FF66"), lty=1)
#' }
#' 
#' 
#' # plot rolling 5-number statistic
#' data(xyz)
#' summ.df <- rollfun(xyz, 33, summary, list.out=TRUE, simplify=TRUE)
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
#' r <- rollfun(xr, 150, list.out=TRUE, partial=TRUE, simplify=TRUE,
#'   function(z) quantile(z, probs, names=FALSE))
#' 
#' cf <- colorRampPalette(c("#00FF44", "cyan", "#0044FF"), space="Lab", bias=0.8)
#' matplot(r, type="l", lty=1, lwd=0.4+(0.0005*(-50:50)^2),
#'   col=cf(101), xaxs="i", yaxs="i")
#' box()
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
#' 
#' 
#' ### demonstrating output formats
#' # when FUN output is scalar
#' rollfun(1:9, 3, mean)
#' 
#' # setting list.out=TRUE makes it a list of scalars
#' rollfun(1:9, 3, mean, list.out=TRUE)
#' 
#' # setting list.out=TRUE and simplify=TRUE returns a single column matrix
#' rollfun(1:9, 3, mean, list.out=TRUE, simplify=TRUE)
#' 
#' # when FUN output is a vector
#' # the following will fail as output is not scalar
#' rollfun(1:9, 3, function(x) c(x[1], x[2]))
#' 
#' # setting list.out=TRUE makes it work
#' rollfun(1:9, 3, function(x) c(x[1], x[2]), list.out=TRUE)
#' 
#' # setting list.out=TRUE and simplify=TRUE returns a matrix
#' rollfun(1:9, 3, function(x) c(x[1], x[2]), list.out=TRUE, simplify=TRUE)
#' 
#' # when FUN output is a list
#' # setting list.out=TRUE makes it work
#' rollfun(1:9, 3, function(x) list(x[1], x[2]), list.out=TRUE)
#' 
#' # setting list.out=TRUE and simplify=TRUE returns a matrix
#' rollfun(1:9, 3, function(x) list(x[1], x[2]), list.out=TRUE, simplify=TRUE)
#' 

rollfun <- function(x, w, FUN, ..., by=1, front=TRUE,
  list.out=FALSE, simplify=FALSE, partial=FALSE) {
  	
  	if (partial & by > 1) {
  		partial <- FALSE
  		warning(paste("  partial set to FALSE.", 
  		              "partial=TRUE and by > 1 don't mix",
  		              "(at the present)."),
  		              call.=FALSE)
  	}
  	
  	even <- (w+1) %% 2
    w <- w %/% 2
    lenx <- NROW(x)

    if (front) {
        z <- seq(w+1-even, lenx-w, by=by)
        index <- function() (i - (w-even)):(w + i)
    } else {
        z <- seq(w+1, lenx-w+even, by=by)
        index <- function() (i - w):(w + i - even)	
    }
    
    if (NCOL(x) == 1) {
	    if (list.out) {
	        v <- as.list(rep(NA, lenx))
	        for (i in z) {
		        v[[i]] <- FUN(x[index()], ...)
	        }
            if (partial) {
		        ip <- which(is.na(v))
		        for (i in ip) {
		        	ix <- index()
		        	ix <- ix[ix > 0 & ix <= lenx]
			        v[[i]] <- FUN(x[ix], ...)
		        }
	        }
	        if (simplify) v <- do.call(rbind, v)
	    } else {
	        v <- rep(NA, lenx)
	        for (i in z) {
		        v[i] <- FUN(x[index()], ...)
	        }
            if (partial) {
		        ip <- which(is.na(v))
		        for (i in ip) {
		        	ix <- index()
		        	ix <- ix[ix > 0 & ix <= lenx]
			        v[i] <- FUN(x[ix], ...)
		        }
	        }
	    }    
	}
	
    if (length(dim(x)) == 2) {
	    if (list.out) {
	        v <- as.list(rep(NA, lenx))
	        for (i in z) {
		        v[[i]] <- FUN(x[index(), ], ...)
	        }
            if (partial) {
		        ip <- which(is.na(v))
		        for (i in ip) {
		        	ix <- index()
		        	ix <- ix[ix > 0 & ix <= lenx]
			        v[[i]] <- FUN(x[ix, ], ...)
		        }
	        }
	        if (simplify) v <- do.call(rbind, v)
	    } else {
	        v <- rep(NA, lenx)
	        for (i in z) {
		        v[i] <- FUN(x[index(), ], ...)
	        }
            if (partial) {
		        ip <- which(is.na(v))
		        for (i in ip) {
		        	ix <- index()
		        	ix <- ix[ix > 0 & ix <= lenx]
			        v[i] <- FUN(x[ix, ], ...)
		        }
	        }
	    }
    }
    v
}

