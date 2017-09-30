
rollfun <- function(x, w, FUN, ..., by=1, list.out=FALSE, simplify=TRUE) {
    w <- w %/% 2
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
    v
}

# # data(xyz)

# # regular rolling mean
# rollfun(xyz[1:15], 7, mean)

# # plot rolling 5-number statistic
# summ.df <- rollfun(xyz, 33, summary, list.out=TRUE)

# plot(xyz, pch=16, cex=0.8)
# matlines(summ.df[, -4], type="l", lty=1, lwd=1.5, col=rainbow(5, start=0.5, alpha=0.6))

# # rolling mean and 95% confidence intervals
# w <- 25
# rf <- rollfun(xyz, w, function(x) {
    # m <- mean(x)
    # s <- 1.96*sd(x)/sqrt(w)
       # c(mean=m,
        # ci.up=m+s, 
      # ci.down=m-s)}, 
    # list.out=TRUE)
    
# df2 <- data.frame(time=seq_along(xyz), xyz, rf)
# plot(df2[1:2], type="l")
# matlines(df2[,1], df2[,-(1:2)], col=c(1, 2, 3, 3), lty=1, lw=1.5)

# # rolling piecewise linear regression
# lm.fun <- function(x) {
	# lm(x ~ seq_along(x))
# }

# s <- 40
# models <- rollfun(xyz2, s, by=1, lm.fun, list.out=TRUE, simplify=FALSE)
# notna <- which(!is.na(models))

# plot(xyz2, pch=16, col="red")

# for (i in notna) {
	# vals <- fitted(models[[i]])
	# lines(seq_along(vals) + i - min(notna), vals, lwd=1.5,
	  # col="#00000040")
# }

# # rolling stepped piecewise polynomial regression
# plm.fun <- function(x, p=4) {
	# lm(x ~ poly(seq_along(x), p))
# }

# s <- 80
# models <- rollfun(xyz2, s, by=63, plm.fun, list.out=TRUE, simplify=FALSE)
# notna <- which(!is.na(models))

# plot(xyz2, pch=16, col="red")

# for (i in notna) {
	# vals <- fitted(models[[i]])
	# lines(seq_along(vals) + i - min(notna), vals, lwd=4,
	  # col="#000000A0")
# }

