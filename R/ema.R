
ema <- function(x, a) {
    x <- c(x[1], x)
    a <- 1/a
    for (i in 2:length(x)) {
        x[i] <- (1-a)*x[i-1] + a*x[i]
    }
    x[-1]
}

# x <- c(1, 6, 9, 7, 5, 9, 7, 2, 4)
# x <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
# ema(x, 5)