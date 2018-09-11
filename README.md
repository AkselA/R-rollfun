# rollfun  

A collection of rolling functions.  

Among others:  
Exponential moving average, Iterative exponential moving average, Optimized windowed convolution filter, Arbitrary rolling function, Iterative rolling mean, Rolling RMS, Windowed rolling RMS.  

Also generate trapezoid windows and lot of other popular windows (epanechnikov, biweight, triweight, hann, hamming, blackmann, kaiser-bessel, poisson, lancsoz, ...)

```R
library(devtools)
install_github("AkselA/R-rollfun")
library(rollfun)

# rolliter() and iema() works well on data with high rate of missingness
set.seed(1)
x <- rnorm(150, 0, 20) + c(1:50, rep(50, 50), 50:1)
x2 <- x
x2[sample(2:149, 50)] <- NA

plot(x, col="lightgrey")
points(x2, pch=16, cex=0.3)

lines(iema(x2, 100, dir="av"), col="red")
lines(rolliter(x2, 6, 10, partial=TRUE), col="blue")
```