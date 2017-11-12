#' @export
winweights <- function(width=11, type="epanechnikov", step.adj=TRUE, lev.adj="sum", a=3) {

    if (width<2) stop("width must be 2 or larger", call.=FALSE)

    typenames <- c("square", "boxcar", "triangle", "triangular",
      "epanechnikov", "parabola", "parabolic", "quartic", "biweight", "triweight",
      "tricube", "cosine", "sine", "optcosine", "cosine-smooth",
      "hann", "hamming", "blackman",  "nuttall", "blackman-nuttall",
      "blackman-harris", "flattop", "flat-top", "flat top",  "kaiser", "kaiser-bessel",
      "lanczos", "sinc")
          
    type <- match.arg(tolower(type), typenames)
    
    if (width %% 1 != 0) {
      width <- ceiling(width)
      warning("width was rounded up to nearest integer", call.=FALSE)
    }
        
    s.adj <- 0
    if (step.adj) s.adj <- (1/width)
    
    s <- seq(-1+s.adj, 1-s.adj, length.out=width)
    n <- 0:(width-1)
    
    kernel <- switch(type,
                     "square"=,
                     "boxcar"=rep(1, width)/width,
                     "triangle"=,
                     "triangular"=1-abs(s),
                     "epanechnikov"=,
                     "parabola"=,
                     "parabolic"=(3/4)*(1-s^2),
                     "quartic"=,
                     "biweight"=(15/16)*(1-s^2)^2,
                     "triweight"=(35/32)*(1-s^2)^3,
                     "tricube"=(70/81)*(1-abs(s)^3)^3,
                     "cosine-smooth"=,
                     "cosine"=(1+cos(pi*s))/2,
                     "sine"=,
                     "optcosine"=(pi/4)*cos(s*pi/2),
                     "hann"=(sin((pi*n)/(width-1)))^2,
                     "hamming"=0.53836 - 0.46164*cos((2*pi*n)/(width-1)),
                     "blackman"=0.42 - 0.50*cos((2*pi*n)/(width-1)) +
                                       0.08*cos((4*pi*n)/(width-1)),
                     "nuttall"=0.355768 - 0.487396*cos((2*pi*n)/(width-1)) +
                                          0.144232*cos((4*pi*n)/(width-1)) -
                                          0.012604*cos((6*pi*n)/(width-1)),
                     "blackman-nuttall"=0.3635819 - 0.4891775*cos((2*pi*n)/(width-1)) +
                                                    0.1365995*cos((4*pi*n)/(width-1)) -
                                                    0.0106411*cos((6*pi*n)/(width-1)),
                     "blackman-harris"=0.35875 - 0.48829*cos((2*pi*n)/(width-1)) +
                                                 0.14128*cos((4*pi*n)/(width-1)) -
                                                 0.01168*cos((6*pi*n)/(width-1)),
                     "flat-top"=1 - 1.930*cos((2*pi*n)/(width-1)) +
                                   1.290*cos((4*pi*n)/(width-1)) -
                                   0.388*cos((6*pi*n)/(width-1)) +
                                   0.028*cos((6*pi*n)/(width-1)),
                     "kaiser"=,
                     "kaiser-bessel"=besselI(pi *
                                             a  * 
                                             sqrt(1 - ( (2*n)/(width-1) - 1)^2), 0) / 
                                             besselI(pi*a, 0),
                     "lanczos"={ kernel <- sin(pi*s*a)/(pi*s*a) * sin(pi*s)/(pi*s)
                                 kernel[is.na(kernel)] <- 1
                                 kernel },
                     "sinc"={ kernel <- sin(pi*s*a)/(pi*s*a)
                              kernel[is.na(kernel)] <- 1
                              kernel } )
    
    l.adj <- 1
    if (tolower(lev.adj)=="sum") l.adj <- sum(kernel)
    if (tolower(lev.adj)=="mean") l.adj <- mean(kernel)
    if (tolower(lev.adj)=="max") l.adj <- max(kernel)
    kernel/l.adj
}


# name <- c("square", "triangular",
          # "epanechnikov", "biweight", "triweight",
          # "tricube", "cosine-smooth", "optcosine",
          # "hann", "hamming", "blackman", "nuttall", "blackman-nuttall",
          # "blackman-harris", "flat-top", "kaiser-bessel",
          # "lanczos", "sinc")

# w <- 99
# par(mfcol=c(6, 3), mar=c(1.5, 1.5, 0.5, 0.5), mgp=c(0, 0.6, 0))
# for (i in name) {
	# plot(winweights(w, type=i, a=3), type="l", xlab="", ylab="", ylim=c(-0.02, 0.06))
	# grid(col="#00000022", lty=1)
	# legend("topright", legend=i, bty="n", text.col="blue", adj=c(0, 0))
# }

# set.seed(1)
# w <- 99
# x <- rnorm(2e4)
# i <- 1
# par(mfcol=c(6, 3), mar=c(1.5, 1.5, 0.5, 0.5), mgp=c(0, 0.6, 0))
# for (i in 1:length(name)) {
	# win <- winweights(w, type=name[i], step.adj=TRUE, lev.adj="mean", a=3)
	# rol <- rollconv(x, win, partial=TRUE, scale.window=FALSE)
	# spectrum(rol, main="", xlab="", ylab="", sub="", lwd=0.2, ylim=c(1e-9, 1e5))
	# grid(col="#00000022", lty=1)
	# legend("topright", legend=name[i], bty="n", text.col="blue", adj=c(0.2, 0))
# }
