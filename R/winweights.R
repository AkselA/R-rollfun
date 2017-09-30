
winweights <- function(width=11, type="epanechnikov", step.adj=TRUE, lev.adj="sum", a=3) {

    if (width<2) stop("width must be 2 or larger", call.=FALSE)

    typenames <- c("squ", "square", "box", "boxcar", "triangle", "triangular", "epa",
      "epanechnikov", "parabola", "parabolic", "quartic", "biweight", "triweight",
      "tricube", "cosine", "sine", "optcosine", "cosine-smooth", "smooth cosine",
      "limit", "hann", "hamming", "blackman",  "nuttall", "blackman-nuttall",
      "blackman-harris", "flattop", "flat-top", "flat top",  "kaiser", "kaiser-bessel",
      "lanczos", "lanc", "sinc", "sine cardinal")
          
    type <- tolower(type)
      
    if (!type %in% typenames) stop("type not one of ", list(typenames), call.=FALSE)
    
    if (width%%1 != 0) {
      width <- ceiling(width)
      warning("width was rounded up to nearest integer", call.=FALSE)
    }

    s.adj <- 0
    if (step.adj) s.adj <- (1/width)
    
    s <- seq(-1+s.adj, 1-s.adj, length.out=width)
    n <- 0:(width-1)
    
    sinc <- function (x) {
	    y <- x
	    z <- x == 0
	    y[z] <- 1
	    y[!z] <- sin(pi * x[!z])/(pi * x[!z])
	    return(y)
	}

    # Square
    if (type %in% c("squ", "square", "box", "boxcar")) {
      kernel <- 1-abs(s)
    }
    
    # Triangular
    if (type %in% c("triangle", "triangular")) {
      kernel <- 1-abs(s)
    }
    
    # Epanechnikov
    if (type %in% c("epa", "epanechnikov", "parabola", "parabolic")) {
      kernel <- (3/4)*(1-s^2)
    }
    
    # Biweight
    if (type %in% c("quartic", "biweight")) {
      kernel <- (15/16)*(1-s^2)^2
    }
    
    # Triweight
    if (type %in% c("triweight")) {
      kernel <- (35/32)*(1-s^2)^3
    }
    
    # Tricube
    if (type %in% c("tricube")) {
      kernel <- (70/81)*(1-abs(s)^3)^3
    }
    
    # Cosine (R optcosine)
    if (type %in% c("cosine", "optcosine")) {
      kernel <- (pi/4)*cos(s*pi/2)
    }
    
    # Cosine (R cosine)
    if (type %in% c("cosine-smooth", "smooth cosine", "sine")) {
      kernel <- (1+cos(pi*s))/2
    }
    
    # Hann
    if (type %in% c("hann")) {
      kernel <-  0.5 * (1 - cos((2*pi*n)/(width-1)) )   
    }
    
    # Hamming
    if (type %in% c("hamming")) {
      kernel <-  0.54 - 0.46*cos((2*pi*n)/(width-1))
    }
    
    # Blackman
    if (type %in% c("blackman")) {
      kernel <-  0.42 - 0.50*cos((2*pi*n)/(width-1)) +
                        0.08*cos((4*pi*n)/(width-1))
    }
    
    # Nuttall
    if (type %in% c("nuttall")) {
      kernel <-  0.355768 - 0.487396*cos((2*pi*n)/(width-1)) +
                            0.144232*cos((4*pi*n)/(width-1)) -
                            0.012604*cos((6*pi*n)/(width-1))
    }
    
    # Blackman-Nuttall
    if (type %in% c("blackman-nuttall")) {
      kernel <-  0.3635819 - 0.4891775*cos((2*pi*n)/(width-1)) +
                             0.1365995*cos((4*pi*n)/(width-1)) -
                             0.0106411*cos((6*pi*n)/(width-1))
    }
    
    # Blackman-Harris
    if (type %in% c("blackman-harris")) {
      kernel <-  0.35875 - 0.48829*cos((2*pi*n)/(width-1)) +
                           0.14128*cos((4*pi*n)/(width-1)) -
                           0.01168*cos((6*pi*n)/(width-1))
    }
    
    # Flat top
    if (type %in% c("flattop", "flat-top", "flat top")) {
      kernel <-  1 - 1.930*cos((2*pi*n)/(width-1)) +
                     1.290*cos((4*pi*n)/(width-1)) -
                     0.388*cos((6*pi*n)/(width-1)) +
                     0.028*cos((6*pi*n)/(width-1))
    }
    
    # Kaiser
    if (type %in% c("kaiser", "kaiser-bessel")) {
      kernel <- besselI(pi*a*sqrt(1 - ( (2*n)/(width-1) - 1)^2), 0)/besselI(pi*a, 0)
    }
    
    # Lanczos
    if (type %in% c("lanczos", "lanc")) {
      kernel <- sin(pi*s*a)/(pi*s*a) * sin(pi*s)/(pi*s)
      kernel[is.na(kernel)] <- 1
    }
    
    # Sinc
    if (type %in% c("sinc", "sine cardinal")) {
      kernel <- sin(pi*s*a)/(pi*s*a)
      kernel[is.na(kernel)] <- 1
    }
    
    l.adj <- 1
    if (tolower(lev.adj)=="sum") l.adj <- sum(kernel)
    if (tolower(lev.adj)=="mean") l.adj <- mean(kernel)
    kernel/l.adj
}

# w <- 61

# plot(seq(-5, 5, length.out=w), winweights(width=w, type="lanc", a=5, lev.adj=""),
  # type="o", pch=16, cex=1.5, col=1, ann=FALSE, xaxt="n")
# axis(1, seq(-5, 5, by=1), )
# abline(h=0)
# points(seq(-4, 4, length.out=w*4/5), winweights(width=w*4/5, type="lanc", a=4, lev.adj=""),
  # type="o", pch=16, cex=1.5, col=2)
# points(seq(-3, 3, length.out=w*3/5), winweights(width=w*3/5, type="lanc", a=3, lev.adj=""),
  # type="o", pch=16, cex=1.5, col=3)
# points(seq(-2, 2, length.out=w*2/5), winweights(width=w*2/5, type="lanc", a=2, lev.adj=""),
  # type="o", pch=16, cex=1.5, col=4)
# points(seq(-1, 1, length.out=w*1/5), winweights(width=w*1/5, type="lanc", a=1, lev.adj=""),
  # type="o", pch=16, cex=1.5, col=5)

# w <- 10000
# plot(winweights(width=w, type="lanc", a=1, lev.adj=""), type="l", pch=16, cex=1.5)
# plot(winweights(width=w, type="sinc", a=1, lev.adj=""), type="l", pch=16, cex=1.5)

