% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ema.R
\name{iema}
\alias{iema}
\title{Iterated Exponential Moving Average}
\usage{
iema(x, a = 2, iter = 8, direction = c("ff", "fb", "bf", "av"))
}
\arguments{
\item{x}{a numeric vector}

\item{a}{numeric; smoothing factor}

\item{iter}{integer; number of iterations}

\item{direction}{character; should the moving averages be run
\itemize{
  \item{\code{ff}: forwards only}
  \item{\code{fb}: forwards then backwards then forwards \dots}
  \item{\code{bf}: backwards then forwards then backwards \dots}
  \item{\code{av}: both \code{fb} and \code{bf}, returning the average}
}}
}
\description{
\code{iema} returns a vector whose values are the 
iteratively exponentially smoothed values of an input vector.
}
\details{
\code{iema} is a generalization of \code{dema} where the
exponential moving average can be applied to the vector an arbitrary number
of times. Just as the root-two'th root of \code{a} is used in \code{dema},
here the root-\code{iter}'th root of \code{a} is used for each
iteration. \cr
\cr
That is \cr

\deqn{a^* = a^{\frac{1}{\sqrt{iter}}}}{a* = a^(1/sqrt(iter))}
}
\examples{
s <- c(0, 0, 1, NA, 2, 0, 1, 1, NA, NA, 2, NA, 3, 1, 2, 1, 0, 1, NA, 0, 0, 0)
plot(s, type="b", col="#00000088")
lines(iema(s, 2, dir="ff"), col=2)
lines(iema(s, 2, dir="fb"), col=3)
lines(iema(s, 2, dir="bf"), col=4)
lines(iema(s, 2, dir="av"), col=5)
}
\seealso{
\code{\link{ema}}, \code{\link{dema}}, \code{\link{rolliter}}
}
