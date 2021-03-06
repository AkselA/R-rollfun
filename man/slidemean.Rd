% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slidemean.R
\name{slidemean}
\alias{slidemean}
\title{left-aligned sliding mean}
\usage{
slidemean(x, w = 5, by = 1)
}
\arguments{
\item{x}{numeric; data vector}

\item{w}{integer; window width}

\item{by}{integer; step length}
}
\description{
left-aligned sliding mean
}
\examples{
x <- c(0, 0, 0, 0, 0, 0, 3, 0, 2, 0, 1, 0, 0, 0)
plot(x, type="b")
lines(slidemean(x, 5))
}
