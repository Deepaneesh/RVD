\name{median_series}
\alias{median_series}
\title{Calculate a Cumulative Median Series}
\description{
Calculates a median series where each value represents the median
of all observations from the beginning of the vector up to the
current observation.
}
\usage{
median_series(x, na.rm = TRUE)
}
\arguments{
\item{x}{A numeric vector.}

\item{na.rm}{Logical value indicating whether missing values should
be removed before calculating the median.}
}
\value{
A numeric vector of the same length as \code{x}.
The ith value is the median of \code{x[1:i]}.
}
\details{
The function calculates a cumulative or expanding median.

For example, for the vector
\code{c(10, 20, 30, 5)}, the resulting median series is
\code{c(10, 15, 20, 15)}.
}
\examples{
x <- c(10, 20, 30, 5, 25)

median_series(x)
}
\keyword{statistics}
