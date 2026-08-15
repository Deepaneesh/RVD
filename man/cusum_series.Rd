\name{cumsum_series}
\alias{cumsum_series}
\title{Create a Cumulative Sum Series}
\description{
Calculates the cumulative sum of a numeric series.
}
\usage{
cumsum_series(x)
}
\arguments{
  \item{x}{
    A numeric vector or time series.
  }
}
\value{
A numeric vector containing the cumulative sums of \code{x}.
}
\examples{
x <- c(100, 8, 7, 8, 7)
cumsum_series(x)
}
\seealso{
\code{\link{cumsum}}
}
\keyword{ts}
\keyword{arith}
