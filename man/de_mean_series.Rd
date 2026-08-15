\name{de_mean_series}
\alias{de_mean_series}
\title{Reconstruct a Series from Its Cumulative Mean}
\description{
Reconstructs the original series from a cumulative mean series.
}
\usage{
de_mean_series(mean_series)
}
\arguments{
  \item{mean_series}{
    A numeric vector containing cumulative means, usually created
    using \code{\link{mean_series}}.
  }
}
\value{
A numeric vector with the same length as \code{mean_series}
containing the reconstructed original series.
}
\examples{
x <- c(100, 108, 115, 123, 130)
x_mean <- mean_series(x)

de_mean_series(x_mean)
}
\seealso{
\code{\link{mean_series}}
}
\keyword{ts}
\keyword{arith}
