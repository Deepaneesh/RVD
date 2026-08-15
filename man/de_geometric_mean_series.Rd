\name{de_geometric_mean_series}
\alias{de_geometric_mean_series}
\title{Reconstruct a Series from Its Cumulative Geometric Mean}
\description{
Reconstructs the original series from a cumulative geometric mean
series. All values in the cumulative geometric mean series must be
greater than zero.
}
\usage{
de_geometric_mean_series(x)
}
\arguments{
  \item{x}{
    A numeric vector containing cumulative geometric means, usually
    created using \code{\link{geometric_mean_series}}.
  }
}
\value{
A numeric vector with the same length as \code{x} containing the
reconstructed original series.
}
\examples{
x <- c(100, 108, 115, 123, 130)
x_gmean <- geometric_mean_series(x)

de_geometric_mean_series(x_gmean)
}
\seealso{
\code{\link{geometric_mean_series}}
}
\keyword{ts}
\keyword{arith}
