\name{de_cumsum_series}
\alias{de_cumsum_series}
\title{Reconstruct a Series from Its Cumulative Sum}
\description{
Reconstructs the original series from a cumulative sum series.
}
\usage{
de_cumsum_series(x_cum)
}
\arguments{
  \item{x_cum}{
    A numeric vector containing cumulative sums, usually created
    using \code{\link{cumsum_series}}.
  }
}
\value{
A numeric vector with the same length as \code{x_cum} containing
the reconstructed original series.
}
\examples{
x <- c(100, 8, 7, 8, 7)
x_cum <- cumsum_series(x)

de_cumsum_series(x_cum)
}
\seealso{
\code{\link{cumsum_series}}
}
\keyword{ts}
\keyword{arith}
