
\name{de_diff_series}
\alias{de_diff_series}
\title{Reconstruct a Series from Its First Differences}
\description{
  Reconstructs a series using a first-difference series and the
  original series as an anchor. Missing values in the original series
  are reconstructed when the corresponding differences and neighboring
  values are available.
}
\usage{
  de_diff_series(diff_x, original_x)
}
\arguments{
  \item{diff_x}{
    A first-difference series, usually created using
    \code{\link{diff_series}}.
  }

  \item{original_x}{
    The original series containing at least one known value to use
    as an anchor.
  }
}
\value{
  A reconstructed numeric vector with the same length as \code{diff_x}.
  Existing non-missing values in \code{original_x} are preserved, while
  missing values are reconstructed using the supplied first differences.
}
\examples{
  x <- c(100, 108, 115, 123, 130, NA, NA)
  dx <- diff_series(x)

  dx[6:7] <- c(8, 7)

  de_diff_series(dx, x)
}
\seealso{
  \code{\link{diff_series}}
}
\keyword{ts}
\keyword{arith}
