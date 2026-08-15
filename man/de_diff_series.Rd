\name{de_diff_series}
\alias{de_diff_series}
\title{Reconstruct a Series from Its n-th Differences}
\description{
Reconstructs a series from an n-th difference series and the known
values in the original series. Missing values after the last known
observation are reconstructed using the supplied difference series.
}
\usage{
de_diff_series(diff_x, original_x, differences = 1)
}
\arguments{
  \item{diff_x}{
    A numeric vector containing an n-th difference series, usually
    created using \code{\link{diff_series}}.
  }

  \item{original_x}{
    A numeric vector containing the original series. It should contain
    known values that can be used as anchors for reconstruction.
  }

  \item{differences}{
    Integer specifying the order of differencing. Defaults to
    \code{1}.
  }
}
\value{
A numeric vector with the same length as \code{diff_x}. Existing
values in \code{original_x} are preserved and missing values after
the last known observation are reconstructed.
}
\examples{
x <- c(100, 108, 115, 123, 130, NA, NA)

d1 <- diff_series(x, differences = 1)
d1[6:7] <- c(8, 7)

de_diff_series(d1, x, differences = 1)
}
\seealso{
\code{\link{diff_series}}
}
\keyword{ts}
\keyword{arith}
