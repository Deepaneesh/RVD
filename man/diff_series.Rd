\name{diff_series}
\alias{diff_series}
\title{Create an n-th Difference Series}
\description{
Calculates the n-th difference of a numeric series while preserving
the original length. The first \code{differences} observations are
always \code{NA}.
}
\usage{
diff_series(x, differences = 1)
}
\arguments{
  \item{x}{
    A numeric vector or time series.
  }

  \item{differences}{
    Integer specifying the order of differencing. Defaults to
    \code{1}.
  }
}
\value{
A numeric vector with the same length as \code{x}.
}
\examples{
x <- c(100, 108, 115, 123, 130)

diff_series(x)

diff_series(x, differences = 2)
}
\seealso{
\code{\link{diff}}
}
\keyword{ts}
\keyword{arith}
