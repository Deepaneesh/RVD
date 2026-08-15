
\name{diff_series}
\alias{diff_series}
\title{Create a First-Difference Series}
\description{
Calculates the first difference of a series while preserving
the original length. The first observation is always \code{NA}.
}
\usage{
diff_series(x)
}
\arguments{
  \item{x}{
    A numeric vector or time series.
  }
}
\value{
A numeric vector with the same length as \code{x}. The first
observation is \code{NA}, followed by the first differences
between consecutive observations.
}
\examples{
x <- c(100, 108, 115, 123, 130)
diff_series(x)
}
\seealso{
\code{\link{diff}}
}
\keyword{ts}
\keyword{arith}
