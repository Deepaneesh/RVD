\name{geometric_mean_series}
\alias{geometric_mean_series}
\title{Create a Cumulative Geometric Mean Series}
\description{
Calculates the cumulative geometric mean of a numeric series.
All non-missing values must be greater than zero.
}
\usage{
geometric_mean_series(x)
}
\arguments{
  \item{x}{
    A numeric vector or time series containing positive values.
  }
}
\value{
A numeric vector containing the cumulative geometric mean at each
position of \code{x}.
}
\examples{
x <- c(100, 108, 115, 123, 130)
geometric_mean_series(x)
}
\seealso{
\code{\link{mean}}
}
\keyword{ts}
\keyword{arith}
