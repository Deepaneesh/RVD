\name{mean_series}
\alias{mean_series}
\title{Create a Cumulative Mean Series}
\description{
Calculates the cumulative arithmetic mean of a numeric series.
Missing values can optionally be excluded from the calculation.
}
\usage{
mean_series(x, na.rm = FALSE)
}
\arguments{
  \item{x}{
    A numeric vector or time series.
  }

  \item{na.rm}{
    Logical value indicating whether missing values should be removed
    from the calculation. Defaults to \code{FALSE}.
  }
}
\value{
A numeric vector containing the cumulative mean at each position
of \code{x}.
}
\examples{
x <- c(100, 108, 115, 123, 130)
mean_series(x)

x_na <- c(100, 108, NA, 123, 130)
mean_series(x_na, na.rm = TRUE)
}
\seealso{
\code{\link{mean}}
}
\keyword{ts}
\keyword{arith}
