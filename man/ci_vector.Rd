\name{ci_vector}
\alias{ci_vector}
\title{Calculate Confidence Intervals for a Numeric Vector}
\description{
Calculates standard errors and confidence intervals for each value
in a numeric vector.
}
\usage{
ci_vector(x, type = c("constant", "series"),
          confidence = c(0.95, 0.90))
}
\arguments{
\item{x}{A numeric vector.}

\item{type}{Character string specifying the confidence interval method.
Must be either \code{"constant"} or \code{"series"}.}

\item{confidence}{Numeric vector containing confidence levels between
0 and 1.}
}
\value{
A data frame containing the original values, standard errors,
and lower and upper confidence limits.
}
\details{
When \code{type = "constant"}, a single standard error is calculated
from the standard deviation of the complete vector.

When \code{type = "series"}, a local standard error is calculated
for each observation using neighboring observations.

Confidence intervals are calculated using the normal approximation.
}
\examples{
x <- c(10, 12, 15, 14, 18, 20)

ci_vector(x, type = "constant")

ci_vector(x, type = "series")
}
\keyword{statistics}
