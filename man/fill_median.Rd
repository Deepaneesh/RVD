\name{fill_median}
\alias{fill_median}
\title{Fill Missing Values Using Median}
\usage{
fill_median(
  x,
  type = "series",
  inside = TRUE,
  outside = FALSE,
  back = FALSE,
  fitted = FALSE
)
}
\arguments{
\item{x}{A numeric vector containing the values to be filled.}

\item{type}{Method used for filling missing values. Must be either
\code{"series"} or \code{"constant"}. The default is \code{"series"}.}

\item{inside}{Logical. If \code{TRUE}, fill missing values occurring
between the first and last non-missing observations. Default is
\code{TRUE}.}

\item{outside}{Logical. If \code{TRUE}, fill missing values occurring
before the first or after the last non-missing observation.
Default is \code{FALSE}.}

\item{back}{Logical. For \code{type = "constant"}, if \code{TRUE},
fill all remaining missing values using the overall median.
This argument cannot be \code{TRUE} when \code{type = "series"}.
Default is \code{FALSE}.}

\item{fitted}{Logical. If \code{TRUE}, return only the fitted median
values instead of the original vector with missing values filled.
Default is \code{FALSE}.}
}
\value{
A numeric vector.
}
\description{
Fills missing values using either a constant median or a cumulative
series median.
}
\details{
For \code{type = "constant"}, one overall median is calculated from
the non-missing observations and used to fill the selected missing
values.

For \code{type = "series"}, a cumulative median is calculated at
each observation using the available non-missing observations up to
that position.

The \code{back = TRUE} option is not supported for the series method.
}
\examples{
x <- c(10, 20, NA, 40, 50)

fill_median(
  x,
  type = "constant"
)

fill_median(
  x,
  type = "series"
)

fill_median(
  x,
  type = "series",
  fitted = TRUE
)
}
\keyword{imputation}
\keyword{missing}
