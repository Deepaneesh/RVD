\name{fill_fl_trend}
\alias{fill_fl_trend}
\title{Fill Left and Right Tails Using a Linear Trend}

\description{
Fits a linear trend using the first and last available (non-missing)
observations in a numeric vector. The fitted trend can be used to fill
missing values before the first observation, after the last observation,
and optionally forecast future values. Interior missing values are left
unchanged.
}

\usage{
fill_fl_trend(
  x,
  fill_right_tail = TRUE,
  fill_left_tail = TRUE,
  original_and_fitted = TRUE,
  only_fitted = FALSE,
  h = 0
)
}

\arguments{

\item{x}{
A numeric vector.
}

\item{fill_right_tail}{
Logical. If \code{TRUE}, fills the right tail.
}

\item{fill_left_tail}{
Logical. If \code{TRUE}, fills the left tail.
}

\item{original_and_fitted}{
Logical. If \code{TRUE}, returns the original vector with the selected
tails filled.
}

\item{only_fitted}{
Logical. If \code{TRUE}, returns the complete fitted trend.
}

\item{h}{
A non-negative integer specifying the number of forecast values to append.
}

}

\value{

Depending on the selected arguments:

\itemize{

\item If \code{original_and_fitted = TRUE}, returns the original vector with
the requested tails filled and forecast values appended.

\item If \code{only_fitted = TRUE}, returns the complete fitted trend.

\item Otherwise, returns only the filled tail values.

}

}

\examples{

x <- c(NA, NA, 10, 12, 14, 16, NA, NA)

fill_fl_trend(x)

fill_fl_trend(x, h = 5)

fill_fl_trend(
  x,
  original_and_fitted = FALSE
)

fill_fl_trend(
  x,
  only_fitted = TRUE
)

fill_fl_trend(
  x,
  fill_left_tail = FALSE
)

fill_fl_trend(
  x,
  fill_right_tail = FALSE
)

}

\author{
Deepaneesh R V
}

\seealso{
\code{\link{fill_linear}}
}

\keyword{utilities}
