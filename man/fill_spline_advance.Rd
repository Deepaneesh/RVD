\name{fill_spline_advance}
\alias{fill_spline_advance}
\title{Advanced spline-based missing value filling}
\description{
Fills missing values in a numeric vector using local spline interpolation
and extrapolation with additional controls for trend, oscillation,
monotonicity, curvature, and rate of change.
}
\usage{
fill_spline_advance(
  x,
  inside = TRUE,
  outside = TRUE,
  back = FALSE,
  method = "monoH.FC",
  n = "auto",
  k = 0.6,
  phi = 0.8,
  smooth = 0.7,
  monotonic = "auto"
)
}
\arguments{
\item{x}{A numeric vector containing missing (\code{NA}) values.}

\item{inside}{Logical. If \code{TRUE}, fill missing values between the first
and last observed values.}

\item{outside}{Logical. If \code{TRUE}, fill missing values outside the range
of observed values.}

\item{back}{Logical. If \code{TRUE}, fill leading missing values by backward
extrapolation. This is used only when \code{outside = TRUE}.}

\item{method}{
Character string specifying the interpolation method. Available methods are:

\itemize{
  \item \code{"monoH.FC"}: Monotone Hermite interpolation. This method
  preserves the monotonicity of the supplied data and avoids introducing
  unnecessary oscillations.

  \item \code{"fmm"}: Forsythe-Malcolm-Moler cubic spline. This is the
  default method used by \code{stats::spline()}.

  \item \code{"natural"}: Natural cubic spline interpolation. The second
  derivative is constrained to be zero at the endpoints.

  \item \code{"periodic"}: Periodic cubic spline interpolation. This is
  suitable for data where the beginning and end of the series are expected
  to join smoothly.
}
}

\item{n}{Number of local observations used for prediction. Use
\code{"auto"} for adaptive selection, or an integer greater than or equal
to 2.}

\item{k}{Numeric value between 0 and 1 controlling the distance weighting
of local observations. Smaller values give more weight to nearby
observations.}

\item{phi}{Numeric value between 0 and 1 controlling the damping of the
extrapolated trend.}

\item{smooth}{Numeric value between 0 and 1 controlling the balance between
the model prediction and the previous observed or fitted value.}

\item{monotonic}{Character value controlling monotonicity. Possible values
are \code{"auto"}, \code{"increasing"}, \code{"decreasing"}, and
\code{"none"}.}
}
\value{
A numeric vector of the same length as \code{x}, with missing values filled
according to the selected options.
}
\details{
For internal missing values, local observations surrounding each missing
position are used for prediction. Linear regression is used when only two
observations are available, quadratic regression when three observations
are available, and \code{smooth.spline()} when four or more observations
are available.

For extrapolation, the function combines local spline prediction with a
robust trend estimate. The trend uses the median of pairwise slopes,
providing resistance to individual unusual observations.

Oscillating series are detected using turning points, direction changes,
and consistency of turning-point distances. When an oscillating pattern is
detected, previous-cycle information is incorporated into the prediction.

Extrapolated predictions are additionally controlled using recent changes,
curvature, and optional monotonicity constraints. The extrapolated trend
is damped using \code{phi}.
}
\examples{
x <- c(10, 12, NA, NA, 20, 22, 24)
fill_spline_advance(x)

x <- c(100, 105, 112, 120, NA, NA, NA)
fill_spline_advance(x, outside = TRUE)

x <- c(NA, NA, 100, 105, 112, 120, NA, NA)
fill_spline_advance(
  x,
  inside = TRUE,
  outside = TRUE,
  back = TRUE
)

x <- c(
  100, 110, 120, 115, 105, 95,
  100, 112, 125, 118, NA, NA, NA
)
fill_spline_advance(x, n = 6)
}
\seealso{
\code{\link[stats]{smooth.spline}},
\code{\link{fill_spline1}}
}
\keyword{ts}
\keyword{imputation}
\keyword{missing}
\keyword{spline}
