\name{fill_spline}
\alias{fill_spline}
\title{Fill Missing Values Using Spline Interpolation}
\usage{
fill_spline(
  x,
  inside = TRUE,
  outside = TRUE,
  back = FALSE,
  method = "natural"
)
}
\arguments{
  \item{x}{A numeric vector containing missing values (NA).}

  \item{inside}{Logical. Should missing values between observed values be filled using spline interpolation?}

  \item{outside}{Logical. Should missing values outside the observed range be filled using spline extrapolation?}

  \item{back}{Logical. Should backward extrapolation be performed for leading missing values?}

  \item{method}{Character string specifying the spline method passed to \code{stats::splinefun()}.}
}
\value{
A numeric vector with missing values filled using spline interpolation and/or extrapolation.
}
\description{
Fills missing values in a numeric vector using spline interpolation.
Missing values between observed values are interpolated, while missing values outside the observed range can optionally be extrapolated.
}
\details{
The function first constructs a spline function using the non-missing observations.

If \code{inside = TRUE}, missing values between the first and last observed values are interpolated.

If \code{outside = TRUE}, missing values beyond the observed range are extrapolated.

If \code{back = TRUE}, leading missing values are also extrapolated.

At least two non-missing observations are required.
}
\examples{
x <- c(10, NA, NA, 25, 30)
fill_spline(x)

x <- c(NA, NA, 10, 20, NA, 35, NA)
fill_spline(x, back = TRUE)

x <- c(5, NA, NA, 20)
fill_spline(x, method = "fmm")
}
