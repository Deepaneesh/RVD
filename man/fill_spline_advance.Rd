\name{fill_spline_advance}
\alias{fill_spline_advance}

\title{
Advanced spline interpolation and extrapolation for missing values
}

\description{
Fills missing values in a numeric vector using local spline interpolation
for internal missing values and robust extrapolation for leading and
trailing missing values. The function can optionally control the direction
of the filled values using monotonicity constraints and detect oscillating
patterns for forward extrapolation.
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
  monotonicity_inside = "auto",
  monotonicity_left = "auto",
  monotonicity_right = "auto"
)
}

\arguments{
  \item{x}{
    A numeric vector containing observed and missing (\code{NA}) values.
  }

  \item{inside}{
    Logical. Should missing values occurring between observed values
    be interpolated? Default is \code{TRUE}.
  }

  \item{outside}{
    Logical. Should missing values outside the observed range
    be extrapolated? Default is \code{TRUE}.
  }

  \item{back}{
    Logical. Should leading missing values be filled using backward
    extrapolation? Default is \code{FALSE}.
  }

  \item{method}{

  \itemize{
    \item \code{"monoH.FC"}: Monotone Hermite interpolation using the
    Fritsch-Carlson method. This method preserves the monotonicity of the
    supplied data and helps avoid unnecessary oscillations.

    \item \code{"fmm"}: Forsythe-Malcolm-Moler cubic spline interpolation.
    This is the default method used by \code{stats::spline()}.

    \item \code{"natural"}: Natural cubic spline interpolation. The second
    derivative is constrained to be zero at the endpoints.

    \item \code{"periodic"}: Periodic cubic spline interpolation. This is
    suitable for data where the beginning and end of the series are expected
    to join smoothly.
  }
}

  \item{n}{
    Either \code{"auto"} or a single numeric value greater than or equal
    to 2. Controls the number of observations used for local calculations.
  }

  \item{k}{
    Numeric value between 0 and 1 controlling distance-based weighting
    of observations.
  }

  \item{phi}{
    Numeric value between 0 and 1 controlling extrapolation behaviour.
  }

  \item{smooth}{
    Numeric value between 0 and 1 controlling the amount of smoothing
    applied to extrapolated predictions.
  }

  \item{monotonicity_inside}{
    Character string specifying the monotonicity constraint for internal
    interpolation. Must be one of \code{"auto"}, \code{"increasing"},
    \code{"decreasing"}, or \code{"none"}.
  }

  \item{monotonicity_left}{
    Character string specifying the monotonicity constraint for leading
    value extrapolation. Must be one of \code{"auto"}, \code{"increasing"},
    \code{"decreasing"}, or \code{"none"}.
  }

  \item{monotonicity_right}{
    Character string specifying the monotonicity constraint for trailing
    value extrapolation. Must be one of \code{"auto"}, \code{"increasing"},
    \code{"decreasing"}, or \code{"none"}.
  }
}

\value{
A numeric vector with missing values filled according to the selected
interpolation and extrapolation options.
}

\details{
Internal missing values are filled using local weighted regression or
smoothing spline depending on the number of available neighbouring
observations.

Leading and trailing missing values are handled using robust trend-based
extrapolation rather than extrapolating a smoothing spline outside its
observed range.

The robust trend is estimated using the median of pairwise slopes.
Change limits and curvature limits are applied during extrapolation to
reduce unrealistic jumps and curvature.

When the observed series exhibits an oscillating pattern, the function
attempts to use the detected period and cycle drift to predict future
values.

Monotonicity can be automatically detected or explicitly specified using
the \code{monotonicity_inside}, \code{monotonicity_left}, and
\code{monotonicity_right} arguments.
}

\examples{
x <- c(10, 12, NA, NA, 20, 25, 28)
fill_spline_advance(x)

x <- c(NA, NA, NA, 10, 15, 20, 25)
fill_spline_advance(
  x,
  back = TRUE,
  monotonicity_left = "increasing"
)

x <- c(10, 15, 20, 25, NA, NA)
fill_spline_advance(
  x,
  monotonicity_right = "increasing"
)

x <- c(NA, 10, 15, NA, 25, 30, NA)
fill_spline_advance(
  x,
  inside = TRUE,
  outside = FALSE
)
}

\keyword{imputation}
\keyword{interpolation}
\keyword{extrapolation}
\keyword{time series}
