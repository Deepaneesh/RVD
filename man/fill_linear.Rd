\name{fill_linear}
\alias{fill_linear}

\title{
Linearly fill missing values in a numeric vector
}

\description{
Fills missing values (NA) in a numeric vector using linear interpolation for
internal gaps and linear extrapolation for values outside the observed range.
The extrapolation follows the trend of the first two and last two non-missing values.
}

\usage{
fill_linear(x, inside = TRUE, outside = TRUE, back = FALSE)
}

\arguments{
  \item{x}{
    A numeric vector containing missing values (NA).
  }
  \item{inside}{
    Logical. If TRUE (default), missing values between observed points are filled
    using linear interpolation.
  }
  \item{outside}{
    Logical. If TRUE (default), missing values beyond the observed range are filled
    using linear extrapolation.
  }
  \item{back}{
    Logical. If TRUE, values before the first observed point are filled using the
    linear trend. If FALSE (default), leading missing values remain NA.
  }
}

\details{
The function estimates the slope at the beginning and end of the series using the
first two and last two non-missing values, respectively. These slopes are used to
extend the series linearly beyond the observed range when extrapolating.
}

\value{
A numeric vector with missing values filled according to the selected options.
}

\examples{
x <- c(NA, 2, 3, NA, NA, 6, NA, 8, 9, 10, NA)
fill_linear(x)
fill_linear(x, back = TRUE)
}
