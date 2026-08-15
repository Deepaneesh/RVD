\name{fill_constant}
\alias{fill_constant}
\title{Fill a Vector When It Contains Only One Unique Non-Missing Value}
\description{
  Checks the non-missing values of a vector. If exactly one unique
  non-missing value is present, all elements of the vector are replaced
  with that value. If there are zero or more than one unique non-missing
  values, the original vector is returned unchanged.
}
\usage{
  fill_constant(x)
}
\arguments{
  \item{x}{
    A vector. Supports numeric, character, logical, Date, and other
    vector types that support \code{is.na()}.
  }
}
\value{
  A vector with the same length as \code{x}. If exactly one unique
  non-missing value exists, all elements are replaced with that value.
  Otherwise, \code{x} is returned unchanged.
}
\examples{
  # Numeric vector with one unique value
  x <- c(10, NA, 10, NA, 10)
  fill_constant(x)

  # Character vector with one unique value
  x <- c("A", NA, "A", NA, "A")
  fill_constant(x)

  # Multiple unique values - returned unchanged
  x <- c(10, NA, 20, NA)
  fill_constant(x)

  # All values are NA - returned unchanged
  x <- c(NA, NA, NA)
  fill_constant(x)
}
\keyword{manip}
\keyword{NA}
