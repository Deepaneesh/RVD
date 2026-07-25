\name{apply_growth_rate_force}
\alias{apply_growth_rate_force}
\title{Apply Growth Rate by Forcing}

\description{
Applies growth rates to fill missing values (\code{NA}) in a numeric vector
by propagating observed values from the left, right, or both directions.

Original (non-missing) observations are never modified.
}

\usage{
apply_growth_rate_force(
  x,
  growth_rate,
  force_from_left = FALSE,
  force_from_right = FALSE
)
}

\arguments{

\item{x}{
A numeric vector containing observations and missing values (\code{NA}).
}

\item{growth_rate}{
A numeric vector of growth rates. Must have the same length as \code{x}.
}

\item{force_from_left}{
Logical. If \code{TRUE}, missing values are filled by propagating values
from left to right.
}

\item{force_from_right}{
Logical. If \code{TRUE}, missing values are filled by propagating values
from right to left.
}

}

\value{
A numeric vector of the same length as \code{x} with missing values filled
where possible. Original non-missing observations remain unchanged.
}

\details{
When forcing from the left, each missing value is estimated using the most
recent observed value and the corresponding growth rate.

When forcing from the right, missing values are estimated by reversing the
growth-rate calculation from the next observed value.

If both \code{force_from_left} and \code{force_from_right} are
\code{TRUE}, the function computes estimates from both directions and
returns their arithmetic mean for missing values that can be estimated by
both methods.
}

\examples{

x <- c(NA, NA, 3, 4, NA, 6, 5, NA, NA, 3, NA, NA)

growth_rate <- rep(0.05, length(x))

apply_growth_rate_force(
  x,
  growth_rate,
  force_from_left = TRUE
)

apply_growth_rate_force(
  x,
  growth_rate,
  force_from_right = TRUE
)

apply_growth_rate_force(
  x,
  growth_rate,
  force_from_left = TRUE,
  force_from_right = TRUE
)

}

\author{
Deepaneesh R V
}
