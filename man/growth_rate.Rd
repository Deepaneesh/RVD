\name{growth_rate}
\alias{growth_rate}
\title{Calculate Growth Rate with Lag k}

\description{
Computes the growth rate of a numeric series using a specified lag \eqn{k}.
The growth rate represents the relative change between the current value
and the value \eqn{k} periods before.
}

\usage{
growth_rate(x, k = 1)
}

\arguments{
  \item{x}{
    A numeric vector representing the time series or sequence.
  }
  \item{k}{
    An integer specifying the lag. Must be greater than or equal to 1
    and less than the length of \code{x}. Default is 1.
  }
}

\value{
A numeric vector of the same length as \code{x}, where the first \code{k}
values are \code{NA}, and the remaining values represent the growth rate.
}

\details{
The growth rate is defined as:

\deqn{g_t = \frac{x_t - x_{t-k}}{x_{t-k}}}

The first \code{k} observations are set to \code{NA} because there are no
prior values available for comparison.

If either \code{x[t]} or \code{x[t-k]} is \code{NA}, or if \code{x[t-k] = 0},
the result at that position will be \code{NA}.
}

\examples{
# Example 1: Basic usage
x <- c(100, 120, 150, 180)
growth_rate(x, k = 1)

# Example 2: Lag = 2
growth_rate(x, k = 2)

# Example 3: With missing values
x <- c(100, NA, 150, 180)
growth_rate(x, k = 1)
}

\seealso{
\code{\link{diff}}
}

\author{
Your Name
}

\note{
This function is useful in time series analysis, finance, and econometrics
for measuring relative change over time.
}
