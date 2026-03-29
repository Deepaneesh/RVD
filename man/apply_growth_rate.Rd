\name{apply_growth_rate}
\alias{apply_growth_rate}
\title{Reconstruct Series from Growth Rates with Lag k}

\description{
Reconstructs a numeric series using a given growth rate vector and a partially
observed series containing anchor values. The reconstruction follows the specified
lag \eqn{k}, where each value depends on the value \eqn{k} steps before.
}

\usage{
apply_growth_rate(gr, series, k = 1)
}

\arguments{
  \item{gr}{
    A numeric vector representing the growth rates. Typically obtained from
    \code{growth_rate()}.
  }
  \item{series}{
    A numeric vector of the same length as \code{gr}, containing \code{NA}
    values except for known anchor points used for reconstruction.
  }
  \item{k}{
    An integer specifying the lag. Must be greater than or equal to 1
    and less than the length of the series. Default is 1.
  }
}

\value{
A numeric vector of the same length as \code{gr}, representing the reconstructed
series. Positions corresponding to chains without anchor values will remain \code{NA}.
}

\details{
The reconstruction is based on the growth rate definition:

\deqn{x_t = x_{t-k} (1 + g_t)}

The series is divided into \eqn{k} independent chains:

\itemize{
  \item Chain 1: 1, 1+k, 1+2k, ...
  \item Chain 2: 2, 2+k, 2+2k, ...
  \item ...
  \item Chain k: k, 2k, 3k, ...
}

For each chain:
\itemize{
  \item Exactly one known value (anchor) must be provided for full reconstruction.
  \item If multiple values are present in a chain, the function stops with an error.
  \item If no value is present in a chain, that chain remains \code{NA}.
}

Forward and backward reconstruction are applied within each chain.
}

\examples{
# Example 1: Full reconstruction (k = 2)
x <- c(100, 120, 100, 180, 210, 100)

g <- growth_rate(x, k = 2)

y <- rep(NA, length(x))
y[3] <- 100   # chain 1
y[4] <- 180   # chain 2

apply_growth_rate(g, y, k = 2)

# Example 2: Missing chain (partial NA)
y <- rep(NA, length(x))
y[4] <- 180

apply_growth_rate(g, y, k = 2)

# Example 3: Error (multiple values in same chain)
y <- rep(NA, length(x))
y[4] <- 180
y[6] <- 100   # same chain

# This will throw an error
try(apply_growth_rate(g, y, k = 2))
}

\seealso{
\code{\link{growth_rate}}
}

\author{
Your Name
}

\note{
This function is useful for reconstructing time series from growth rates,
handling missing data, and inverse transformations in time series analysis.
}
