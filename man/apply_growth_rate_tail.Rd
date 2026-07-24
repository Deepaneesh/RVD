\name{apply_growth_rate_tail}
\alias{apply_growth_rate_tail}
\title{Apply Growth Rates to Leading and Trailing Missing Values}

\description{
Applies growth rates to fill leading and/or trailing missing values in a
numeric vector while preserving all existing observations.
}

\usage{
apply_growth_rate_tail(
  x,
  growth_rate,
  left_tail = TRUE,
  right_tail = TRUE
)
}

\arguments{

\item{x}{
A numeric vector containing leading and/or trailing \code{NA} values.
}

\item{growth_rate}{
A numeric vector of growth rates with the same length as \code{x}.
}

\item{left_tail}{
Logical. Should leading \code{NA} values be filled?
}

\item{right_tail}{
Logical. Should trailing \code{NA} values be filled?
}

}

\details{

Leading missing values are reconstructed using

\deqn{x_i = \frac{x_{i+1}}{1 + g_{i+1}}}

Trailing missing values are projected using

\deqn{x_i = x_{i-1}(1 + g_i)}

where \eqn{g_i} denotes the growth rate corresponding to position
\eqn{i}. Existing non-missing values are never modified.

}

\value{
A numeric vector with leading and/or trailing missing values replaced
using the supplied growth rates.
}

\examples{

gr <- c(0.2, 0.3, 0.4, 0.2, 0.1, 0.4, 0.3, 0.2)

x <- c(NA, NA, NA, 2, 3, NA, NA, NA)

apply_growth_rate_tail(x, gr)

apply_growth_rate_tail(
  x,
  gr,
  left_tail = TRUE,
  right_tail = FALSE
)

apply_growth_rate_tail(
  x,
  gr,
  left_tail = FALSE,
  right_tail = TRUE
)

}

\author{
Deepaneesh R V
}
