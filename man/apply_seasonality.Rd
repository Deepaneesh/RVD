\name{apply_seasonality}
\alias{apply_seasonality}

\title{Apply Seasonality to a Numeric Vector}

\description{
Applies additive or multiplicative seasonality to a numeric vector using
a specified seasonal pattern. The seasonal pattern is assumed to be in
its natural sequential order and is repeated cyclically over the input
vector.
}

\usage{
apply_seasonality(
  x,
  seasonal_value,
  seasonal_type = c("additive", "multiplicative"),
  cycle_length = length(seasonal_value),
  applying_cycle_begin = 1
)
}

\arguments{

\item{x}{
A numeric vector.
}

\item{seasonal_value}{
A numeric vector containing one complete seasonal cycle.
}

\item{seasonal_type}{
Character string specifying the type of seasonality.
Must be either \code{"additive"} or \code{"multiplicative"}.
}

\item{cycle_length}{
A positive integer specifying the number of observations in one seasonal
cycle. Defaults to \code{length(seasonal_value)}.
}

\item{applying_cycle_begin}{
An integer indicating which element of \code{seasonal_value} should be
applied to the first observation of \code{x}. The seasonal values are
assumed to be in sequential order and are repeated cyclically from this
starting position.
}

}

\value{
A numeric vector with seasonality applied.
}

\examples{

x <- 1:12

apply_seasonality(
  x = x,
  seasonal_value = c(-2, 1, 3),
  seasonal_type = "additive"
)

apply_seasonality(
  x = x,
  seasonal_value = c(0.9, 1.1, 1.2),
  seasonal_type = "multiplicative",
  applying_cycle_begin = 2
)

}

\author{
Deepaneesh R V
}

\seealso{
\code{\link{fill_fl_trend}}
}
