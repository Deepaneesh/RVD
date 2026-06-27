\name{ts_split}
\alias{ts_split}
\title{Split a Time Series into Training and Testing Sets}

\description{
Splits a time series into training and testing subsets using either the
proportion of observations for the training set or the number of
observations in the testing set.
}

\usage{
ts_split(data, prop = NULL, n = NULL)
}

\arguments{

  \item{data}{
  A time series object of class \code{ts} or a numeric vector.
  }

  \item{prop}{
  A numeric value between 0 and 1 specifying the proportion of observations
  to include in the training set.
  }

  \item{n}{
  An integer specifying the number of observations in the testing set.
  }

}

\value{

A list containing:

\describe{
  \item{train_data}{The training data.}
  \item{test_data}{The testing data.}
}

}

\details{
Exactly one of \code{prop} or \code{n} must be supplied.

If \code{prop} is specified, the training set contains
\code{floor(prop * length(data))} observations.

If \code{n} is specified, the last \code{n} observations are used as the
testing set, and the remaining observations form the training set.

For objects of class \code{ts}, the returned training and testing sets
preserve the original time series attributes.
}

\examples{

# Split using 80% of the observations for training
split <- ts_split(AirPassengers, prop = 0.8)

split$train_data
split$test_data

# Use the last 12 observations as the testing set
split <- ts_split(AirPassengers, n = 12)

split$train_data
split$test_data

}

\author{
Deepaneesh
}

\seealso{
\code{\link{window}},
\code{\link{ts}}
}

\keyword{datasets}
