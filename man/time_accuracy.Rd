\name{time_accuracy}
\alias{time_accuracy}

\title{Calculate Time Series Forecast Accuracy Measures}

\description{
Computes commonly used forecast accuracy measures between observed
(actual) and predicted values for evaluating forecasting models.
}

\usage{
time_accuracy(actual, predicted, digits = 4)
}

\arguments{

\item{actual}{
A numeric vector or time series object containing the observed values.
}

\item{predicted}{
A numeric vector or time series object containing the predicted values.
Must have the same length as \code{actual}.
}

\item{digits}{
An integer specifying the number of decimal places used to round the
returned accuracy measures. Default is \code{4}.
}

}

\details{

The function computes the following forecast accuracy measures:

\itemize{
  \item \strong{R-squared}: Coefficient of determination.
  \item \strong{Correlation}: Pearson correlation coefficient between observed and predicted values.
  \item \strong{RMSE}: Root Mean Squared Error.
  \item \strong{MSE}: Mean Squared Error.
  \item \strong{MAE}: Mean Absolute Error.
  \item \strong{ME}: Mean Error.
  \item \strong{MPE}: Mean Percentage Error.
  \item \strong{MAPE}: Mean Absolute Percentage Error.
  \item \strong{SMAPE}: Symmetric Mean Absolute Percentage Error.
  \item \strong{MASE}: Mean Absolute Scaled Error.
}

Missing values and non-finite values are not permitted. The function
returns \code{NA} for percentage-based measures when the observed values
contain zeros.

}

\value{

A data frame with two columns:

\describe{

\item{Metric}{
Name of the forecast accuracy measure.
}

\item{Value}{
Computed value of the corresponding accuracy measure.
}

}

}

\examples{

# Example using AirPassengers
train <- window(AirPassengers, end = c(1958, 12))
test <- window(AirPassengers, start = c(1959, 1))

fit <- stats::HoltWinters(train)

pred <- predict(fit, n.ahead = length(test))

time_accuracy(actual = test, predicted = pred)

}

\seealso{
\code{\link{predict}},
\code{\link{ts}}
}

\author{
Deepaneesh
}

\keyword{models}
