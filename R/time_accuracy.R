#' Calculate Time Series Forecast Accuracy Measures
#'
#' Computes commonly used forecast accuracy metrics between actual and
#' predicted values.
#'
#' @param actual A numeric vector or time series of observed values.
#' @param predicted A numeric vector or time series of predicted values.
#' @param digits Number of decimal places to round the results. Default is 4.
#'
#' @return A data frame containing the forecast accuracy measures.
#'
#' @export

time_accuracy <- function(actual, predicted, digits = 4) {

  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)

  if (length(actual) != length(predicted)) {
    stop("'actual' and 'predicted' must have the same length.",
         call. = FALSE)
  }

  if (anyNA(actual) || anyNA(predicted)) {
    stop("Missing values are not allowed.",
         call. = FALSE)
  }

  if (any(!is.finite(actual)) || any(!is.finite(predicted))) {
    stop("Inputs must contain only finite numeric values.",
         call. = FALSE)
  }

  errors <- actual - predicted

  ## Mean Error
  me <- mean(errors)

  ## Mean Squared Error
  mse <- mean(errors^2)

  ## Root Mean Squared Error
  rmse <- sqrt(mse)

  ## Mean Absolute Error
  mae <- mean(abs(errors))

  ## Mean Percentage Error
  mpe <- if (any(actual == 0)) {
    NA_real_
  } else {
    mean(errors / actual) * 100
  }

  ## Mean Absolute Percentage Error
  mape <- if (any(actual == 0)) {
    NA_real_
  } else {
    mean(abs(errors / actual)) * 100
  }

  ## Symmetric Mean Absolute Percentage Error
  smape <- mean(
    2 * abs(errors) /
      (abs(actual) + abs(predicted)),
    na.rm = TRUE
  ) * 100

  ## Mean Absolute Scaled Error
  mase <- if (length(actual) < 2) {
    NA_real_
  } else {
    scale <- mean(abs(diff(actual)))
    if (scale == 0) NA_real_ else mae / scale
  }

  ## R-squared
  ss_res <- sum(errors^2)
  ss_tot <- sum((actual - mean(actual))^2)

  r_squared <- if (ss_tot == 0) {
    NA_real_
  } else {
    1 - ss_res / ss_tot
  }

  ## Correlation
  correlation <- cor(actual, predicted)

  out <- data.frame(
    Metric = c(
      "R-squared",
      "Correlation",
      "RMSE",
      "MSE",
      "MAE",
      "ME",
      "MPE (%)",
      "MAPE (%)",
      "SMAPE (%)",
      "MASE"
    ),
    Value = round(
      c(
        r_squared,
        correlation,
        rmse,
        mse,
        mae,
        me,
        mpe,
        mape,
        smape,
        mase
      ),
      digits
    ),
    row.names = NULL
  )

  return(out)
}
