#' Apply Seasonality to a Numeric Vector
#'
#' Applies additive or multiplicative seasonality to a numeric vector using a
#' specified seasonal pattern. The seasonal pattern is assumed to be ordered
#' sequentially from the first to the last seasonal period and is repeated
#' cyclically over the input vector.
#'
#' @param x A numeric vector.
#' @param seasonal_value A numeric vector containing one complete seasonal
#'   cycle.
#' @param seasonal_type Character string specifying the type of seasonality.
#'   Either `"additive"` or `"multiplicative"`.
#' @param cycle_length A positive integer specifying the number of observations
#'   in one seasonal cycle. Defaults to `length(seasonal_value)`.
#' @param applying_cycle_begin An integer indicating which element of
#'   `seasonal_value` should be applied to the first observation of `x`.
#'
#' @return
#' A numeric vector with seasonality applied.
#'
#' @examples
#' x <- 1:12
#'
#' apply_seasonality(
#'   x = x,
#'   seasonal_value = c(-2, 1, 3),
#'   seasonal_type = "additive"
#' )
#'
#' apply_seasonality(
#'   x = x,
#'   seasonal_value = c(0.9, 1.1, 1.2),
#'   seasonal_type = "multiplicative",
#'   applying_cycle_begin = 2
#' )
#'
#' @export
apply_seasonality <- function(
    x,
    seasonal_value,
    seasonal_type = c("additive", "multiplicative"),
    cycle_length = length(seasonal_value),
    applying_cycle_begin = 1) {
  ## --------------------------------------------------
  ## Input validation
  ## --------------------------------------------------

  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.")
  }

  if (!is.numeric(seasonal_value)) {
    stop("'seasonal_value' must be a numeric vector.")
  }

  seasonal_type <- match.arg(seasonal_type)

  if (length(cycle_length) != 1 ||
    !is.numeric(cycle_length) ||
    cycle_length != as.integer(cycle_length) ||
    cycle_length < 1) {
    stop("'cycle_length' must be a positive integer.")
  }

  if (length(seasonal_value) != cycle_length) {
    stop("'length(seasonal_value)' must equal 'cycle_length'.")
  }

  if (length(applying_cycle_begin) != 1 ||
    !is.numeric(applying_cycle_begin) ||
    applying_cycle_begin != as.integer(applying_cycle_begin)) {
    stop("'applying_cycle_begin' must be an integer.")
  }

  if (applying_cycle_begin < 1 ||
    applying_cycle_begin > cycle_length) {
    stop("'applying_cycle_begin' must be between 1 and 'cycle_length'.")
  }

  ## --------------------------------------------------
  ## Seasonal pattern
  ## --------------------------------------------------

  seasonal_pattern <-
    seasonal_value[
      ((seq_along(x) + applying_cycle_begin - 2) %% cycle_length) + 1
    ]

  ## --------------------------------------------------
  ## Apply seasonality
  ## --------------------------------------------------

  if (seasonal_type == "additive") {
    x + seasonal_pattern
  } else {
    x * seasonal_pattern
  }
}
