#' Create a cumulative geometric mean series
#'
#' Calculates the cumulative geometric mean of a numeric series.
#' All non-missing values must be greater than zero.
#'
#' @param x A numeric vector or time series containing positive values.
#'
#' @return A numeric vector containing the cumulative geometric mean
#'   at each position of `x`.
#'
#' @examples
#' x <- c(100, 108, 115, 123, 130)
#' geometric_mean_series(x)
#'
#' @export
geometric_mean_series <- function(x) {

  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.")
  }

  if (any(x <= 0, na.rm = TRUE)) {
    stop("All non-NA values of x must be greater than 0.")
  }

  exp(cumsum(log(x)) / seq_along(x))
}
