#' Create a cumulative sum series
#'
#' Calculates the cumulative sum of a numeric series.
#'
#' @param x A numeric vector or time series.
#'
#' @return A numeric vector containing the cumulative sums of `x`.
#'
#' @examples
#' x <- c(100, 8, 7, 8, 7)
#' cumsum_series(x)
#'
#' @export
cumsum_series <- function(x) {

  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.")
  }

  cumsum(x)
}
