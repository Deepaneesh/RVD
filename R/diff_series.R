#' Create a first-difference series
#'
#' Calculates the first difference of a series while preserving
#' the original length. The first observation is always NA.
#'
#' @param x A numeric vector or time series.
#'
#' @return A numeric vector with the same length as `x`.
#'
#' @examples
#' x <- c(100, 108, 115, 123, 130)
#' diff_series(x)
#'
#' @export
diff_series <- function(x) {

  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.")
  }

  c(NA_real_, diff(x))
}
