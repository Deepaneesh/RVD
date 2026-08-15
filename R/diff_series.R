#' Create an n-th difference series
#'
#' Calculates the n-th difference of a numeric series while preserving
#' the original length. The first `differences` observations are always
#' `NA`.
#'
#' @param x A numeric vector or time series.
#' @param differences Integer specifying the order of differencing.
#'   Defaults to `1`.
#'
#' @return A numeric vector with the same length as `x`.
#'
#' @examples
#' x <- c(100, 108, 115, 123, 130)
#'
#' diff_series(x)
#'
#' diff_series(x, differences = 2)
#'
#' @export
diff_series <- function(x, differences = 1) {

  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.")
  }

  if (!is.numeric(differences) ||
      length(differences) != 1 ||
      is.na(differences) ||
      differences < 1 ||
      differences != as.integer(differences)) {
    stop("`differences` must be a positive integer.")
  }

  differences <- as.integer(differences)

  if (length(x) <= differences) {
    return(rep(NA_real_, length(x)))
  }

  c(
    rep(NA_real_, differences),
    diff(x, differences = differences)
  )
}
