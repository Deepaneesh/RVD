#' Reconstruct a series from its cumulative mean
#'
#' Reconstructs the original series from a cumulative mean series.
#'
#' @param mean_series A numeric vector containing cumulative means,
#'   usually created using `mean_series()`.
#'
#' @return A numeric vector with the same length as `mean_series`
#'   containing the reconstructed original series.
#'
#' @examples
#' x <- c(100, 108, 115, 123, 130)
#' x_mean <- mean_series(x)
#'
#' de_mean_series(x_mean)
#'
#' @export
de_mean_series <- function(mean_series) {

  if (!is.numeric(mean_series)) {
    stop("`mean_series` must be a numeric vector.")
  }

  n <- length(mean_series)

  if (n == 0) {
    return(numeric(0))
  }

  x <- numeric(n)

  x[1] <- mean_series[1]

  if (n > 1) {
    for (i in 2:n) {
      x[i] <- i * mean_series[i] -
        (i - 1) * mean_series[i - 1]
    }
  }

  x
}
