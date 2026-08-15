#' Reconstruct a series from its cumulative geometric mean
#'
#' Reconstructs the original series from a cumulative geometric
#' mean series. All values in the cumulative geometric mean series
#' must be greater than zero.
#'
#' @param x A numeric vector containing cumulative geometric means,
#'   usually created using `geometric_mean_series()`.
#'
#' @return A numeric vector with the same length as `x` containing
#'   the reconstructed original series.
#'
#' @examples
#' x <- c(100, 108, 115, 123, 130)
#' x_gmean <- geometric_mean_series(x)
#'
#' de_geometric_mean_series(x_gmean)
#'
#' @export
de_geometric_mean_series <- function(x) {

  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.")
  }

  if (any(x <= 0, na.rm = TRUE)) {
    stop("All non-NA values of x must be greater than 0.")
  }

  n <- seq_along(x)

  result <- numeric(length(x))

  result[1] <- x[1]

  if (length(x) > 1) {
    result[-1] <- x[-1]^n[-1] /
      x[-length(x)]^n[-length(x)]
  }

  result
}
