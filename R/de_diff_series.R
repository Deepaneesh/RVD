#' Reconstruct a series from its first differences
#'
#' Reconstructs the original series using a difference series and
#' the original series as an anchor.
#'
#' @param diff_x A first-difference series, usually created using
#'   `diff_series()`.
#' @param original_x The original series containing at least one
#'   known value to use as an anchor.
#'
#' @return A reconstructed numeric vector with the same length as
#'   `diff_x`.
#'
#' @examples
#' x <- c(100, 108, 115, 123, 130, NA, NA)
#' dx <- diff_series(x)
#'
#' dx[6:7] <- c(8, 7)
#'
#' de_diff_series(dx, x)
#'
#' @export
de_diff_series <- function(diff_x, original_x) {

  if (!is.numeric(diff_x)) {
    stop("`diff_x` must be a numeric vector.")
  }

  if (!is.numeric(original_x)) {
    stop("`original_x` must be a numeric vector.")
  }

  if (length(diff_x) != length(original_x)) {
    stop("`diff_x` and `original_x` must have the same length.")
  }

  # Start with the original series
  result <- original_x

  # Find the first available original value
  anchor <- which(!is.na(original_x))[1]

  if (is.na(anchor)) {
    stop("`original_x` must contain at least one non-missing value.")
  }

  # Reconstruct values after the anchor
  if (anchor < length(result)) {

    for (i in (anchor + 1):length(result)) {

      if (is.na(result[i]) && !is.na(diff_x[i]) && !is.na(result[i - 1])) {
        result[i] <- result[i - 1] + diff_x[i]
      }
    }
  }

  # Reconstruct values before the anchor
  if (anchor > 1) {

    for (i in (anchor - 1):1) {

      if (is.na(result[i]) && !is.na(diff_x[i + 1]) && !is.na(result[i + 1])) {
        result[i] <- result[i + 1] - diff_x[i + 1]
      }
    }
  }

  result
}
