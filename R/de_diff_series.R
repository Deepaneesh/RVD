#' Reconstruct a series from its n-th differences
#'
#' Reconstructs a series from an n-th difference series and the
#' known values in the original series. Missing values after the
#' last known observation are reconstructed using the supplied
#' difference series.
#'
#' @param diff_x A numeric vector containing an n-th difference series,
#'   usually created using `diff_series()`.
#' @param original_x A numeric vector containing the original series.
#'   It should contain known values that can be used as anchors for
#'   reconstruction.
#' @param differences Integer specifying the order of differencing.
#'   Defaults to `1`.
#'
#' @return A numeric vector with the same length as `diff_x`.
#'   Existing values in `original_x` are preserved and missing values
#'   after the last known observation are reconstructed.
#'
#' @examples
#' x <- c(100, 108, 115, 123, 130, NA, NA)
#'
#' d1 <- diff_series(x, differences = 1)
#' d1[6:7] <- c(8, 7)
#'
#' de_diff_series(d1, x, differences = 1)
#'
#' @export
de_diff_series <- function(diff_x, original_x, differences = 1) {

  if (!is.numeric(diff_x)) {
    stop("`diff_x` must be a numeric vector.")
  }

  if (!is.numeric(original_x)) {
    stop("`original_x` must be a numeric vector.")
  }

  if (length(diff_x) != length(original_x)) {
    stop("`diff_x` and `original_x` must have the same length.")
  }

  if (length(differences) != 1 ||
      !is.numeric(differences) ||
      is.na(differences) ||
      differences < 1 ||
      differences != as.integer(differences)) {
    stop("`differences` must be a positive integer.")
  }

  differences <- as.integer(differences)

  n <- length(original_x)

  if (n <= differences) {
    stop("Series length must be greater than `differences`.")
  }

  # Find the last known observation
  last_known <- max(which(!is.na(original_x)))

  # If there are no missing values after the last known value,
  # nothing needs to be reconstructed.
  if (last_known == n) {
    return(original_x)
  }

  # Start with the n-th differences
  current <- diff_x

  # Reconstruct lower-order differences
  for (d in seq(differences, 1)) {

    # Get known original values
    known_x <- original_x[1:last_known]

    # Create the required lower-order difference
    if (d == 1) {
      lower <- known_x
    } else {
      lower <- diff(known_x, differences = d - 1)
    }

    # Find the last known value
    last_value <- lower[length(lower)]

    # Reconstruct future values
    future_idx <- (last_known + 1):n

    for (i in future_idx) {

      if (!is.na(current[i])) {
        last_value <- last_value + current[i]
        lower <- c(lower, last_value)
      }
    }

    # If this is the first difference, we now have
    # the original series.
    if (d == 1) {

      result <- original_x

      result[(last_known + 1):n] <-
        lower[(length(lower) - length(future_idx) + 1):length(lower)]

      return(result)
    }

    # Otherwise continue to the next lower difference.
    current <- c(
      rep(NA_real_, d - 1),
      lower
    )
  }

  original_x
}
