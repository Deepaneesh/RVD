#' Linearly fill missing values inside and outside a vector
#'
#' Fills missing values (NA) in a numeric vector using linear interpolation for
#' internal gaps and linear extrapolation for values outside the observed range.
#' The direction of extrapolation follows the trend of the first two and last two
#' non-missing values.
#'
#' @param x A numeric vector with missing values (NA).
#' @param inside Logical. If TRUE (default), fills missing values between observed points.
#' @param outside Logical. If TRUE (default), fills missing values beyond the observed range.
#' @param back Logical. If TRUE, fills values before the first observed point using the trend.
#'   If FALSE (default), leading missing values remain NA.
#'
#' @return A numeric vector with missing values filled according to the selected options.
#' @examples
#' x <- c(NA, 2, 3, NA, NA, 6, NA, 8, 9, 10, NA)
#' fill_linear(x)
#' fill_linear(x, back = TRUE)
#' @export
fill_linear <- function(x, inside = TRUE, outside = TRUE, back = FALSE) {
  x <- as.numeric(x)
  n <- length(x)
  idx <- which(!is.na(x))

  if (length(idx) < 2) {
    stop("At least two non-NA values are required for linear filling.")
  }

  y <- x

  # Slopes for trend-based extrapolation
  slope_start <- (x[idx[2]] - x[idx[1]]) / (idx[2] - idx[1])
  slope_end   <- (x[idx[length(idx)]] - x[idx[length(idx) - 1]]) /
    (idx[length(idx)] - idx[length(idx) - 1])

  # ---- Inside interpolation ONLY ----
  if (inside) {
    inside_vals <- approx(x = idx, y = x[idx], xout = idx, method = "linear", rule = 1)$y
    # Fill only positions between first and last known values
    full_inside <- approx(x = idx, y = x[idx], xout = seq_len(n), method = "linear", rule = 1)$y
    mid_range <- seq(idx[1], tail(idx, 1))
    y[mid_range] <- full_inside[mid_range]
  }

  # ---- Outside extrapolation ONLY ----
  if (outside) {
    # Backward extrapolation
    if (back && idx[1] > 1) {
      for (i in seq_len(idx[1] - 1)) {
        y[i] <- x[idx[1]] - slope_start * (idx[1] - i)
      }
    }

    # Forward extrapolation
    if (tail(idx, 1) < n) {
      for (i in (tail(idx, 1) + 1):n) {
        y[i] <- x[tail(idx, 1)] + slope_end * (i - tail(idx, 1))
      }
    }
  }

  y
}
