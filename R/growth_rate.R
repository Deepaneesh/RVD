#############################################
# Growth Rate Function (Lag k)
#############################################

#' Calculate Growth Rate with Lag k
#'
#' Computes the growth rate of a numeric series using a specified lag.
#'
#' @param x Numeric vector (time series or sequence)
#' @param k Integer lag (default = 1)
#'
#' @return Numeric vector of growth rates with first k values as NA
#'
#' @details
#' Growth rate is defined as:
#' (x[t] - x[t-k]) / x[t-k]
#'
#' @examples
#' x <- c(100, 120, 150, 180)
#' growth_rate(x, k = 1)
#'
#' @export

growth_rate <- function(x, k = 1) {

  # -----------------------------
  # Input validation
  # -----------------------------
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }

  n <- length(x)

  if (k < 1 || k >= n) {
    stop("k must be >= 1 and less than length of x")
  }

  # -----------------------------
  # Initialize output
  # -----------------------------
  g <- rep(NA_real_, n)

  # -----------------------------
  # Compute growth rate
  # -----------------------------
  for (t in seq.int(k + 1, n)) {

    if (!is.na(x[t]) && !is.na(x[t - k]) && x[t - k] != 0) {
      g[t] <- (x[t] - x[t - k]) / x[t - k]
    } else {
      g[t] <- NA_real_
    }

  }

  return(g)
}
