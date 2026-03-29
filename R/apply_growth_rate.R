#############################################
# Apply Growth Rate (Reconstruct Series)
#############################################

#' Reconstruct Series from Growth Rates with Lag k
#'
#' Reconstructs a numeric series using a given growth rate vector and a partially
#' observed series containing anchor values. The reconstruction follows a lag
#' structure where each value depends on the value \code{k} steps before.
#'
#' @param gr A numeric vector of growth rates.
#' @param series A numeric vector of the same length as \code{gr}, containing
#'   \code{NA} values except for known anchor points.
#' @param k An integer specifying the lag. Must be >= 1 and less than the length
#'   of the series. Default is 1.
#'
#' @return A numeric vector representing the reconstructed series. Chains without
#'   anchor values remain \code{NA}.
#'
#' @details
#' The reconstruction is based on:
#' \deqn{x_t = x_{t-k} (1 + g_t)}
#'
#' The series is divided into \code{k} independent chains:
#' \itemize{
#'   \item Chain 1: 1, 1+k, 1+2k, ...
#'   \item Chain 2: 2, 2+k, 2+2k, ...
#'   \item ...
#'   \item Chain k: k, 2k, 3k, ...
#' }
#'
#' For each chain:
#' \itemize{
#'   \item If exactly one anchor is provided → reconstruction is performed
#'   \item If no anchor → that chain remains \code{NA}
#'   \item If multiple anchors → function stops with an error
#' }
#'
#' @examples
#' # Example 1: Full reconstruction
#' x <- c(100, 120, 100, 180, 210, 100)
#' g <- growth_rate(x, k = 2)
#'
#' y <- rep(NA, length(x))
#' y[3] <- 100
#' y[4] <- 180
#'
#' apply_growth_rate(g, y, k = 2)
#'
#' # Example 2: Missing chain
#' y <- rep(NA, length(x))
#' y[4] <- 180
#'
#' apply_growth_rate(g, y, k = 2)
#'
#' # Example 3: Error case
#' y <- rep(NA, length(x))
#' y[4] <- 180
#' y[6] <- 100
#'
#' try(apply_growth_rate(g, y, k = 2))
#'
#' @seealso \code{\link{growth_rate}}
#'
#' @export

apply_growth_rate <- function(gr, series, k = 1) {

  # -----------------------------
  # Input validation
  # -----------------------------
  if (!is.numeric(gr)) {
    stop("gr must be a numeric vector")
  }

  if (!is.numeric(series)) {
    stop("series must be a numeric vector")
  }

  n <- length(gr)

  if (length(series) != n) {
    stop("gr and series must have the same length")
  }

  if (k < 1 || k >= n) {
    stop("k must be >= 1 and less than length of series")
  }

  # -----------------------------
  # Initialize output
  # -----------------------------
  x <- rep(NA_real_, n)

  # -----------------------------
  # Process each chain
  # -----------------------------
  for (start in seq_len(k)) {

    idx <- seq(from = start, to = n, by = k)
    known_pos <- idx[!is.na(series[idx])]

    # ❌ Multiple anchors → STOP
    if (length(known_pos) > 1) {
      stop(sprintf(
        "Chain %d has multiple values at positions: %s",
        start,
        paste(known_pos, collapse = ", ")
      ))
    }

    # ⚠️ No anchor → leave NA
    if (length(known_pos) == 0) {
      message(sprintf(
        "Chain %d: no known value → returning NA for this chain",
        start
      ))
      next
    }

    # -----------------------------
    # Anchor point
    # -----------------------------
    pos <- known_pos[1]
    x[pos] <- series[pos]

    # -----------------------------
    # Forward reconstruction
    # -----------------------------
    for (t in idx[idx > pos]) {
      if (!is.na(gr[t]) && !is.na(x[t - k])) {
        x[t] <- x[t - k] * (1 + gr[t])
      }
    }

    # -----------------------------
    # Backward reconstruction
    # -----------------------------
    for (t in rev(idx[idx < pos])) {
      if (!is.na(gr[t + k]) && !is.na(x[t + k])) {
        x[t] <- x[t + k] / (1 + gr[t + k])
      }
    }
  }

  return(x)
}
