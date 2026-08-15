#' Reconstruct a series from its cumulative sum
#'
#' Reconstructs the original series from a cumulative sum series.
#'
#' @param x_cum A numeric vector containing cumulative sums, usually
#'   created using `cumsum_series()`.
#'
#' @return A numeric vector with the same length as `x_cum`
#'   containing the reconstructed original series.
#'
#' @examples
#' x <- c(100, 8, 7, 8, 7)
#' x_cum <- cumsum_series(x)
#'
#' de_cumsum_series(x_cum)
#'
#' @export
de_cumsum_series <- function(x_cum) {

  if (!is.numeric(x_cum)) {
    stop("`x_cum` must be a numeric vector.")
  }

  c(x_cum[1], diff(x_cum))
}
