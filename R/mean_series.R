#' Create a cumulative mean series
#'
#' Calculates the cumulative arithmetic mean of a numeric series.
#' Missing values can optionally be excluded from the calculation.
#'
#' @param x A numeric vector or time series.
#' @param na.rm Logical value indicating whether missing values should
#'   be removed from the calculation. Defaults to `FALSE`.
#'
#' @return A numeric vector containing the cumulative mean at each
#'   position of `x`.
#'
#' @examples
#' x <- c(100, 108, 115, 123, 130)
#' mean_series(x)
#'
#' x_na <- c(100, 108, NA, 123, 130)
#' mean_series(x_na, na.rm = TRUE)
#'
#' @export
mean_series <- function(x, na.rm = FALSE) {

  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.")
  }

  if (!is.logical(na.rm) || length(na.rm) != 1 || is.na(na.rm)) {
    stop("`na.rm` must be TRUE or FALSE.")
  }

  if (na.rm) {
    count <- cumsum(!is.na(x))
    x[is.na(x)] <- 0

    result <- cumsum(x) / count

    result[count == 0] <- NA_real_

    result
  } else {
    cumsum(x) / seq_along(x)
  }
}
