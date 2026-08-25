#' Calculate a Cumulative Median Series
#'
#' Calculates a median series where each value represents the median of
#' all observations from the beginning of the vector up to the current
#' observation.
#'
#' @param x A numeric vector.
#' @param na.rm Logical value indicating whether missing values should
#'   be removed before calculating the median. Default is `TRUE`.
#'
#' @return A numeric vector of the same length as `x`. The ith value is
#'   the median of `x[1:i]`.
#'
#' @details
#' The function calculates a cumulative or expanding median. For example,
#' for the vector `c(10, 20, 30, 5)`, the resulting median series is
#' `c(10, 15, 20, 15)`.
#'
#' @examples
#' x <- c(10, 20, 30, 5, 25)
#'
#' median_series(x)
#'
#' @export
median_series <- function(x, na.rm = TRUE) {

  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  result <- numeric(length(x))

  for (i in seq_along(x)) {

    result[i] <- median(
      x[1:i],
      na.rm = na.rm
    )
  }

  result
}
