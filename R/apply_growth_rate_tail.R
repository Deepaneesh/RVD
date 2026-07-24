#' Apply Growth Rates to Leading and Trailing Missing Values
#'
#' Applies growth rates to fill leading and/or trailing missing values in a
#' numeric vector while preserving all existing observations. Leading missing
#' values are reconstructed by reversing the growth rates, whereas trailing
#' missing values are projected forward using the supplied growth rates.
#'
#' @param x A numeric vector containing leading and/or trailing `NA` values.
#' Existing non-missing values are preserved.
#' @param growth_rate A numeric vector of growth rates with the same length as
#' `x`. Each value represents the growth rate associated with the corresponding
#' position.
#' @param left_tail Logical. Should leading `NA` values be filled?
#' Default is `TRUE`.
#' @param right_tail Logical. Should trailing `NA` values be filled?
#' Default is `TRUE`.
#'
#' @return
#' A numeric vector with leading and/or trailing `NA` values replaced using the
#' supplied growth rates. Existing non-missing values remain unchanged.
#'
#' @details
#' For the left tail, values are reconstructed using
#'
#' \deqn{x_i = \frac{x_{i+1}}{1 + g_{i+1}}}
#'
#' For the right tail, values are projected using
#'
#' \deqn{x_i = x_{i-1}(1 + g_i)}
#'
#' where \eqn{g_i} is the growth rate at position \eqn{i}.
#'
#' @examples
#' gr <- c(0.2, 0.3, 0.4, 0.2, 0.1, 0.4, 0.3, 0.2)
#'
#' x <- c(NA, NA, NA, 2, 3, NA, NA, NA)
#'
#' apply_growth_rate_tail(x, gr)
#'
#' apply_growth_rate_tail(
#'   x,
#'   gr,
#'   left_tail = TRUE,
#'   right_tail = FALSE
#' )
#'
#' apply_growth_rate_tail(
#'   x,
#'   gr,
#'   left_tail = FALSE,
#'   right_tail = TRUE
#' )
#'
#' @export
apply_growth_rate_tail <- function(
    x,
    growth_rate,
    left_tail = TRUE,
    right_tail = TRUE) {
  if (length(x) != length(growth_rate)) {
    stop("'x' and 'growth_rate' must have the same length.")
  }

  if (!left_tail && !right_tail) {
    return(x)
  }

  x_new <- x

  non_na <- which(!is.na(x))

  if (length(non_na) == 0) {
    stop("'x' contains only NA values.")
  }

  if (left_tail) {
    first_non_na <- min(non_na)

    if (first_non_na > 1) {
      for (i in seq(first_non_na - 1, 1)) {
        x_new[i] <- x_new[i + 1] /
          (1 + growth_rate[i + 1])
      }
    }
  }

  if (right_tail) {
    last_non_na <- max(non_na)

    if (last_non_na < length(x)) {
      for (i in (last_non_na + 1):length(x)) {
        x_new[i] <- x_new[i - 1] *
          (1 + growth_rate[i])
      }
    }
  }

  x_new
}
