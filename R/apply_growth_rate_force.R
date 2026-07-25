#' Apply Growth Rate by Forcing
#'
#' Applies growth rates to fill missing values (`NA`) in a numeric vector by
#' propagating observed values from the left, right, or both directions.
#'
#' When forcing from the left, each missing value is estimated using the most
#' recent available value and the corresponding growth rate. When forcing from
#' the right, missing values are estimated by reversing the growth-rate
#' calculation from the next available value.
#'
#' Original (non-missing) observations are never modified.
#'
#' @param x A numeric vector containing observations and missing values (`NA`).
#' @param growth_rate A numeric vector of growth rates. Must have the same
#'   length as `x`.
#' @param force_from_left Logical. Should missing values be filled by propagating
#'   values from left to right? Default is `FALSE`.
#' @param force_from_right Logical. Should missing values be filled by
#'   propagating values from right to left? Default is `FALSE`.
#'
#' @return
#' A numeric vector of the same length as `x` with missing values filled where
#' possible. Original non-missing observations remain unchanged.
#'
#' @examples
#' x <- c(NA, NA, 3, 4, NA, 6, 5, NA, NA, 3, NA, NA)
#' growth_rate <- rep(0.05, length(x))
#'
#' apply_growth_rate_force(
#'   x,
#'   growth_rate,
#'   force_from_left = TRUE
#' )
#'
#' apply_growth_rate_force(
#'   x,
#'   growth_rate,
#'   force_from_right = TRUE
#' )
#'
#' apply_growth_rate_force(
#'   x,
#'   growth_rate,
#'   force_from_left = TRUE,
#'   force_from_right = TRUE
#' )
#'
#' @export
apply_growth_rate_force <- function(
    x,
    growth_rate,
    force_from_left = FALSE,
    force_from_right = FALSE) {
  #----------------------------#
  # Validation
  #----------------------------#

  if (length(x) != length(growth_rate)) {
    stop("'x' and 'growth_rate' must have the same length.")
  }

  if (!is.logical(force_from_left) || length(force_from_left) != 1) {
    stop("'force_from_left' must be TRUE or FALSE.")
  }

  if (!is.logical(force_from_right) || length(force_from_right) != 1) {
    stop("'force_from_right' must be TRUE or FALSE.")
  }

  if (!force_from_left && !force_from_right) {
    return(x)
  }

  #----------------------------#
  # Initialization
  #----------------------------#

  original <- !is.na(x)

  left_fill <- x
  right_fill <- x

  #----------------------------#
  # Force from Left
  #----------------------------#

  if (force_from_left) {
    for (i in seq_len(length(x) - 1)) {
      if (!is.na(left_fill[i])) {
        j <- i + 1

        while (j <= length(x) && !original[j]) {
          left_fill[j] <-
            left_fill[j - 1] * (1 + growth_rate[j])

          j <- j + 1
        }
      }
    }
  }

  #----------------------------#
  # Force from Right
  #----------------------------#

  if (force_from_right) {
    for (i in seq(length(x), 2)) {
      if (!is.na(right_fill[i])) {
        j <- i - 1

        while (j >= 1 && !original[j]) {
          right_fill[j] <-
            right_fill[j + 1] /
              (1 + growth_rate[j + 1])

          j <- j - 1
        }
      }
    }
  }

  #----------------------------#
  # Final Result
  #----------------------------#

  x_new <- x

  missing <- which(!original)

  for (i in missing) {
    if (force_from_left &&
      force_from_right &&
      !is.na(left_fill[i]) &&
      !is.na(right_fill[i])) {
      x_new[i] <- mean(c(left_fill[i], right_fill[i]))
    } else if (force_from_left &&
      !is.na(left_fill[i])) {
      x_new[i] <- left_fill[i]
    } else if (force_from_right &&
      !is.na(right_fill[i])) {
      x_new[i] <- right_fill[i]
    }
  }

  #----------------------------#
  # Preserve Original Values
  #----------------------------#

  x_new[original] <- x[original]

  return(x_new)
}
