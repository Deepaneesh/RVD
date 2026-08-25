#' Calculate Confidence Intervals for a Numeric Vector
#'
#' Calculates standard errors and confidence intervals for each value
#' in a numeric vector. Two methods are available: a constant standard
#' error calculated from the entire vector, or a series of local standard
#' errors calculated using neighboring observations.
#'
#' @param x A numeric vector.
#' @param type Character string specifying the confidence interval method.
#'   Must be either `"constant"` or `"series"`.
#' @param confidence Numeric vector containing confidence levels between
#'   0 and 1. Default is `c(0.95, 0.90)`.
#'
#' @return A data frame containing:
#' \describe{
#'   \item{value}{The original values of `x`.}
#'   \item{se}{The standard error used for each observation.}
#'   \item{lower_XX}{Lower confidence limit for the specified confidence level.}
#'   \item{upper_XX}{Upper confidence limit for the specified confidence level.}
#' }
#'
#' @details
#' When `type = "constant"`, a single standard error is calculated from
#' the standard deviation of the complete vector and is applied to every
#' observation.
#'
#' When `type = "series"`, a local standard error is calculated for each
#' observation. The first observation uses the first two observations,
#' the last observation uses the last two observations, and intermediate
#' observations use the preceding, current, and following observations.
#'
#' Confidence intervals are calculated using the normal approximation:
#' \deqn{x_i \pm z_{\alpha/2} SE_i}
#'
#' @examples
#' x <- c(10, 12, 15, 14, 18, 20)
#'
#' ci_vector(x, type = "constant")
#'
#' ci_vector(x, type = "series")
#'
#' ci_vector(x, type = "series", confidence = c(0.95, 0.80))
#'
#' @export
ci_vector <- function(x,
                      type = c("constant", "series"),
                      confidence = c(.95, .90)) {

  type <- match.arg(type)

  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  if (length(x) < 2) {
    stop("x must contain at least two observations.")
  }

  if (any(!is.finite(x))) {
    stop("x must contain only finite values.")
  }

  if (any(confidence <= 0 | confidence >= 1)) {
    stop("confidence values must be between 0 and 1.")
  }

  if (type == "constant") {

    n <- length(x)

    sd_x <- sd(x)

    se <- sd_x / sqrt(n)

    result <- data.frame(
      value = x,
      se = rep(se, n)
    )

    for (conf in confidence) {

      z <- qnorm(1 - (1 - conf) / 2)

      level <- round(conf * 100)

      result[[paste0("lower_", level)]] <-
        x - z * se

      result[[paste0("upper_", level)]] <-
        x + z * se
    }

  }

  if (type == "series") {

    n <- length(x)

    se <- rep(NA_real_, n)

    for (i in seq_len(n)) {

      if (i == 1) {

        local_x <- x[1:2]

      } else if (i == n) {

        local_x <- x[(n - 1):n]

      } else {

        local_x <- x[(i - 1):(i + 1)]
      }

      se[i] <- sd(local_x) / sqrt(length(local_x))
    }

    result <- data.frame(
      value = x,
      se = se
    )

    for (conf in confidence) {

      z <- qnorm(1 - (1 - conf) / 2)

      level <- round(conf * 100)

      result[[paste0("lower_", level)]] <-
        x - z * se

      result[[paste0("upper_", level)]] <-
        x + z * se
    }
  }

  return(result)
}
