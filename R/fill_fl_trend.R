#' Fill Left and Right Tails Using a Linear Trend
#'
#' Fits a linear trend using the first and last available (non-missing)
#' observations in a numeric vector. The fitted trend can be used to fill
#' missing values occurring before the first observation, after the last
#' observation, and optionally forecast future values.
#'
#' Interior missing values are left unchanged.
#'
#' @param x A numeric vector.
#' @param fill_right_tail Logical. Should the right tail be filled?
#' @param fill_left_tail Logical. Should the left tail be filled?
#' @param original_and_fitted Logical. If TRUE, returns the original vector
#' with the selected tails filled.
#' @param only_fitted Logical. If TRUE, returns the complete fitted trend,
#' including any forecast values.
#' @param h A non-negative integer specifying the number of forecast values
#' to append.
#'
#' @return
#' Depending on the arguments:
#' \itemize{
#'   \item If \code{original_and_fitted = TRUE}, returns the original vector
#'   with the requested tails filled and optional forecast values appended.
#'   \item If \code{only_fitted = TRUE}, returns the complete fitted trend.
#'   \item Otherwise, returns only the filled tail values.
#' }
#'
#' @export
fill_fl_trend <- function(
    x,
    fill_right_tail = TRUE,
    fill_left_tail = TRUE,
    original_and_fitted = TRUE,
    only_fitted = FALSE,
    h = 0) {
  ## --------------------------------------------------
  ## Input validation
  ## --------------------------------------------------

  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.")
  }

  if (length(x) < 2) {
    stop("'x' must contain at least two elements.")
  }

  if (sum(!is.na(x)) < 2) {
    stop("'x' must contain at least two non-missing values.")
  }

  if (!is.logical(fill_right_tail) || length(fill_right_tail) != 1) {
    stop("'fill_right_tail' must be TRUE or FALSE.")
  }

  if (!is.logical(fill_left_tail) || length(fill_left_tail) != 1) {
    stop("'fill_left_tail' must be TRUE or FALSE.")
  }

  if (!is.logical(original_and_fitted) || length(original_and_fitted) != 1) {
    stop("'original_and_fitted' must be TRUE or FALSE.")
  }

  if (!is.logical(only_fitted) || length(only_fitted) != 1) {
    stop("'only_fitted' must be TRUE or FALSE.")
  }

  if (original_and_fitted && only_fitted) {
    stop("'original_and_fitted' and 'only_fitted' cannot both be TRUE.")
  }

  if (!is.numeric(h) || length(h) != 1 || h < 0 || h %% 1 != 0) {
    stop("'h' must be a non-negative integer.")
  }

  ## --------------------------------------------------
  ## First and last available values
  ## --------------------------------------------------

  idx <- which(!is.na(x))

  first_idx <- idx[1]
  last_idx <- idx[length(idx)]

  first_val <- x[first_idx]
  last_val <- x[last_idx]

  ## --------------------------------------------------
  ## Calculate trend
  ## --------------------------------------------------

  slope <- (last_val - first_val) /
    (last_idx - first_idx)

  intercept <- first_val - slope * first_idx

  fitted <- intercept + slope * seq_len(length(x) + h)

  ## --------------------------------------------------
  ## Return complete fitted values
  ## --------------------------------------------------

  if (only_fitted) {
    return(fitted)
  }

  ## --------------------------------------------------
  ## Return original vector with fitted tails
  ## --------------------------------------------------

  if (original_and_fitted) {
    if (h > 0) {
      out <- c(x, rep(NA_real_, h))
    } else {
      out <- x
    }

    if (fill_left_tail && first_idx > 1) {
      out[1:(first_idx - 1)] <-
        fitted[1:(first_idx - 1)]
    }

    if (fill_right_tail) {
      out[(last_idx + 1):length(out)] <-
        fitted[(last_idx + 1):length(out)]
    }

    return(out)
  }

  ## --------------------------------------------------
  ## Return only filled tails
  ## --------------------------------------------------

  if (!fill_left_tail && !fill_right_tail) {
    return(x)
  }

  result <- numeric(0)

  if (fill_left_tail && first_idx > 1) {
    result <- c(result, fitted[1:(first_idx - 1)])
  }

  if (fill_right_tail && last_idx < length(x)) {
    result <- c(result, fitted[(last_idx + 1):length(x)])
  }

  if (fill_right_tail && h > 0) {
    result <- c(result, fitted[(length(x) + 1):(length(x) + h)])
  }

  return(result)
}
