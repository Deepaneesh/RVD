#' Fill Missing Values Using Median
#'
#' Fills missing values using either a constant median or a cumulative
#' series median.
#'
#' The \code{"constant"} method calculates one overall median from all
#' non-missing observations and uses that value to fill missing values.
#'
#' The \code{"series"} method calculates a cumulative median at each
#' observation using all available non-missing observations up to that
#' position. Missing values can then be replaced using the corresponding
#' cumulative median.
#'
#' @param x A numeric vector containing the values to be filled.
#' @param type Method used for filling missing values. Must be either
#'   \code{"series"} or \code{"constant"}. The default is \code{"series"}.
#' @param inside Logical. If \code{TRUE}, fill missing values occurring
#'   between the first and last non-missing observations. Default is
#'   \code{TRUE}.
#' @param outside Logical. If \code{TRUE}, fill missing values occurring
#'   before the first or after the last non-missing observation.
#'   Default is \code{FALSE}.
#' @param back Logical. For \code{type = "constant"}, if \code{TRUE},
#'   fill all remaining missing values using the overall median.
#'   This argument cannot be \code{TRUE} when \code{type = "series"}.
#'   Default is \code{FALSE}.
#' @param fitted Logical. If \code{TRUE}, return only the fitted median
#'   values instead of the original vector with missing values filled.
#'   For \code{type = "constant"}, the fitted values are the same overall
#'   median for every observation. For \code{type = "series"}, the fitted
#'   values are the cumulative medians. Default is \code{FALSE}.
#'
#' @return
#' A numeric vector.
#'
#' If \code{fitted = FALSE}, the function returns the original vector
#' with the requested missing values replaced.
#'
#' If \code{fitted = TRUE}, the function returns the median fitted values
#' for every observation.
#'
#' @details
#' For \code{type = "constant"}, the median is calculated as:
#'
#' \deqn{median(x, na.rm = TRUE)}
#'
#' The same median is used for all values that are selected for filling.
#'
#' For \code{type = "series"}, the fitted value at position \eqn{i} is
#' calculated using all non-missing observations from position 1 through
#' \eqn{i}. This produces a cumulative median series.
#'
#' The \code{back = TRUE} option is not supported for the series method
#' because the series method depends on previous observations to calculate
#' the cumulative median.
#'
#' @examples
#' # Constant median method
#' x <- c(10, 20, NA, 40, 50)
#'
#' fill_median(
#'   x,
#'   type = "constant"
#' )
#'
#' # Constant median with outside missing values
#' x <- c(NA, 10, 20, NA, 40, NA)
#'
#' fill_median(
#'   x,
#'   type = "constant",
#'   inside = TRUE,
#'   outside = TRUE
#' )
#'
#' # Series median method
#' x <- c(10, 20, NA, 40, 50)
#'
#' fill_median(
#'   x,
#'   type = "series"
#' )
#'
#' # Return cumulative fitted median values
#' fill_median(
#'   x,
#'   type = "series",
#'   fitted = TRUE
#' )
#'
#' # Return constant fitted median values
#' fill_median(
#'   x,
#'   type = "constant",
#'   fitted = TRUE
#' )
#'
#' @export
fill_median <- function(
    x,
    type = "series",
    inside = TRUE,
    outside = FALSE,
    back = FALSE,
    fitted = FALSE) {

  # ------------------------------------------------------------
  # Validate type
  # ------------------------------------------------------------

  type <- match.arg(type, c("series", "constant"))

  # ------------------------------------------------------------
  # Series method cannot use back
  # ------------------------------------------------------------

  if (type == "series" && back) {
    stop(
      "'back = TRUE' cannot be used when 'type = \"series\"'. ",
      "The series method uses previous observations to calculate ",
      "the cumulative median.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # Convert to numeric
  # ------------------------------------------------------------

  x <- as.numeric(x)

  n <- length(x)

  # ============================================================
  # CONSTANT METHOD
  # ============================================================

  if (type == "constant") {

    # Overall median
    median_value <- median(x, na.rm = TRUE)

    # Fitted values
    fitted_values <- rep(median_value, n)

    # Return fitted values only
    if (fitted) {
      return(fitted_values)
    }

    result <- x

    # ----------------------------------------------------------
    # Identify NA positions
    # ----------------------------------------------------------

    na_positions <- which(is.na(x))

    if (length(na_positions) == 0) {
      return(result)
    }

    # ----------------------------------------------------------
    # Identify first and last non-NA
    # ----------------------------------------------------------

    non_na <- which(!is.na(x))

    # If all values are NA
    if (length(non_na) == 0) {
      return(result)
    }

    first_non_na <- min(non_na)
    last_non_na <- max(non_na)

    # ----------------------------------------------------------
    # Internal NA
    # ----------------------------------------------------------

    if (inside) {

      inside_idx <- na_positions[
        na_positions > first_non_na &
          na_positions < last_non_na
      ]

      result[inside_idx] <- median_value
    }

    # ----------------------------------------------------------
    # Outside NA
    # ----------------------------------------------------------

    if (outside) {

      outside_idx <- na_positions[
        na_positions < first_non_na |
          na_positions > last_non_na
      ]

      result[outside_idx] <- median_value
    }

    # ----------------------------------------------------------
    # Backward filling
    #
    # For constant method, back also uses the SAME
    # overall median.
    # ----------------------------------------------------------

    if (back) {

      result[is.na(result)] <- median_value
    }

    return(result)
  }

  # ============================================================
  # SERIES METHOD
  # ============================================================

  fitted_values <- rep(NA_real_, n)

  for (i in seq_len(n)) {

    # Values available up to current position
    previous_values <- x[seq_len(i)]

    previous_values <- previous_values[
      !is.na(previous_values)
    ]

    if (length(previous_values) > 0) {
      fitted_values[i] <- median(previous_values)
    }
  }

  # ------------------------------------------------------------
  # Return fitted cumulative median
  # ------------------------------------------------------------

  if (fitted) {
    return(fitted_values)
  }

  result <- x

  # ------------------------------------------------------------
  # Find non-NA range
  # ------------------------------------------------------------

  non_na <- which(!is.na(x))

  if (length(non_na) == 0) {
    return(result)
  }

  first_non_na <- min(non_na)
  last_non_na <- max(non_na)

  # ------------------------------------------------------------
  # Fill NA values
  # ------------------------------------------------------------

  for (i in seq_len(n)) {

    if (!is.na(x[i])) {
      next
    }

    # Internal NA
    if (
      inside &&
      i > first_non_na &&
      i < last_non_na
    ) {
      result[i] <- fitted_values[i]
    }

    # Outside NA
    if (
      outside &&
      (i < first_non_na || i > last_non_na)
    ) {
      result[i] <- fitted_values[i]
    }
  }

  return(result)
}
