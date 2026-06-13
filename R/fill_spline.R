#' Fill Missing Values Using Spline Interpolation
#'
#' Fills missing values in a numeric vector using spline interpolation.
#' Missing values between observed values are interpolated, while missing
#' values outside the observed range can optionally be extrapolated.
#'
#' @param x A numeric vector containing missing values (`NA`).
#' @param inside Logical. Should missing values between observed values
#'   be filled using spline interpolation? Default is `TRUE`.
#' @param outside Logical. Should missing values outside the observed
#'   range be filled using spline extrapolation? Default is `TRUE`.
#' @param back Logical. Should backward extrapolation be performed for
#'   leading missing values? Default is `FALSE`.
#' @param method Character string specifying the spline method passed to
#'   [stats::splinefun()]. Common options are `"natural"`, `"fmm"`,
#'   `"periodic"`, `"monoH.FC"`, and `"hyman"`.
#'
#' @return
#' A numeric vector with missing values filled using spline interpolation
#' and/or extrapolation.
#'
#' @details
#' The function first constructs a spline function using the non-missing
#' observations in `x`.
#'
#' - If `inside = TRUE`, missing values between the first and last
#'   observed values are interpolated.
#' - If `outside = TRUE`, missing values beyond the observed range are
#'   extrapolated.
#' - If `back = TRUE`, leading missing values are also extrapolated.
#'
#' At least two non-missing observations are required.
#'
#' @examples
#' x <- c(10, NA, NA, 25, 30)
#' fill_spline(x)
#'
#' x <- c(NA, NA, 10, 20, NA, 35, NA)
#' fill_spline(x, back = TRUE)
#'
#' x <- c(5, NA, NA, 20)
#' fill_spline(x, method = "fmm")
#'
#' @export
fill_spline <- function(x,
                        inside = TRUE,
                        outside = TRUE,
                        back = FALSE,
                        method = "natural") {

  x <- as.numeric(x)
  n <- length(x)
  idx <- which(!is.na(x))

  if (length(idx) < 2) {
    stop("At least two non-NA values are required for spline filling.")
  }

  y <- x

  sf <- stats::splinefun(
    x = idx,
    y = x[idx],
    method = method
  )

  if (inside) {

    mid_range <- idx[1]:idx[length(idx)]

    na_inside <- which(
      is.na(x) &
        seq_len(n) %in% mid_range
    )

    y[na_inside] <- sf(na_inside)
  }

  if (outside) {

    if (back && idx[1] > 1) {

      left_idx <- seq_len(idx[1] - 1)

      y[left_idx] <- sf(left_idx)
    }

    if (idx[length(idx)] < n) {

      right_idx <- (idx[length(idx)] + 1):n

      y[right_idx] <- sf(right_idx)
    }
  }

  y
}
