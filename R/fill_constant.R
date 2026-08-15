#' Fill a vector when it contains only one unique non-missing value
#'
#' Checks the non-missing values of a vector. If exactly one unique
#' non-missing value is present, all elements of the vector are
#' replaced with that value. If there are zero or more than one
#' unique non-missing values, the original vector is returned
#' unchanged.
#'
#' @param x A vector. Supports numeric, character, logical, Date,
#'   and other vector types that support `is.na()`.
#'
#' @return A vector with the same length as `x`. If exactly one
#'   unique non-missing value exists, all elements are replaced
#'   with that value. Otherwise, `x` is returned unchanged.
#'
#' @examples
#' # Numeric vector with one unique value
#' x <- c(10, NA, 10, NA, 10)
#' fill_constant(x)
#'
#' # Character vector with one unique value
#' x <- c("A", NA, "A", NA, "A")
#' fill_constant(x)
#'
#' # Multiple unique values - returned unchanged
#' x <- c(10, NA, 20, NA)
#' fill_constant(x)
#'
#' # All values are NA - returned unchanged
#' x <- c(NA, NA, NA)
#' fill_constant(x)
#'
#' @export
fill_constant <- function(x) {

  non_na <- x[!is.na(x)]

  unique_values <- unique(non_na)

  if (length(unique_values) != 1) {
    return(x)
  }

  rep(unique_values, length(x))
}
