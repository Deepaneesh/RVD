#' Trim whitespace and restore column types
#'
#' Trims leading and trailing whitespace from all columns in a data frame.
#' Numeric columns are converted back to numeric and factor columns are
#' converted back to factors after trimming.
#'
#' @param data A data.frame or tibble.
#'
#' @return A data frame with trimmed character values and restored column types.
#'
#' @examples
#' df <- data.frame(
#'   name = c("  John ", " Mary "),
#'   age = c(" 23 ", " 30 "),
#'   gender = factor(c(" M ", " F "))
#' )
#'
#' trimed_data(df)
#'
#' @export
trimed_data <- function(data) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.")
  }

  numeric_var <- names(dplyr::select(data, where(is.numeric)))
  factor_var  <- names(dplyr::select(data, where(is.factor)))

  trim_data <- data %>%
    dplyr::mutate(dplyr::across(everything(), ~ as.character(.))) %>%
    dplyr::mutate(dplyr::across(everything(), ~ stringr::str_trim(.))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(factor_var), as.factor)) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(numeric_var), as.numeric))

  return(trim_data)
}
