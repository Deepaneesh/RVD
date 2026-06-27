#' Split a Time Series into Training and Testing Sets
#'
#' Splits a time series into training and testing subsets using either
#' the proportion of observations for the training set or the number of
#' observations in the testing set.
#'
#' @param data A time series object (`ts`) or numeric vector.
#' @param prop Proportion of observations to include in the training set.
#' Must be between 0 and 1.
#' @param n Number of observations in the testing set.
#'
#' @return A list containing:
#' \describe{
#'   \item{train_data}{Training data.}
#'   \item{test_data}{Testing data.}
#' }
#'
#' @examples
#' # Split using 80% training data
#' split <- ts_split(AirPassengers, prop = 0.8)
#'
#' # Split using the last 12 observations as testing data
#' split <- ts_split(AirPassengers, n = 12)
#'
#' @export

ts_split <- function(data, prop = NULL, n = NULL) {

  # Check input type
  if (!inherits(data, "ts") && !is.numeric(data)) {
    stop("'data' must be a ts object or a numeric vector.",
         call. = FALSE)
  }

  # Only one of prop or n should be supplied
  if (!is.null(prop) && !is.null(n)) {
    stop("Specify only one of 'prop' or 'n', not both.",
         call. = FALSE)
  }

  # At least one of prop or n should be supplied
  if (is.null(prop) && is.null(n)) {
    stop("Either 'prop' or 'n' must be specified.",
         call. = FALSE)
  }

  N <- length(data)

  if (!is.null(prop)) {

    if (!is.numeric(prop) ||
        length(prop) != 1 ||
        prop <= 0 ||
        prop >= 1) {
      stop("'prop' must be a single numeric value between 0 and 1.",
           call. = FALSE)
    }

    train_n <- floor(prop * N)

  }

  if (!is.null(n)) {

    if (!is.numeric(n) ||
        length(n) != 1 ||
        n < 1 ||
        n >= N) {
      stop("'n' must be an integer between 1 and length(data) - 1.",
           call. = FALSE)
    }

    train_n <- N - as.integer(n)
  }

  train_data <- data[1:train_n]
  test_data  <- data[(train_n + 1):N]

  # Preserve ts attributes
  if (inherits(data, "ts")) {

    train_data <- ts(
      train_data,
      start = start(data),
      frequency = frequency(data)
    )

    test_data <- ts(
      test_data,
      start = time(data)[train_n + 1],
      frequency = frequency(data)
    )
  }

  return(list(
    train_data = train_data,
    test_data = test_data
  ))
}
