#' Fill Missing Values Using a Repeating Pattern
#'
#' Detects a repeating pattern in a vector and uses the detected pattern
#' to replace missing (`NA`) values.
#'
#' @param x A vector containing observed values and possibly `NA` values.
#' @param max_pattern_length Maximum length of the repeating pattern to test.
#' @param min_repetitions Minimum number of repetitions required when searching
#'   for a pattern.
#' @param print_pattern Logical; if `TRUE`, prints the detected pattern and
#'   its match percentage.
#'
#' @return A vector of the same length as `x`, with `NA` values replaced
#'   according to the detected repeating pattern. If no suitable pattern
#'   can be detected, the original vector is returned unchanged.
#'
#' @details
#' The function tests possible repeating pattern lengths up to
#' `max_pattern_length`. For each pattern position, the first available
#' non-missing value is used to construct the candidate pattern. The candidate
#' pattern is then compared with the observed values, ignoring `NA` values.
#'
#' The pattern with the highest match percentage is selected. If multiple
#' patterns have the same match percentage, the shortest pattern is preferred.
#'
#' Only missing values are replaced. Existing non-missing values are never
#' modified.
#'
#' If all values are missing, or no suitable pattern can be constructed,
#' the original vector is returned.
#'
#' @examples
#' x <- c("A", "B", "C", "D", "A", NA, "C", "D")
#' fill_pattern(x, print_pattern = TRUE)
#'
#' x <- c(10, 20, 30, 10, NA, 30, 10, 20)
#' fill_pattern(x)
#'
#' @export
fill_pattern <- function(x,
                         max_pattern_length = 10,
                         min_repetitions = 2,
                         print_pattern = FALSE) {

  # ------------------------------------------------------------
  # Basic checks
  # ------------------------------------------------------------

  if (length(x) == 0) {
    return(x)
  }

  if (!is.numeric(max_pattern_length) ||
      length(max_pattern_length) != 1 ||
      is.na(max_pattern_length) ||
      max_pattern_length < 1 ||
      max_pattern_length != floor(max_pattern_length)) {
    stop("`max_pattern_length` must be a positive integer.")
  }

  if (!is.numeric(min_repetitions) ||
      length(min_repetitions) != 1 ||
      is.na(min_repetitions) ||
      min_repetitions < 1 ||
      min_repetitions != floor(min_repetitions)) {
    stop("`min_repetitions` must be a positive integer.")
  }

  if (!is.logical(print_pattern) ||
      length(print_pattern) != 1 ||
      is.na(print_pattern)) {
    stop("`print_pattern` must be TRUE or FALSE.")
  }

  if (!anyNA(x)) {
    if (print_pattern) {
      message("No NA values found.")
    }
    return(x)
  }

  if (all(is.na(x))) {
    if (print_pattern) {
      message("No pattern can be detected: all values are NA.")
    }
    return(x)
  }

  n <- length(x)

  # Maximum pattern length cannot exceed the number
  # of observations available for the requested repetitions.
  max_pattern_length <- min(
    max_pattern_length,
    floor(n / min_repetitions)
  )

  if (max_pattern_length < 1) {
    if (print_pattern) {
      message("No suitable pattern found.")
    }
    return(x)
  }

  # ------------------------------------------------------------
  # Store the best pattern
  # ------------------------------------------------------------

  best_pattern <- NULL
  best_length <- NULL
  best_score <- -Inf

  # ------------------------------------------------------------
  # Test every possible pattern length
  # ------------------------------------------------------------

  for (p in seq_len(max_pattern_length)) {

    # Need enough observed values.
    if (sum(!is.na(x)) < p * min_repetitions) {
      next
    }

    # ----------------------------------------------------------
    # Construct candidate pattern
    # ----------------------------------------------------------

    pattern <- vector("list", p)

    valid <- TRUE

    for (j in seq_len(p)) {

      # Positions corresponding to this pattern position.
      positions <- seq(j, n, by = p)

      values <- x[positions]
      values <- values[!is.na(values)]

      if (length(values) == 0) {
        valid <- FALSE
        break
      }

      # Use the first observed value as the candidate
      # for this pattern position.
      pattern[[j]] <- values[1]
    }

    if (!valid) {
      next
    }

    pattern <- unlist(pattern, use.names = FALSE)

    # ----------------------------------------------------------
    # Generate expected sequence
    # ----------------------------------------------------------

    expected <- pattern[
      ((seq_len(n) - 1) %% p) + 1
    ]

    # ----------------------------------------------------------
    # Calculate similarity using only observed values
    # ----------------------------------------------------------

    observed <- !is.na(x)

    matches <- x[observed] == expected[observed]

    score <- mean(matches)

    # ----------------------------------------------------------
    # Select best pattern
    #
    # Priority 1: Highest similarity
    # Priority 2: Shortest pattern when similarity is equal
    # ----------------------------------------------------------

    if (score > best_score ||
        (score == best_score &&
         (is.null(best_length) || p < best_length))) {

      best_score <- score
      best_pattern <- pattern
      best_length <- p
    }
  }

  # ------------------------------------------------------------
  # No pattern found
  # ------------------------------------------------------------

  if (is.null(best_pattern)) {

    if (print_pattern) {
      message("No suitable pattern found.")
    }

    return(x)
  }

  # ------------------------------------------------------------
  # Print detected pattern
  # ------------------------------------------------------------

  if (print_pattern) {

    message(
      "Detected pattern: ",
      paste(best_pattern, collapse = ", ")
    )

    message(
      "Pattern match: ",
      round(best_score * 100, 2),
      "%"
    )
  }

  # ------------------------------------------------------------
  # Generate final expected sequence
  # ------------------------------------------------------------

  expected <- best_pattern[
    ((seq_len(n) - 1) %% best_length) + 1
  ]

  # ------------------------------------------------------------
  # Fill only NA values
  # ------------------------------------------------------------

  x[is.na(x)] <- expected[is.na(x)]

  return(x)
}
