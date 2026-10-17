#' Get Latest File Based on Date Prefix
#'
#' This function retrieves the latest file from a directory based on
#' a date prefix in the format "dd-mm-yyyy - filename". It optionally
#' filters files by dataset name and/or file extension.
#'
#' @param path Character. Directory containing the files.
#' @param name Character (optional). Filter files by dataset name (partial match, case-insensitive).
#' @param ext Character (optional). Filter files by file extension (e.g., "csv", "txt").
#'
#' @return A character string representing the full path of the latest file.
#'
#' @details
#' The function expects file names in the format:
#' "dd-mm-yyyy - filename.ext".
#'
#' If multiple files share the latest date, the function will stop with an error
#' unless further filtering is applied using \code{name} or \code{ext}.
#'
#' @examples
#' # Get latest file
#' get_latest_data("input")
#'
#' # Filter by dataset name
#' get_latest_data("input", name = "iris")
#'
#' # Filter by extension
#' get_latest_data("input", ext = "csv")
#'
#' @export

get_latest_data <- function(path, name = NULL, ext = NULL) {

  # Get all files with full path
  files <- list.files(
    path = path,
    full.names = TRUE
  )

  if (length(files) == 0) {
    stop("No files found in the specified path.", call. = FALSE)
  }

  # Get only file names
  file_names <- basename(files)

  # Split file names into date and dataset name
  split_data <- strsplit(file_names, " - ")

  # Extract dates
  dates <- sapply(split_data, function(x) x[1])

  # Convert dates
  dates <- as.Date(
    dates,
    format = "%d-%m-%Y"
  )

  # Filter by dataset name
  if (!is.null(name)) {

    dataset_names <- sapply(
      split_data,
      function(x) x[2]
    )

    dataset_names <- sub(
      "\\..*$",
      "",
      dataset_names
    )

    idx <- grepl(
      name,
      dataset_names,
      ignore.case = TRUE
    )

    files <- files[idx]
    dates <- dates[idx]
  }

  # Filter by extension
  if (!is.null(ext)) {

    file_names <- basename(files)

    idx <- grepl(
      paste0("\\.", ext, "$"),
      file_names,
      ignore.case = TRUE
    )

    files <- files[idx]
    dates <- dates[idx]
  }

  # Check whether files remain
  if (length(files) == 0) {
    stop(
      "No files found matching the specified criteria.",
      call. = FALSE
    )
  }

  # Find the latest date
  latest_date <- max(
    dates,
    na.rm = TRUE
  )

  # Get file corresponding to latest date
  latest_file <- files[dates == latest_date]

  # Handle multiple files with same latest date
  if (length(latest_file) > 1) {

    message("Multiple files found with the latest date:")

    message(
      paste(latest_file, collapse = "\n")
    )

    stop(
      "Please specify 'name' or 'ext' to narrow down the files.",
      call. = FALSE
    )
  }

  # Print and return the file
  message(
    paste0("Loading: ", latest_file)
  )

  return(latest_file)
}


