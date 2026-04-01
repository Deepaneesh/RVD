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

  files <- list.files(path, full.names = TRUE)
  file_names <- basename(files)

  split_data <- strsplit(file_names, " - ")

  dates <- sapply(split_data, function(x) x[1])
  dataset_names <- sapply(split_data, function(x) x[2])

  dataset_names_clean <- sub("\\..*$", "", dataset_names)

  dates <- as.Date(dates, format = "%d-%m-%Y")

  # Filter by dataset name
  if (!is.null(name)) {
    idx <- grepl(name, dataset_names_clean, ignore.case = TRUE)

    files <- files[idx]
    dates <- dates[idx]
    dataset_names_clean <- dataset_names_clean[idx]
  }

  # Filter by extension
  if (!is.null(ext)) {
    idx <- grepl(paste0("\\.", ext, "$"), file_names, ignore.case = TRUE)

    files <- files[idx]
    dates <- dates[idx]
    dataset_names_clean <- dataset_names_clean[idx]
  }

  if (length(files) == 0) {
    stop("No files found matching criteria", call. = FALSE)
  }

  # Get latest date
  max_date <- max(dates, na.rm = TRUE)
  idx <- which(dates == max_date)

  latest_files <- files[idx]

  if (length(latest_files) > 1) {
    message("Multiple files found with the latest date:")
    message(paste(latest_files, collapse = "\n"))
    stop("Please specify 'name' or 'ext' to narrow down.", call. = FALSE)
  }

  message(paste0("Loading: ", latest_files))

  return(latest_files)
}
