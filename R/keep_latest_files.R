#' Keep the Latest Files
#'
#' Keeps only the latest `n` files for each filename and extension,
#' and removes older versions.
#'
#' Files must follow the naming convention:
#' \code{DD-MM-YYYY - filename.extension}
#'
#' @param path A character string specifying the directory containing
#'   the files.
#' @param n A positive integer specifying the number of latest files
#'   to retain for each filename. Defaults to `3`.
#'
#' @return Invisibly returns a character vector containing the paths
#'   of the files that were deleted. Returns `NULL` invisibly if no
#'   files are found or no date-based files are available.
#'
#' @details
#' The function identifies dates from the beginning of filenames and
#' groups files by their filename and extension. Within each group,
#' files are ordered by date in descending order. Only the latest
#' `n` files are retained, while older files are deleted.
#'
#' Files that do not contain a valid date in the expected
#' `DD-MM-YYYY` format are ignored.
#'
#' @examples
#' \dontrun{
#' # Keep the latest 3 versions of each file
#' keep_latest_files("data")
#'
#' # Keep the latest 5 versions of each file
#' keep_latest_files("data", n = 5)
#' }
#'
#' @export
keep_latest_files <- function(path, n = 3) {

  if (!dir.exists(path)) {
    stop("The specified path does not exist")
  }

  if (n < 1 || n != as.integer(n)) {
    stop("n must be a positive integer")
  }

  files <- list.files(path, full.names = TRUE)
  files <- files[!dir.exists(files)]

  if (length(files) == 0) {
    message("No files found")
    return(invisible(NULL))
  }

  file_names <- basename(files)

  file_date <- as.Date(
    sub(
      "^([0-9]{2}-[0-9]{2}-[0-9]{4}).*$",
      "\\1",
      file_names
    ),
    format = "%d-%m-%Y"
  )

  valid <- !is.na(file_date)

  files <- files[valid]
  file_names <- file_names[valid]
  file_date <- file_date[valid]

  if (length(files) == 0) {
    message("No date-based files found")
    return(invisible(NULL))
  }

  base_name <- sub(
    "^[0-9]{2}-[0-9]{2}-[0-9]{4} - ",
    "",
    file_names
  )

  groups <- split(seq_along(files), base_name)

  files_to_delete <- character(0)

  for (group in groups) {

    ordered_files <- group[
      order(file_date[group], decreasing = TRUE)
    ]

    if (length(ordered_files) > n) {
      files_to_delete <- c(
        files_to_delete,
        files[ordered_files[(n + 1):length(ordered_files)]]
      )
    }
  }

  if (length(files_to_delete) > 0) {
    file.remove(files_to_delete)
  }

  invisible(files_to_delete)
}
