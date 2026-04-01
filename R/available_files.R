#' List Available Files with Metadata
#'
#' This function scans a directory and returns a data frame containing
#' file metadata including date, file name, extension, and full file name.
#' It supports file names with an optional date prefix in the format
#' "dd-mm-yyyy - filename.ext".
#'
#' @param path Character. Directory containing the files.
#'
#' @return A data frame with columns:
#' \itemize{
#'   \item date: Date extracted from file name (NA if not present)
#'   \item file_name: Name of the file without extension
#'   \item extension: File extension
#'   \item full_name: Full file name
#' }
#'
#' @details
#' If a file does not contain a valid date prefix, the date column will be NA.
#' The function expects date format as "dd-mm-yyyy".
#'
#' @examples
#' available_files("input")
#'
#' @importFrom lubridate dmy
#' @export
available_files <- function(path) {

  if (!dir.exists(path)) {
    stop("Directory does not exist", call. = FALSE)
  }

  files <- list.files(path)

  date <- character(length(files))
  file_name <- character(length(files))
  extension <- character(length(files))
  full_name <- files

  for (i in seq_along(files)) {

    parts <- strsplit(files[i], " - ")[[1]]

    # Check if date exists
    if (length(parts) > 1 && grepl("^\\d{2}-\\d{2}-\\d{4}$", parts[1])) {
      date[i] <- parts[1]
      name_ext <- parts[2]
    } else {
      date[i] <- NA
      name_ext <- parts[length(parts)]
    }

    # Split name and extension
    name_parts <- strsplit(name_ext, "\\.")[[1]]

    file_name[i] <- name_parts[1]
    extension[i] <- ifelse(length(name_parts) > 1,
                           name_parts[length(name_parts)],
                           NA)
  }

  data.frame(
    date = lubridate::dmy(date),
    file_name = file_name,
    extension = extension,
    full_name = full_name,
    stringsAsFactors = FALSE
  )
}
