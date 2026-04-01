#' Save File Name with Current Date
#'
#' This function appends the current system date to a file name
#' in the format "dd-mm-yyyy - filename" and returns the full file path.
#' If the specified directory does not exist, it will be created.
#'
#' @param path Character. Directory where the file will be saved.
#' @param filename Character. Name of the file (with extension).
#'
#' @return A character string representing the full file path with date-prefixed filename.
#'
#' @details
#' The date is added in the format "dd-mm-yyyy".
#' If the directory does not exist, it will be created recursively.
#'
#' @examples
#' save_with_date("input", "iris.csv")
#'
#' @export
save_with_date <- function(path, filename) {

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  today <- format(Sys.Date(), "%d-%m-%Y")

  new_name <- paste0(today, " - ", filename)

  message(paste0("Saving file as: ", new_name))

  return(file.path(path, new_name))
}
