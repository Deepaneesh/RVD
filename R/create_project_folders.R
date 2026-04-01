#' Create Standard Project Folders
#'
#' This function creates a set of standard project directories if they do not already exist.
#' By default, it creates folders for input, output, logs, and archive.
#'
#' @param dirs Character vector. Names of directories to create.
#' Default is c("input", "output", "logs", "archive").
#'
#' @return Invisibly returns NULL. Creates directories as a side effect.
#'
#' @details
#' The function checks whether each directory exists before creating it.
#' If a directory does not exist, it will be created recursively.
#'
#' @examples
#' # Create default folders
#' create_project_folders()
#'
#' # Create custom folders
#' create_project_folders(c("data", "results", "temp"))
#'
#' @export
create_project_folders <- function(dirs = c("input", "output", "logs", "archive")) {

  for (d in dirs) {
    if (!dir.exists(d)) {
      message(paste0("Creating directory: ", d))
      dir.create(d, recursive = TRUE)
    }
  }

  invisible(NULL)
}
