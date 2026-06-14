#' Delete One or More Folders
#'
#' Deletes one or more folders and their contents recursively.
#'
#' @param path A character vector of folder paths to delete.
#'   Defaults to `"temp"`.
#' @param force Logical. Should read-only files be removed if possible?
#'   Defaults to `TRUE`.
#'
#' @return Invisibly returns a logical vector indicating whether each
#'   folder was successfully deleted.
#'
#' @examples
#' \dontrun{
#' delete_folder()
#'
#' delete_folder("my_folder")
#'
#' delete_folder(c("folder1", "folder2"))
#' }
#'
#' @export
delete_folder <- function(path = "temp", force = TRUE) {

  invisible(sapply(path, function(p) {

    if (!dir.exists(p)) {
      message("Not available: ", p)
      return(FALSE)
    }

    deleted <- unlink(
      p,
      recursive = TRUE,
      force = force
    ) == 0

    if (deleted) {
      message("Deleted: ", p)
    }

    deleted
  }))
}
