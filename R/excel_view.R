#' View a Data Frame in Excel
#'
#' Exports a data frame to CSV or Excel and opens it automatically.
#'
#' @param df A data frame or tibble
#' @param format Output format: "csv" or "xlsx"
#'
#' @return Opens the file in Excel
#' @export
excel_view <- function(df, format = c("csv", "xlsx")) {
  format <- match.arg(format)

  dir_path <- file.path(getwd(), "temp")
  if (!dir.exists(dir_path)) dir.create(dir_path)

  file <- file.path(
    dir_path,
    paste0(
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
      "_temp.",
      format
    )
  )

  if (format == "csv") {
    readr::write_csv(df, file)
  } else if (format == "xlsx") {
    writexl::write_xlsx(df, file)
  }

  shell.exec(normalizePath(file))
}
