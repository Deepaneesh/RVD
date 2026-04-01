\name{get_latest_data}
\alias{get_latest_data}
\title{Get Latest File Based on Date Prefix}
\description{
This function retrieves the latest file from a directory based on
a date prefix in the format "dd-mm-yyyy - filename". It optionally
filters files by dataset name and/or file extension.
}
\usage{
get_latest_data(path, name = NULL, ext = NULL)
}
\arguments{
  \item{path}{Character. Directory containing the files.}
  \item{name}{Character (optional). Filter files by dataset name (partial match, case-insensitive).}
  \item{ext}{Character (optional). Filter files by file extension (e.g., "csv", "txt").}
}
\value{
A character string representing the full path of the latest file.
}
\details{
The function expects file names in the format:
"dd-mm-yyyy - filename.ext".

If multiple files share the latest date, the function will stop with an error
unless further filtering is applied using \code{name} or \code{ext}.
}
\examples{
# Get latest file
get_latest_data("input")

# Filter by dataset name
get_latest_data("input", name = "iris")

# Filter by extension
get_latest_data("input", ext = "csv")
}
