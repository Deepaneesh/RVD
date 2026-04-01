\name{available_files}
\alias{available_files}
\title{List Available Files with Metadata}
\description{
This function scans a directory and returns a data frame containing
file metadata including date, file name, extension, and full file name.
It supports file names with an optional date prefix in the format
"dd-mm-yyyy - filename.ext".
}
\usage{
available_files(path)
}
\arguments{
  \item{path}{Character. Directory containing the files.}
}
\value{
A data frame with the following columns:
\itemize{
  \item date: Date extracted from file name (NA if not present)
  \item file_name: Name of the file without extension
  \item extension: File extension
  \item full_name: Full file name
}
}
\details{
If a file does not contain a valid date prefix, the date column will be NA.
The function expects date format as "dd-mm-yyyy".
}
\examples{
available_files("input")
}
