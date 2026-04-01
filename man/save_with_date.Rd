\name{save_with_date}
\alias{save_with_date}
\title{Save File Name with Current Date}
\description{
This function appends the current system date to a file name
in the format "dd-mm-yyyy - filename" and returns the full file path.
If the specified directory does not exist, it will be created.
}
\usage{
save_with_date(path, filename)
}
\arguments{
  \item{path}{Character. Directory where the file will be saved.}
  \item{filename}{Character. Name of the file (with extension).}
}
\value{
A character string representing the full file path with date-prefixed filename.
}
\details{
The date is added in the format "dd-mm-yyyy".
If the directory does not exist, it will be created recursively.
}
\examples{
save_with_date("input", "iris.csv")
}
