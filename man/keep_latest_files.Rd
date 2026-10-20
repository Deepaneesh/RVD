\name{keep_latest_files}
\alias{keep_latest_files}
\title{Keep the Latest Files}
\usage{
keep_latest_files(path, n = 3)
}
\arguments{
\item{path}{A character string specifying the directory containing
the files.}

\item{n}{A positive integer specifying the number of latest files
to retain for each filename. Defaults to \code{3}.}
}
\value{
Invisibly returns a character vector containing the paths of the
files that were deleted. Returns \code{NULL} invisibly if no files
are found or no date-based files are available.
}
\description{
Keeps only the latest \code{n} files for each filename and extension,
and removes older versions.

Files must follow the naming convention:
\code{DD-MM-YYYY - filename.extension}.
}
\details{
The function identifies dates from the beginning of filenames and
groups files by their filename and extension. Within each group,
files are ordered by date in descending order. Only the latest
\code{n} files are retained, while older files are deleted.

Files that do not contain a valid date in the expected
\code{DD-MM-YYYY} format are ignored.
}
\examples{
\dontrun{
# Keep the latest 3 versions of each file
keep_latest_files("data")

# Keep the latest 5 versions of each file
keep_latest_files("data", n = 5)
}
}
