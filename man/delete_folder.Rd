\name{delete_folder}
\alias{delete_folder}
\title{Delete One or More Folders}
\usage{
delete_folder(path = "temp", force = TRUE)
}
\arguments{
  \item{path}{A character vector of folder paths to delete.
  Defaults to \code{"temp"}.}

  \item{force}{Logical. Should read-only files be removed if possible?
  Defaults to \code{TRUE}.}
}
\value{
Invisibly returns a logical vector indicating whether each
folder was successfully deleted.
}
\description{
Deletes one or more folders and their contents recursively.
}
\examples{
\dontrun{
delete_folder()

delete_folder("my_folder")

delete_folder(c("folder1", "folder2"))
}
}
\author{
Deepaneesh
}
