\name{create_project_folders}
\alias{create_project_folders}
\title{Create Standard Project Folders}
\description{
This function creates a set of standard project directories if they do not already exist.
By default, it creates folders for input, output, logs, and archive.
}
\usage{
create_project_folders(dirs = c("input", "output", "logs", "archive"))
}
\arguments{
  \item{dirs}{Character vector. Names of directories to create.
  Default is c("input", "output", "logs", "archive").}
}
\value{
Invisibly returns NULL. Creates directories as a side effect.
}
\details{
The function checks whether each directory exists before creating it.
If a directory does not exist, it will be created recursively.
}
\examples{
# Create default folders
create_project_folders()

# Create custom folders
create_project_folders(c("data", "results", "temp"))
}
