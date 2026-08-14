\name{load_rvd_packages}
\alias{load_rvd_packages}

\title{
Install, Update, and Load RVD Packages
}

\description{
Checks whether the packages required by RVD are installed,
installs missing packages, optionally updates the listed packages,
and loads them into the current R session.
}

\usage{
load_rvd_packages(update = FALSE)
}

\arguments{
  \item{update}{
    A logical value indicating whether the listed packages should
    be updated. Defaults to \code{FALSE}.
  }
}

\details{
When \code{update = FALSE}, the function checks for missing packages,
installs them if necessary, and loads all RVD packages.

When \code{update = TRUE}, the function checks for missing packages,
installs missing packages, updates the listed packages, and then
loads all RVD packages.

This function is intended as a convenience function for setting up
the packages used by RVD.
}

\value{
Returns \code{NULL} invisibly. The function is primarily used for
installing, updating, and loading RVD packages.
}

\examples{
\dontrun{
# Install missing packages and load all RVD packages
load_rvd_packages()

# Install missing packages, update packages, and load them
load_rvd_packages(update = TRUE)
}
}

\seealso{
\code{\link[utils]{install.packages}},
\code{\link[utils]{update.packages}}
}

\keyword{utilities}
