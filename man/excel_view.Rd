\name{excel_view}
\alias{excel_view}

\title{
Editable preview of a data frame in Excel
}

\description{
This function saves a data frame to a temporary file and opens it in Excel for easy viewing and editing.
The file can be saved in CSV or XLSX format. By default, the format is CSV.
}

\usage{
excel_view(df, format = c("csv", "xlsx"))
}

\arguments{
  \item{df}{
    A data frame or tibble to be exported and opened in Excel.
  }
  \item{format}{
    The file format to export. Must be either \code{"csv"} or \code{"xlsx"}.
    The default is \code{"csv"}.
  }
}

\details{
The function creates a temporary folder in the current working directory (if it does not already exist),
writes the data frame to a file with a timestamped filename, and opens the file in Excel or the default
spreadsheet application available on the system.
}

\value{
This function does not return any value. It is called for its side effect of opening the file in Excel.
}

\author{
Deepaneesh R V
}

\note{
The file is opened using the default spreadsheet application available on your system.
}

\seealso{
\code{\link[readr]{write_csv}}, \code{\link[writexl]{write_xlsx}}
}

\examples{
\dontrun{
excel_view(iris)
excel_view(mtcars, format = "xlsx")
}
}
