\name{trimed_data}
\alias{trimed_data}
\title{Trim whitespace and restore column types}
\usage{
trimed_data(data)
}
\arguments{
\item{data}{A data.frame or tibble.}
}
\value{
A data frame with trimmed character values and restored column types.
}
\description{
Trims leading and trailing whitespace from all columns in a data frame.
Numeric columns are converted back to numeric and factor columns are
converted back to factors after trimming.
}
\examples{
df <- data.frame(
  name = c("  John ", " Mary "),
  age = c(" 23 ", " 30 "),
  gender = factor(c(" M ", " F "))
)

trimed_data(df)
}
