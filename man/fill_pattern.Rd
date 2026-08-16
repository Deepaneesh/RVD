
\name{fill_pattern}
\alias{fill_pattern}
\title{Fill Missing Values Using a Repeating Pattern}
\usage{
fill_pattern(
  x,
  max_pattern_length = 10,
  min_repetitions = 2,
  print_pattern = FALSE
)
}
\arguments{
\item{x}{A vector containing observed values and possibly \code{NA} values.}

\item{max_pattern_length}{Maximum length of the repeating pattern to test.}

\item{min_repetitions}{Minimum number of repetitions required when searching
for a pattern.}

\item{print_pattern}{Logical; if \code{TRUE}, prints the detected pattern and
its match percentage.}
}
\value{
A vector of the same length as \code{x}, with \code{NA} values replaced
according to the detected repeating pattern. If no suitable pattern
can be detected, the original vector is returned unchanged.
}
\description{
Detects a repeating pattern in a vector and uses the detected pattern
to replace missing (\code{NA}) values.
}
\details{
The function tests possible repeating pattern lengths up to
\code{max_pattern_length}. For each pattern position, the first available
non-missing value is used to construct the candidate pattern. The candidate
pattern is then compared with the observed values, ignoring \code{NA} values.

The pattern with the highest match percentage is selected. If multiple
patterns have the same match percentage, the shortest pattern is preferred.

Only missing values are replaced. Existing non-missing values are never
modified.

If all values are missing, or no suitable pattern can be constructed,
the original vector is returned.
}
\examples{
x <- c("A", "B", "C", "D", "A", NA, "C", "D","A", "B", "C", "D")
fill_pattern(x, print_pattern = TRUE)

x <- c(10, 20, 30, 10, NA, 30, 10, 20)
fill_pattern(x)
}
