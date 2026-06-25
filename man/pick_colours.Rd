\name{pick_colours}
\alias{pick_colours}
\title{Interactive Colour Picker}
\usage{
pick_colours(n = 1)
}

\arguments{
  \item{n}{
    Integer. Number of colours to select. Default is 1.
  }
}

\value{
A character vector containing the selected HEX colour codes.
}

\description{
Opens an interactive Shiny colour picker and allows users
to select one or more colours. The selected colours are
returned as HEX colour codes.
}

\details{
The function launches a Shiny application using the
\pkg{colourpicker} package. After selecting colours and
clicking the \strong{Done} button, the selected colours
are returned as a character vector.

The returned HEX codes can be used directly in plotting
functions, dashboards, themes, and other visualization
tools.
}

\examples{
\dontrun{

# Select one colour
col <- pick_colours()

print(col)

# Select three colours
cols <- pick_colours(3)

print(cols)

}
}

\author{
Deepaneesh RV
}

\seealso{
\code{\link[colourpicker]{colourInput}},
\code{\link[shiny]{runApp}}
}

\keyword{utilities}
\keyword{colors}
