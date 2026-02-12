\name{rvd_colours}
\alias{rvd_colours}
\title{Generate random high-contrast colours (with forecast pairs)}
\usage{
rvd_colours(n, palette = c("contrast", "viridis", "hue"), forecast = FALSE, seed = NULL)
}
\arguments{
\item{n}{Number of colour pairs (or colours if forecast = FALSE).}

\item{palette}{Palette type. One of "contrast", "viridis", "hue".}

\item{forecast}{Logical. If TRUE, returns dark + light colour pairs.}

\item{seed}{Optional integer for reproducible randomness.}
}
\value{
If forecast = FALSE, a character vector of hex colours.
If forecast = TRUE, a data.frame with columns \code{dark} and \code{light}.
}
\description{
Generates a random set of N visually distinct colours suitable for plots.
When forecast = TRUE, returns paired colours (dark, light) for each series.
Colours are randomized on each call.
}
\examples{
rvd_colours(3)
rvd_colours(3, forecast = TRUE)
rvd_colours(3, seed = 123)
}
