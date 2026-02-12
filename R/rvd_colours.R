#' Generate random high-contrast colours (with forecast pairs)
#'
#' Generates a random set of N visually distinct colours suitable for plots.
#' When forecast = TRUE, returns paired colours (dark, light) for each series.
#' Colours are randomized on each call.
#'
#' @param n Number of colour pairs (or colours if forecast = FALSE).
#' @param palette Palette type. One of "contrast", "viridis", "hue".
#' @param forecast Logical. If TRUE, returns dark + light colour pairs.
#' @param seed Optional integer for reproducible randomness.
#'
#' @return If forecast = FALSE, a character vector of hex colours.
#' If forecast = TRUE, a data.frame with columns `dark` and `light`.
#'
#' @examples
#' rvd_colours(3)
#' rvd_colours(3, forecast = TRUE)
#' rvd_colours(3, seed = 123)
#'
#' @export
rvd_colours <- function(n, palette = c("contrast", "viridis", "hue"),
                        forecast = FALSE, seed = NULL) {

  palette <- match.arg(palette)

  if (!is.numeric(n) || n <= 0) {
    stop("`n` must be a positive number.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate more colours than needed, then randomly sample
  base_pool <- switch(
    palette,
    contrast = colorspace::qualitative_hcl(max(n * 2, 10)),
    viridis  = viridisLite::viridis(max(n * 2, 10)),
    hue      = scales::hue_pal()(max(n * 2, 10))
  )

  base_cols <- sample(base_pool, n)

  if (!forecast) {
    return(base_cols)
  }

  # Create dark & light variants
  dark_cols  <- colorspace::darken(base_cols, amount = 0.3)
  light_cols <- colorspace::lighten(base_cols, amount = 0.35)

  data.frame(
    dark = dark_cols,
    light = light_cols,
    stringsAsFactors = FALSE
  )
}
