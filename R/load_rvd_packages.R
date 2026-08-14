load_rvd_packages <- function(update = FALSE) {

  packages <- c(
    "tidyverse",     # Data manipulation and visualization
    "readxl",        # Reading Excel files
    "lubridate",     # Working with dates
    "janitor",       # Cleaning data
    "magrittr",      # Pipes (%>% and %<>%)
    "forecast",      # Forecasting and prediction
    "tseries",       # Time series analysis
    "data.table",    # Data table format
    "dbplyr",        # Database operations in dplyr style
    "dtplyr",        # data.table in tidyverse style
    "plyr",          # plyr functions
    "imputeTS",      # Time series missing-value imputation
    "patchwork",     # Combining ggplots
    "lmtest",        # Regression models
    "car",           # Regression analysis
    "leaps",         # Stepwise regression
    "fastDummies",   # Dummy variable creation
    "ggThemeAssist", # ggplot theme assistance
    "plotly",        # Interactive plots
    "rsample",       # Data splitting
    "shiny",         # Shiny applications
    "colourpicker",  # Colour picker
    "connections",   # DBI connections
    "DBI"            # Database connections
  )

  # Check for missing packages
  missing_packages <- packages[
    !vapply(
      packages,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]

  # Install missing packages
  if (length(missing_packages) > 0) {

    message(
      "Installing missing packages: ",
      paste(missing_packages, collapse = ", ")
    )

    install.packages(missing_packages)
  }

  # Update packages if requested
  if (isTRUE(update)) {

    message("Checking for package updates...")

    update.packages(
      oldPkgs = packages,
      ask = FALSE
    )
  }

  # Load packages
  invisible(
    lapply(
      packages,
      function(pkg) {
        suppressPackageStartupMessages(
          library(pkg, character.only = TRUE)
        )
      }
    )
  )

  message("All RVD packages are loaded successfully.")
  search()
}
