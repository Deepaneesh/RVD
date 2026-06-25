#' Pick one or more colours interactively
#'
#' Opens a Shiny colour picker and returns the selected colours.
#'
#' @param n Number of colours to select.
#'
#' @return Character vector of HEX colour codes.
#'
#' @export
pick_colours <- function(n = 1) {

  ui <- shiny::fluidPage(

    lapply(
      seq_len(n),
      function(i) {
        colourpicker::colourInput(
          inputId = paste0("col_", i),
          label = paste("Colour", i),
          value = "#FF0000",
          allowTransparent = TRUE
        )
      }
    ),

    shiny::br(),

    shiny::actionButton("done", "Done")
  )

  server <- function(input, output, session) {

    shiny::observeEvent(input$done, {

      cols <- unname(
        sapply(
          seq_len(n),
          function(i) input[[paste0("col_", i)]]
        )
      )

      shiny::stopApp(cols)

    })

  }

  shiny::runApp(
    shiny::shinyApp(ui, server)
  )

}
