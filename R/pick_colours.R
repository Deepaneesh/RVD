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

    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML("
          .colour-grid {
            display: flex;
            flex-wrap: wrap;
            gap: 10px;
          }

          .colour-item {
            width: calc(20% - 10px);
            min-width: 160px;
          }

          .colourpicker {
            width: 100%;
          }
        ")
      )
    ),

    shiny::div(
      class = "colour-grid",

      lapply(seq_len(n), function(i) {

        shiny::div(
          class = "colour-item",

          colourpicker::colourInput(
            inputId = paste0("col_", i),
            label = paste("Colour", i),
            value = "#FF0000",
            allowTransparent = TRUE
          )

        )

      })

    ),

    shiny::br(),

    shiny::actionButton(
      inputId = "done",
      label = "Done"
    )

  )

  server <- function(input, output, session) {

    shiny::observeEvent(input$done, {

      cols <- vapply(
        seq_len(n),
        function(i) input[[paste0("col_", i)]],
        character(1)
      )

      shiny::stopApp(cols)

    })

  }

  shiny::runApp(
    shiny::shinyApp(ui = ui, server = server)
  )

}
