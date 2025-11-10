server <- function(input, output, session) {

  data_uploaded <- reactive({
    req(input$file1)

    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep)
        df
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
  })

  # ---- Display only the first few rows ----
  output$contents <- renderTable({
    head(data_uploaded())
  })

  # ---- Update variable selector choices when data is loaded ----
  observeEvent(data_uploaded(), {
    df <- data_uploaded()
    cols <- names(df)

    updateSelectInput(session, "active_vars", choices = cols)
    updateSelectInput(session, "illustrative_vars", choices = cols)
  })

  # ---- Keep variable lists mutually exclusive ----
  observeEvent(input$active_vars, {
    if (!is.null(input$file1)) {
      updateSelectInput(session, "illustrative_vars",
                        choices = setdiff(names(data_uploaded()), input$active_vars),
                        selected = intersect(input$illustrative_vars,
                                             setdiff(names(data_uploaded()), input$active_vars)))
    }
  })

  observeEvent(input$illustrative_vars, {
    if (!is.null(input$file1)) {
      updateSelectInput(session, "active_vars",
                        choices = setdiff(names(data_uploaded()), input$illustrative_vars),
                        selected = intersect(input$active_vars,
                                             setdiff(names(data_uploaded()), input$illustrative_vars)))
    }
  })
}

