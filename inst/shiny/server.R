# ---------------------------
# Server.R for ClusteringVariables Shiny App
# ---------------------------

source("varclus_ui.R")
source("kmeans_ui.R")
source("acm_cah_ui.R")

# ---------------------------
# Server function
# ---------------------------
server <- function(input, output, session) {

  # ---------------------------
  # Reactive values to track run state
  # ---------------------------
  run_clicked        <- reactiveVal(FALSE)
  frozen_algo        <- reactiveVal(NULL)
  frozen_active_vars <- reactiveVal(NULL)
  frozen_data        <- reactiveVal(NULL)
  frozen_n_clusters  <- reactiveVal(NULL)

  # ---------------------------
  # Data upload
  # ---------------------------
  data_uploaded <- reactive({
    req(input$file1)

    tryCatch(
      {
        read.csv(input$file1$datapath,
                 header = input$header,
                 sep    = input$sep)
      },
      error = function(e) stop(safeError(e))
    )
  })

  # ---------------------------
  # Number of clusters input control
  # ---------------------------
  observe({
    if (input$auto_k) {
      shinyjs::disable("num_k")
    } else {
      shinyjs::enable("num_k")
    }
  })

  # ---------------------------
  # Run Clustering button
  # ---------------------------
  observeEvent(input$run_clustering, {
    req(data_uploaded())

    frozen_algo(input$algorithm)
    frozen_active_vars(input$active_vars)
    frozen_data(data_uploaded())
    frozen_n_clusters(if (input$auto_k) NULL else input$num_k)
    run_clicked(TRUE)
  })

  # ---------------------------
  # Main content UI
  # ---------------------------
  output$main_content <- renderUI({
    if(!run_clicked()) {

      # ===== Welcome / Instructions =====
      tagList(
        fluidRow(
          style = "margin-bottom: 25px;",
          column(
            width = 12,
            div(
              style = "display: flex; align-items: center; justify-content: center; gap: 15px;",
              img(src = "cluster.png", height = "80px"),
              h1("Welcome to Clustering Variables Application!",
                 style = "margin:0; font-weight:600; font-size:2.2em;")
            )
          )
        ),

        # Description
        fluidRow(
          column(
            width = 12,
            p(
              "This application offers an interactive interface for the ",
              strong("ClusteringVariables"), " R package, enabling users to cluster variables and analyze their relationships in datasets. ",
              "Developed by Lamia Hatem, Yasine Cheniour, and Maissa Lajimi.",
              style = "font-size:16px; line-height:1.7; color:#444; max-width:900px; margin:auto; text-align: justify;"
            )
          )
        ),

        # Clustering methods list
        fluidRow(
          column(
            width = 12,
            div(
              style = "max-width:900px; margin:auto; margin-top:10px; font-size:16px; line-height:1.7; color:#444; text-align: justify;",
              tags$ul(
                style = "padding-left:20px;",
                tags$li(strong("🎯 KMeans – "), "Quantitative variables, reallocation-based."),
                tags$li(strong("📊 VarClus – "), "Continuous variables, divisive hierarchical approach."),
                tags$li(strong("🧩 MCA/CAH – "), "Categorical variables, MCA + CAH approach.")
              )
            )
          )
        ),

        # GitHub repository link
        div(
          style = "max-width:900px; margin:auto; margin-top:0px; display:flex; justify-content:flex-end;",
          tags$a(
            href = "https://github.com/maissaladjimi/SISE_Clustering_Variables_R",
            target = "_blank",
            class  = "btn",
            style  = "display: inline-flex; align-items: center; gap: 8px; font-weight: 500; font-size: 16px; border-radius: 8px; padding: 10px 18px; border: none; background-color: #24292e; color: #ffffff; text-decoration: none;",
            tags$img(src = "github.png", height = "22px", style = "vertical-align: middle;"),
            "App GitHub Repository"
          )
        ),

        # Data preview placeholder
        tags$hr(style = "border: 0; border-top: 1px solid #ccc; margin: 20px 0;"),
        fluidRow(
          column(
            width = 12,
            h4("Preview of uploaded data"),
            uiOutput("data_preview")
          )
        )
      )

    } else {

      # ===== Results UI based on selected algorithm =====
      algo <- frozen_algo()

      if(algo == "kmeans") {
        kmeans_ui()
      } else if(algo == "varclus") {
        varclus_ui()
      } else if(algo == "acm_cah") {
        acm_cah_ui()
      }
    }
  })

  # ---------------------------
  # Data Preview
  # ---------------------------
  output$data_preview <- renderUI({
    if (is.null(input$file1)) {
      div(
        style = "font-style: italic; color: #666; font-size: 16px; margin-top:10px; padding: 10px; border: 1px dashed #ccc; border-radius: 6px; background-color:#f9f9f9;",
        "📂 Please upload a dataset to get started."
      )
    } else {
      df <- data_uploaded()
      n_quanti <- sum(sapply(df, is.numeric))
      n_quali  <- sum(sapply(df, function(x) !is.numeric(x)))

      fluidRow(
        # Summary panel
        column(
          width = 3,
          div(
            style = "background-color: #f8f9fa; padding: 10px; border-radius: 6px; line-height:1.5;",
            strong("Summary:"),
            br(),
            paste("Quantitative variables:", n_quanti),
            br(),
            paste("Qualitative variables:", n_quali)
          )
        ),
        # Table preview
        column(
          width = 9,
          tableOutput("contents")
        )
      )
    }
  })

  output$contents <- renderTable({
    req(data_uploaded())
    head(data_uploaded())
  })

  # ---------------------------
  # Active / Illustrative variables selection
  # ---------------------------
  observeEvent(data_uploaded(), {
    run_clicked(FALSE)
    cols <- names(data_uploaded())
    updateSelectInput(session, "active_vars", choices = cols)
    updateSelectInput(session, "illustrative_vars", choices = cols)
  })

  observeEvent(input$active_vars, {
    if (!is.null(input$file1)) {
      updateSelectInput(
        session,
        "illustrative_vars",
        choices  = setdiff(names(data_uploaded()), input$active_vars),
        selected = intersect(input$illustrative_vars,
                             setdiff(names(data_uploaded()), input$active_vars))
      )
    }
  })

  observeEvent(input$illustrative_vars, {
    if (!is.null(input$file1)) {
      updateSelectInput(
        session,
        "active_vars",
        choices  = setdiff(names(data_uploaded()), input$illustrative_vars),
        selected = intersect(input$active_vars,
                             setdiff(names(data_uploaded()), input$illustrative_vars))
      )
    }
  })

  # ---------------------------
  # Clustering engine
  # ---------------------------
  clustering_engine <- eventReactive(input$run_clustering, {
    req(frozen_data(), frozen_active_vars(), frozen_algo())

    df <- frozen_data()[, frozen_active_vars(), drop = FALSE]

    if (frozen_algo() == "varclus") {
      df <- ClusteringVariables::get_numeric_vars(df)
    }

    engine <- ClusteringVariables::ClusterEngine$new(
      data       = df,
      method     = frozen_algo(),
      n_clusters = frozen_n_clusters()
    )
    engine$fit()
    engine
  })

  # ---------------------------
  # VarClus outputs
  # ---------------------------

  # Elbow method plot
  output$varclus_elbow <- renderPlot({
    req(clustering_engine())
    clustering_engine()$model$plot_elbow()
  })

  # Dendrogram plot
  output$varclus_dendrogram <- renderPlot({
    req(clustering_engine())
    clustering_engine()$model$get_dendrogram()()
  })

  # Heatmap plot
  output$varclus_heatmap <- plotly::renderPlotly({
    req(clustering_engine())
    clustering_engine()$model$get_heatmap()()
  })

  # Print method
  output$varclus_print <- renderPrint({
    req(clustering_engine())
    clustering_engine()$model$print()
  })

  # Text summary
  output$varclus_summary_text <- renderText({
    req(clustering_engine())
    clustering_engine()$model$summary()$text
  })

  # cluster summary
  output$varclus_cluster_summary <- renderTable({
    req(clustering_engine())
    clustering_engine()$model$summary()$cluster_summary
  })

  # Cluster R2 details
  output$varclus_R2_summary <- renderTable({
    req(clustering_engine())
    clustering_engine()$model$summary()$R2_summary
  })

}
