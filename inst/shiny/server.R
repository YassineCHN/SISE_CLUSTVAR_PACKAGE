source("../../R/utils.R")
source("../../R/clusterengine.R")
source("../../R/n_clusters.R")
source("varclus_ui.R")
source("../../R/varclus.R")

server <- function(input, output, session) {

  run_clicked <- reactiveVal(FALSE)

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

  observeEvent(input$run_clustering, {
    run_clicked(TRUE)
  })

  output$main_content <- renderUI({

    # If button has never been clicked, show preview
    if(!run_clicked()){
      tagList(
        # ===== Logo + Title =====
        fluidRow(
          style = "margin-bottom: 25px;",
          column(
            width = 12,
            div(
              style = "display: flex; align-items: center; ; justify-content: center; gap: 15px;",
              img(src = "cluster.png", height = "80px"),
              h1("Welcome to Clustering Variables Application!",
                 style = "margin:0; font-weight:600; font-size:2.2em;")
            )
          )
        ),

        # ===== Description =====
        fluidRow(
          column(
            width = 12,
            p(
              "This application offers an interactive interface for the ",
              strong("ClusteringVariables"), " R package, enabling users to cluster variables and analyze their relationships in datasets. ",
              "The package was developed by ", em("Lamia Hatem"), ", ", em("Yasine Cheniour"),
              ", and ", em("Maissa Lajimi"), " as part of their coursework in the Master SISE program at the University of Lyon 2. It provides the following clustering methods:",
              style = "font-size:16px; line-height:1.7; color:#444; max-width:900px; margin:auto; text-align: justify;"
            )
          )
        ),

        # ===== Clustering Methods =====
        fluidRow(
          column(
            width = 12,
            div(
              style = "max-width:900px; margin:auto; margin-top:10px; font-size:16px; line-height:1.7; color:#444; text-align: justify;",
              tags$ul(
                style = "padding-left:20px;",
                tags$li(strong("🎯 KMeans (Mixed Variables) – "),
                        "A reallocation-based algorithm."),
                tags$li(strong("📊 VarClus (Quantitative Variables) – "),
                        "Specifically designed for continuous data, uses a divisive hierarchical approach."),
                tags$li(strong("🧩MCA/CAH (Qualitative Variables) – "),
                        "Two-step approach: MCA for dimension reduction, then CAH for clustering categorical variables.")
              )
            )
          )
        ),

        # ===== GitHub Button =====
        div(
          style = "max-width:900px; margin:auto; margin-top:0px; display:flex; justify-content:flex-end;",
          tags$a(
            href = "https://github.com/maissaladjimi/SISE_Clustering_Variables_R",
            target = "_blank",
            class = "btn",  # remove outline, use standard button
            style = "
                  display: inline-flex;
                  align-items: center;
                  gap: 8px;
                  font-weight: 500;
                  font-size: 16px;
                  border-radius: 8px;
                  padding: 10px 18px;
                  border: none;
                  background-color: #24292e;
                  color: #ffffff;
                  text-transform: none;
                  text-decoration: none;
                ",
            tags$img(
              src = "github.png",
              height = "22px",
              style = "vertical-align: middle;"
            ),
            "App GitHub Repository"
          )
        ),


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
      # Button clicked → show results based on selected algorithm
      algo <- input$algorithm

      # kmeans results --------------

      if(algo == "kmeans"){
        tagList(h3("K-Means Clustering Results")
                )
      # varclus results --------------
      } else if(algo == "varclus"){
          varclus_ui()

      # acm_cah results --------------
      } else if(algo == "acm_cah"){
        tagList(h3("MCA & CAH (Qualitative) Results"))
      }
    }
  })

  # ---- Data Preview ----
  output$data_preview <- renderUI({
    if (is.null(input$file1)) {
      # Show message when no file uploaded
      div(
        style = "font-style: italic; color: #666; font-size: 16px; margin-top:10px; padding: 10px; border: 1px dashed #ccc; border-radius: 6px; background-color:#f9f9f9;",
        " 📂 Please upload a dataset to get started."
      )
    } else {
      df <- data_uploaded()
      n_quanti <- sum(sapply(df, is.numeric))
      n_quali  <- sum(sapply(df, function(x) !is.numeric(x)))

      fluidRow(
        # Left column: summary
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
        # Right column: table preview
        column(
          width = 9,
          tableOutput("contents")
        )
      )
    }
  })

  # ---- Display only the first few rows ----
  output$contents <- renderTable({
    req(data_uploaded())
    head(data_uploaded())
  })

  # ---- Active/Illustrative variables choice----
  observeEvent(data_uploaded(), {
    run_clicked(FALSE)

    df <- data_uploaded()
    cols <- names(df)

    updateSelectInput(session, "active_vars", choices = cols)
    updateSelectInput(session, "illustrative_vars", choices = cols)
  })

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


  #---------- Clustering Results ------------------------------------

  clustering_engine <- reactive({
    req(input$active_vars)
    df <- data_uploaded()[, input$active_vars, drop = FALSE]

    # Data Preprocessing
    if(input$algorithm == "varclus"){
      df <- get_numeric_vars(df)

    } else if(input$algorithm == "kmeans"){
    } else if(input$algorithm == "acm_cah"){}

    engine <- ClusterEngine$new(data = df, method = input$algorithm)
    engine$fit()
    engine
  })

  # ------------ VarClus Outputs -----------------------------------

  # Elbow plot
  output$varclus_elbow <- renderPlot({
    req(clustering_engine())
    clustering_engine()$model$plot_elbow()
  })

  # Dendrogram
  output$varclus_dendrogram <- renderPlot({
    req(clustering_engine())
    dend_fun <- clustering_engine()$model$get_dendrogram()
    dend_fun()  # call function to plot
  })

  # heatmap
  output$varclus_heatmap <- renderPlot({
    req(clustering_engine())
    heat_fun <- clustering_engine()$model$get_heatmap()
    heat_fun()
  })

  # Print
  output$varclus_print <- renderPrint({
    req(clustering_engine())
    clustering_engine()$model$print()
  })

  # Summary
  output$varclus_summary_text <- renderText({
    req(clustering_engine())
    clustering_engine()$model$summary()$text
  })

  output$varclus_cluster_summary <- renderTable({
    req(clustering_engine())
    clustering_engine()$model$summary()$cluster_summary
  })

  output$varclus_R2_summary <- renderTable({
    req(clustering_engine())
    clustering_engine()$model$summary()$R2_summary
  })
}
