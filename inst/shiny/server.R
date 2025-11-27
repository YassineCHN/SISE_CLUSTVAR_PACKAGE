# ---------------------------
# Server.R for ClusteringVariables Shiny App
# ---------------------------

source("varclus_ui.R")
source("kmeans_ui.R")
source("acm_cah_ui.R")

server <- function(input, output, session) {

  # Store all uploaded datasets
  datasets <- reactiveValues(data = list())

  # ---------------------------
  # Reactive preview of the CSV
  # ---------------------------
  preview_data <- reactive({
    req(input$file1)
    tryCatch(
      read.csv(
        input$file1$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      ),
      error = function(e) NULL
    )
  })

  # ---------------------------
  # Show preview in Data Import tab
  # ---------------------------
  output$data_preview <- renderUI({
    if (is.null(input$file1)) {
      div(
        style = "font-style: italic; color: #666; font-size: 16px; margin-top:10px; padding: 10px; border: 1px dashed #ccc; border-radius: 6px; background-color:#f9f9f9;",
        "📂 Please upload a dataset to get started."
      )
    } else {
      df <- preview_data()
      if (is.null(df)) return(div("Error reading file with current settings"))
      n_quanti <- sum(sapply(df, is.numeric))
      n_quali  <- sum(sapply(df, function(x) !is.numeric(x)))

      fluidRow(
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
        column(
          width = 9,
          tableOutput("contents")
        )
      )
    }
  })

  output$contents <- renderTable({
    req(preview_data())
    head(preview_data())
  })

  # ---------------------------
  # Upload button: Save dataset based on user choice
  # ---------------------------

  save_msg <- reactiveVal(NULL)
  observeEvent(input$file1, {
    save_msg(NULL)
  })

  observeEvent(input$save_dataset, {
    req(preview_data())
    datasets$data[[input$file1$name]] <- preview_data()

    # Update dropdown in Clustering tab
    updateSelectInput(
      session,
      "dataset_choice",
      choices = names(datasets$data),
      selected = input$file1$name
    )

    save_msg(
      div(
        style = "color: green; font-weight: bold; margin-top: 10px;",
        HTML("✅ Dataset saved successfully!"))
    )

    output$save_msg <- renderUI({
      save_msg()
    })
  })

  # ---------------------------
  # Update variable selectors when dataset changes
  # ---------------------------
  observeEvent(input$dataset_choice, {
    req(datasets$data)
    df <- datasets$data[[input$dataset_choice]]
    req(df)
    cols <- names(df)

    updateSelectInput(session, "active_vars", choices = cols)
    updateSelectInput(session, "illustrative_vars", choices = cols)
  })

  # ---------------------------
  # Ensure active / illustrative variables are mutually exclusive
  # ---------------------------
  observeEvent(input$active_vars, {
    req(input$dataset_choice)
    df <- datasets$data[[input$dataset_choice]]
    updateSelectInput(
      session,
      "illustrative_vars",
      choices  = setdiff(names(df), input$active_vars),
      selected = intersect(input$illustrative_vars,
                           setdiff(names(df), input$active_vars))
    )
  })

  observeEvent(input$illustrative_vars, {
    req(input$dataset_choice)
    df <- datasets$data[[input$dataset_choice]]
    updateSelectInput(
      session,
      "active_vars",
      choices  = setdiff(names(df), input$illustrative_vars),
      selected = intersect(input$active_vars,
                           setdiff(names(df), input$illustrative_vars))
    )
  })

  # ---------------------------
  # Return selected dataset for clustering
  # ---------------------------
  selected_data <- reactive({
    req(input$dataset_choice)
    datasets$data[[input$dataset_choice]]
  })

  # Disable slider when auto_k checked
  observe({
    if (isTRUE(input$auto_k)) {
      disable("num_k")
    } else {
      enable("num_k")
    }
  })

  # ---------------------------
  # Clustering Output
  # ---------------------------

  clustering_result <- eventReactive(input$run_clustering, {
    list(
      algo = input$algorithm
    )
  })

  output$clustering_output <- renderUI({
    req(clustering_result())

    algo <- clustering_result()$algo

    if (algo == "kmeans") {
      tagList(
        h3("KMeans Results")
      )
    }

    else if (algo == "varclus") {
      tagList(
        varclus_ui()
      )
    }

    else if (algo == "acm_cah") {
      tagList(
        h3("HAC Results")
      )
    }
  })


  # ---------------------------
  # Clustering engine
  # ---------------------------
  clustering_engine <- eventReactive(input$run_clustering, {
    req(selected_data(), input$active_vars, input$algorithm)

    df <- selected_data()[ , input$active_vars, drop = FALSE]

    if (input$algorithm == "varclus") {
      df <- ClusteringVariables::get_numeric_vars(df)
    }

    n_clusters <- if (input$auto_k) NULL else input$num_k

    engine <- ClusteringVariables::ClusterEngine$new(
      data       = df,
      method     = input$algorithm,
      n_clusters = n_clusters
    )

    engine$fit()
    engine
  })

  # ---------------------------
  # illustrative variables
  # ---------------------------
  illust_results <- reactive({
    engine <- clustering_engine()
    req(engine)
    req(input$illustrative_vars)

    # Subset the selected illustrative variables
    illust_df <- selected_data()[, input$illustrative_vars, drop = FALSE]

    # Apply illustrative() on the already fitted VarClus model
    engine$model$illustrative(illust_df)
  })

  # ---------------------------
  # VarClus outputs
  # ---------------------------

  # Elbow method plot
  output$varclus_elbow <- renderPlot({
    engine <- clustering_engine()
    req(engine)
    engine$model$plot_elbow()
  })

  # Dendrogram plot
  output$varclus_dendrogram <- renderPlot({
    engine <- clustering_engine()
    req(engine)
    engine$model$get_dendrogram()()
  })

  # Heatmap plot
  output$varclus_heatmap <- plotly::renderPlotly({
    engine <- clustering_engine()
    req(engine)
    engine$model$get_heatmap()()
  })

  # Print method
  output$varclus_print <- renderPrint({
    engine <- clustering_engine()
    req(engine)
    engine$model$print()
  })

  # Text summary
  output$varclus_summary_text <- renderText({
    engine <- clustering_engine()
    req(engine)
    engine$model$summary()$text
  })

  # Similarity Matrix
  output$varclus_similarity_matrix <- renderTable({
    engine <- clustering_engine()
    req(engine)
    engine$model$summary()$similarity_matrix
  })

  # cluster summary
  output$varclus_cluster_summary <- renderTable({
    engine <- clustering_engine()
    req(engine)
    engine$model$summary()$cluster_summary
  })

  # Cluster R2 details
  output$varclus_R2_summary <- renderTable({
    engine <- clustering_engine()
    req(engine)
    engine$model$summary()$R2_summary
  })

  # Render illustrative table
  output$varclus_illu_table <- renderTable({
    illust_results()$table
  })

  # Render PCA correlation circle plot
  output$varclus_illu_plot <- renderPlot({
    illust_results()$plot()
  })
}
