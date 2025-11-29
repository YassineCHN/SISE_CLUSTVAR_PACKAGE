# =============================================================================
# inst/shiny/server.R
# Logique serveur de l'application Shiny - VERSION FINALE AVEC ILLUSTRATIVE
# =============================================================================

library(shiny)
library(DT)
library(shinyjs)

# Charger les classes R6
source("../../R/n_clusters.R")  # Fonctions elbow (DOIT être chargé AVANT les classes)
source("../../R/kmeans.R")      # Classe K-means
source("../../R/acm_cah.R")     # Classe ACM-CAH
source("../../R/varclus.R")     # Classe VarClus

# Charger les modules Shiny
source("modules/kmeans_module.R")
source("modules/acm_cah_module.R")
source("modules/varclus_module.R")

# =============================================================================
# FONCTION SERVEUR
# =============================================================================

server <- function(input, output, session) {

  # ===========================================================================
  # STOCKAGE DES DATASETS
  # ===========================================================================

  # Store all uploaded datasets
  datasets <- reactiveValues(data = list())

  # ===========================================================================
  # REACTIVE 1: Preview des données uploadées
  # ===========================================================================

  preview_data <- reactive({
    req(input$file1)

    tryCatch({
      file_ext <- tools::file_ext(input$file1$name)

      if (file_ext %in% c("xlsx", "xls")) {
        # Support Excel files
        if (!requireNamespace("readxl", quietly = TRUE)) {
          showNotification(
            "Package 'readxl' requis pour les fichiers Excel. Installation...",
            type = "warning",
            duration = 5
          )
          install.packages("readxl")
        }
        df <- readxl::read_excel(input$file1$datapath)

      } else {
        # CSV/TXT files
        df <- read.csv(
          input$file1$datapath,
          header = input$header,
          sep = input$sep,
          quote = input$quote,
          dec = ","  # Support comma as decimal separator
        )
      }

      # Convert character columns with comma decimals to numeric
      for (col in names(df)) {
        if (is.character(df[[col]])) {
          # Try to convert if it looks like numbers with commas
          test_numeric <- gsub(",", ".", df[[col]])
          if (all(grepl("^-?[0-9]+(\\.[0-9]+)?$", test_numeric, perl = TRUE) | is.na(test_numeric))) {
            df[[col]] <- as.numeric(gsub(",", ".", df[[col]]))
          }
        }
      }

      df

    }, error = function(e) {
      showNotification(
        paste("Erreur de lecture:", e$message),
        type = "error",
        duration = 10
      )
      NULL
    })
  })

  # ===========================================================================
  # OUTPUT: Afficher la preview des données
  # ===========================================================================

  output$data_preview <- renderUI({
    if (is.null(input$file1)) {
      div(
        class = "data-preview-box",
        style = "font-style: italic; color: #666; font-size: 16px; text-align: center; padding: 50px;",
        icon("upload", class = "fa-3x", style = "color: #ccc;"),
        br(), br(),
        "📂 Veuillez charger un fichier de données pour commencer."
      )
    } else {
      df <- preview_data()

      if (is.null(df)) {
        return(div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          " Erreur de lecture du fichier avec les paramètres actuels."
        ))
      }

      n_quanti <- sum(sapply(df, is.numeric))
      n_quali  <- sum(sapply(df, function(x) !is.numeric(x)))

      tagList(
        # Résumé
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: #e3f2fd; padding: 15px; border-radius: 6px; margin-bottom: 15px; border-left: 4px solid #2196f3;",
              strong(style = "color: #1976d2; font-size: 1.1em;", "📊 Résumé des données"),
              br(), br(),
              fluidRow(
                column(6,
                       div(style = "font-size: 1.05em;",
                           icon("table"),
                           strong(" Dimensions:"),
                           paste(nrow(df), "lignes ×", ncol(df), "colonnes")
                       )
                ),
                column(6,
                       div(style = "font-size: 1.05em;",
                           icon("file"),
                           strong(" Fichier:"),
                           input$file1$name
                       )
                )
              ),
              br(),
              fluidRow(
                column(6,
                       div(style = "font-size: 1.05em;",
                           icon("chart-bar", style = "color: #4caf50;"),
                           strong(" Variables quantitatives:"),
                           n_quanti
                       )
                ),
                column(6,
                       div(style = "font-size: 1.05em;",
                           icon("tags", style = "color: #ff9800;"),
                           strong(" Variables qualitatives:"),
                           n_quali
                       )
                )
              )
            )
          )
        ),

        # Table preview
        fluidRow(
          column(
            width = 12,
            h5(icon("eye"), " Aperçu des 10 premières lignes", style = "color: #555;"),
            div(
              style = "overflow-x: auto;",
              tableOutput("contents")
            )
          )
        )
      )
    }
  })

  output$contents <- renderTable({
    req(preview_data())
    head(preview_data(), 10)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ===========================================================================
  # OBSERVER: Sauvegarder le dataset
  # ===========================================================================

  save_msg <- reactiveVal(NULL)

  # Reset message when new file uploaded
  observeEvent(input$file1, {
    save_msg(NULL)
  })

  observeEvent(input$save_dataset, {
    req(preview_data())

    # Sauvegarder dans reactiveValues
    datasets$data[[input$file1$name]] <- preview_data()

    # Mettre à jour le sélecteur de datasets
    updateSelectInput(
      session,
      "dataset_choice",
      choices = names(datasets$data),
      selected = input$file1$name
    )

    # Message de confirmation
    save_msg(
      div(
        style = "color: #28a745; font-weight: bold; margin-top: 10px; padding: 10px; background-color: #d4edda; border-radius: 5px; border: 1px solid #c3e6cb;",
        icon("check-circle"),
        HTML(" Dataset enregistré avec succès !")
      )
    )
  })

  output$save_msg <- renderUI({
    save_msg()
  })

  # ===========================================================================
  # OBSERVER: Mettre à jour les sélecteurs de variables
  # ===========================================================================

  observeEvent(input$dataset_choice, {
    req(datasets$data)
    df <- datasets$data[[input$dataset_choice]]
    req(df)

    cols <- names(df)

    # Séparer variables numériques et qualitatives
    num_vars <- names(df)[sapply(df, is.numeric)]

    # Par défaut, sélectionner toutes les variables numériques comme actives
    updateSelectInput(
      session,
      "active_vars",
      choices = cols,
      selected = num_vars
    )

    updateSelectInput(
      session,
      "illustrative_vars",
      choices = cols
    )
  })

  # ===========================================================================
  # OBSERVER: Variables actives/illustratives mutuellement exclusives
  # ===========================================================================

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

  # ===========================================================================
  # OBSERVER: Désactiver slider k quand auto_k est coché
  # ===========================================================================

  observe({
    if (isTRUE(input$auto_k)) {
      disable("num_k")
    } else {
      enable("num_k")
    }
  })

  # ===========================================================================
  # REACTIVE: Dataset sélectionné
  # ===========================================================================

  selected_data <- reactive({
    req(input$dataset_choice)
    datasets$data[[input$dataset_choice]]
  })

  # ===========================================================================
  # REACTIVES: Variables actives et illustratives
  # ===========================================================================

  active_data <- eventReactive(input$run_clustering, {
    req(selected_data(), input$active_vars)

    if (length(input$active_vars) < 2) {
      showNotification(
        "Veuillez sélectionner au moins 2 variables actives",
        type = "error",
        duration = 5
      )
      return(NULL)
    }

    df <- selected_data()
    df[, input$active_vars, drop = FALSE]
  })

  # NOUVEAU: Reactive pour variables illustratives
  illustrative_data <- eventReactive(input$run_clustering, {
    req(selected_data())

    if (!is.null(input$illustrative_vars) && length(input$illustrative_vars) > 0) {
      df <- selected_data()
      df[, input$illustrative_vars, drop = FALSE]
    } else {
      NULL
    }
  })

  # ===========================================================================
  # REACTIVE: k optimal ou choisi
  # ===========================================================================

  k_value <- eventReactive(input$run_clustering, {
    req(active_data())
    req(input$algorithm)

    X <- active_data()

    if (input$auto_k) {
      # Détection automatique selon l'algorithme

      if (input$algorithm == "kmeans") {
        # K-means auto-detect
        km_temp <- KMeansVariablesQuant$new(k = 2, seed = 42)
        km_temp$fit(X)
        elbow_res <- km_temp$elbow(k_range = 2:min(10, ncol(X)), plot = FALSE)
        k_opt <- elbow_res$optimal_k

        showNotification(
          paste("✅ K-means: k optimal =", k_opt),
          type = "message",
          duration = 5
        )

        return(k_opt)

      } else if (input$algorithm == "acm_cah") {
        # ACM-CAH auto-detect
        quali_data <- X[, sapply(X, is.factor) | sapply(X, is.character), drop = FALSE]
        if (ncol(quali_data) > 0) {
          quali_data[] <- lapply(quali_data, factor)
        }

        elbow_res <- acm_cah_elbow(
          X_quali = quali_data,
          method = input$acm_cah_method,
          k_max = min(10, nrow(quali_data))
        )

        k_opt <- elbow_res$optimal_k

        showNotification(
          paste("✅ ACM-CAH: k optimal =", k_opt),
          type = "message",
          duration = 8
        )

        return(k_opt)

      } else if (input$algorithm == "varclus") {
        # VarClus auto-detect
        quant_data <- X[, sapply(X, is.numeric), drop = FALSE]
        quant_data <- as.matrix(quant_data)
        mode(quant_data) <- "numeric"

        elbow_res <- varclus_elbow(X_num = quant_data, similarity = "pearson")
        k_opt <- elbow_res$optimal_k

        showNotification(
          paste("✅ VarClus: k optimal =", k_opt),
          type = "message",
          duration = 5
        )

        return(k_opt)
      }
    } else {
      # k manuel
      return(input$num_k)
    }
  })

  # ===========================================================================
  # MODULE SERVEUR: K-means (NOUVEAU AVEC ILLUSTRATIVE)
  # ===========================================================================

  kmeansServer(
    "kmeans_tab",
    data = active_data,
    k = k_value,
    illustrative_vars = illustrative_data  # NOUVEAU!
  )

  # ===========================================================================
  # MODULE SERVEUR: ACM-CAH (NOUVEAU AVEC ILLUSTRATIVE)
  # ===========================================================================

  acm_cah_server(
    input, output, session,
    data = active_data,
    k = k_value,
    method = reactive(input$acm_cah_method),
    n_axes = reactive(input$acm_cah_n_axes),
    illustrative_vars = illustrative_data  # NOUVEAU!
  )

  # ===========================================================================
  # MODULE SERVEUR: VarClus (NOUVEAU AVEC ILLUSTRATIVE)
  # ===========================================================================

  varclus_server(
    input, output, session,
    data = active_data,
    k = k_value,
    illustrative_vars = illustrative_data  # NOUVEAU!
  )

  # ===========================================================================
  # OBSERVER: Changer d'onglet selon l'algo sélectionné
  # ===========================================================================

  observeEvent(input$algorithm, {
    updateTabsetPanel(session, "algo_tabs", selected = input$algorithm)
  })

  # ===========================================================================
  # OBSERVER: Passer à l'onglet Clustering après avoir lancé le clustering
  # ===========================================================================

  observeEvent(input$run_clustering, {
    updateTabsetPanel(session, "main_tabs", selected = "clustering")
  })

}
