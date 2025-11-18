library(shiny)
library(bslib)

# =========================
# Define UI for ClusteringVariables App
# =========================
ui <- fluidPage(

  # ---- Theme & JS ----
  theme = shinythemes::shinytheme("united"),
  shinyjs::useShinyjs(),

  # ---- App Title ----
  titlePanel("ClusteringVariables"),

  # ---- Sidebar Layout ----
  sidebarLayout(

    # =========================
    # Sidebar Panel: Inputs
    # =========================
    sidebarPanel(
      width = 3,

      # ---- File Upload ----
      fileInput(
        "file1",
        "Choose CSV File",
        multiple = FALSE,
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),

      # ---- Header Option ----
      checkboxInput("header", "Header", TRUE),

      # ---- Separator Option ----
      radioButtons(
        "sep",
        "Separator",
        choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
        selected = ","
      ),

      # ---- Variable Selectors ----
      selectInput(
        "active_vars",
        "Choose Active Variables",
        choices = NULL,
        multiple = TRUE
      ),
      selectInput(
        "illustrative_vars",
        "Choose Illustrative Variables",
        choices = NULL,
        multiple = TRUE
      ),

      # ---- Clustering Algorithm ----
      radioButtons(
        inputId = "algorithm",
        label = "Choose a clustering model:",
        choices = c("KMeans" = "kmeans",
                    "VarClus" = "varclus",
                    "MCA&CAH" = "acm_cah"),
        selected = "kmeans"
      ),

      # ---- Number of Clusters ----
      tags$div(
        tags$strong("Choose number of clusters (k):"),
        style = "margin-bottom: 2px;"
      ),

      checkboxInput(
        inputId = "auto_k",
        label = "auto",
        value = TRUE
      ),

      sliderInput(
        inputId = "num_k",
        label = NULL,
        min = 2,
        max = 15,
        value = 3,
        step = 1
      ),

      # ---- Run Button ----
      actionButton("run_clustering", "Run Clustering")
    ),

    # =========================
    # Main Panel: Outputs
    # =========================
    mainPanel(
      uiOutput("main_content")
    )
  )
)
