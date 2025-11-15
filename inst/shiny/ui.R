library(shiny)
library(bslib)

# Define UI for data upload app ----
ui <- fluidPage(

  theme = shinythemes::shinytheme("sandstone"),

  # App title ----
  titlePanel("ClusteringVariables"),

  # Sidebar layout ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 3,
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),

      # ---- Variable selectors (always visible) ----
      selectInput("active_vars", "Choose Active Variables",
                  choices = NULL,
                  multiple = TRUE),

      selectInput("illustrative_vars", "Choose Illustrative Variables",
                  choices = NULL,
                  multiple = TRUE),


      # Input: Choose clustering algorithm ----
      radioButtons(
        inputId = "algorithm",
        label = "Choose a clustering model:",
        choices = c(
          "K-means (Mixed)" = "kmeans",
          "VarClus (Quantitative)" = "varclus",
          "MCA&CAH (Qualitative" = "acm_cah"
        ),
        selected = "kmeans"
      ),

      actionButton("run_clustering", "Run Clustering")

  ),

  # Main panel for displaying outputs----
  mainPanel(
    uiOutput("main_content")
   )
  )
)
