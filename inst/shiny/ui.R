library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(

  theme = shinythemes::shinytheme("sandstone"),

  # App title ----
  titlePanel("Clustering Variables in R"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Horizontal line ----
      tags$hr(style = "border: 1px solid black;"),

      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),

      # Horizontal line ----
      tags$hr(style = "border: 1px solid black;"),

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
          "Hierarchical (hclustvar)" = "hclustvar",
          "K-means (kmeansvar)" = "kmeansvar",
          "MCA-based" = "mca"
        ),
        selected = "hclustvar"
      )

  ),
  # Main panel for displaying outputs----
  mainPanel(
    h4("Preview of Uploaded Data"),
    tableOutput("contents")

    )
  )
)
