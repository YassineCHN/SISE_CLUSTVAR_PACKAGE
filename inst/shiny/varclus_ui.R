varclus_ui <- function() {
  tagList(
    # ===== Title =====
    div(
      h2("Clustering numeric variables using VarClus Method"),
      style = "
        text-align: center;
        margin: 30px 0 40px 0;
        font-weight: 700;
        font-size: 28px;
        color: #2c3e50;"
    ),

    # ===== First row: Model Overview + Elbow Plot (40/60) =====
    div(
      style = "display: flex; gap: 20px; flex-wrap: nowrap; margin-bottom: 30px;",

      # Model Overview
      div(
        style = "
          flex: 0 0 40%;
          padding: 20px;
          border-radius: 10px;
          background: #ffffff;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
          overflow: visible;",
        h4("VarClus model overview", style = "margin-bottom: 15px; color:#34495e; font-weight: bold;"),
        tags$head(
          tags$style(HTML("
            #varclus_print {
              background-color: white !important;
              color: black !important;
              font-family: Arial, sans-serif !important;
              font-size: 14px !important;
              padding: 10px !important;
              border: none !important;
              height: auto !important;
              overflow-y: visible !important;
            }
          "))
        ),
        verbatimTextOutput("varclus_print")
      ),

      # Elbow Plot
      div(
        style = "
          flex: 0 0 60%;
          padding: 20px;
          border-radius: 10px;
          background: #ffffff;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
        h4("Elbow Method Plot - Optimal k Suggested", style = "margin-bottom: 15px; color:#34495e; font-weight: bold;"),
        plotOutput("varclus_elbow", height = "300px")
      )
    ),

    # ===== Second row: Heatmap + Dendrogram  =====
    div(
      style = "display: flex; gap: 20px; flex-wrap: nowrap; margin-bottom: 30px;",

      div(
        style = "
          flex: 0 0 50%;
          padding: 10px;
          border-radius: 10px;
          background: #ffffff;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
        h4("Heatmap - Variables Correlation", style = "margin-bottom: 15px; color:#34495e; font-weight: bold;"),
        plotlyOutput("varclus_heatmap", height = "400px")
      ),

      div(
        style = "
          flex: 0 0 50%;
          padding: 10px;
          border-radius: 10px;
          background: #ffffff;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
        h4("Dendrogram - Colored Clusters", style = "margin-bottom: 15px; color:#34495e; font-weight: bold;"),
        plotOutput("varclus_dendrogram", height = "400px")
      )
    ),

    # ===== Third row: Cluster Summary =====
    div(
      style = "
        padding: 20px;
        border-radius: 10px;
        background: #ffffff;
        box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        margin-bottom: 40px;",
      h4("VarClus Model - Clusters Summary", style = "margin-bottom: 15px; color:#34495e; font-weight: bold;"),
      verbatimTextOutput("varclus_summary_text"),
      tableOutput("varclus_cluster_summary"),
      tableOutput("varclus_R2_summary")
    )
  )
}



