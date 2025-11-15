varclus_ui <- function() {
  tagList(
    h3("VarClus Clustering Results"),

    # ===== First row: Print + Elbow =====
    fluidRow(
      column(
        width = 4,
        div(style = "padding:15px; border-radius:8px;
                    box-shadow:0 2px 5px rgba(0,0,0,0.1); height:300px; overflow-y:auto;",
            h4("Cluster Print"),
            verbatimTextOutput("varclus_print")
        )
      ),
      column(
        width = 8,
        div(style = "padding:15px; border-radius:8px;
                    box-shadow:0 2px 5px rgba(0,0,0,0.1);",
            h4("Elbow Method"),
            plotOutput("varclus_elbow")
        )
      )
    ),

    tags$br(),

    # ===== Second row: Heatmap + Dendrogram =====
    fluidRow(
      column(
        width = 7,
        div(style = "padding:15px; border-radius:8px;
                    box-shadow:0 2px 5px rgba(0,0,0,0.1);",
            h4("Heatmap"),
            plotOutput("varclus_heatmap")
        )
      ),
      column(
        width = 5,
        div(style = "padding:15px; border-radius:8px;
                    box-shadow:0 2px 5px rgba(0,0,0,0.1);",
            h4("Dendrogram"),
            plotOutput("varclus_dendrogram")
        )
      )
    ),

    tags$br(),

    # ===== Third row: Cluster Summary =====
    fluidRow(
      column(
        width = 12,
        div(style = "padding:15px; border-radius:8px;
                    box-shadow:0 2px 5px rgba(0,0,0,0.1);",
            h4("Cluster Summary"),
            verbatimTextOutput("varclus_summary_text"),
            tableOutput("varclus_cluster_summary"),
            tableOutput("varclus_R2_summary")
        )
      )
    )
  )
}


