library(shiny)
library(shinyjs)
library(shinythemes)

ui <- navbarPage(
  "ClusteringVariables",

  theme = shinytheme("cosmo"),

  # ========================================================
  #                        HOME TAB
  # ========================================================
  tabPanel("Home",
           fluidRow(
             column(
               width = 12,
               div(
                 style = "padding: 40px 30px 40px 30px; text-align: center;",

                 h1("Welcome to Clustering Variables Application!",
                    style = "margin: 0 0 20px 0; font-weight: 700; font-size: 2.5em; color: #2d3748;"),

                 p("Discover relationships and patterns in your data by clustering variables using our ",
                   tags$code("ClusteringVariables", style = "background: #edf2f7; color: #c53030; padding: 4px 12px; border-radius: 4px; font-size: 0.9em; font-weight: 700;"),
                   " R Package",
                   style = "text-align: center; font-size: 1.2em; color: #4a5568; margin: 0; font-weight: 400;")
               )
             )
           ),

           # Main Content Section
           fluidRow(
             column(
               width = 10,
               offset = 1,

               # Introduction Card
               div(
                 style = "background: white;
                   padding: 35px;
                   border-radius: 10px;
                   box-shadow: 0 4px 15px rgba(0,0,0,0.08);
                   margin-bottom: 35px;
                   border-left: 5px solid #667eea;",

                 h3("Why Cluster Variables?",
                    style = "color: #2d3748; font-weight: 600; margin-top: 0; margin-bottom: 20px;"),

                 p("While traditional clustering groups data points (individuals), ",
                   strong("variable clustering"), " reveals the hidden structure within your features themselves.
            By analyzing correlations and grouping variables with similar behaviors, you can:",
                   style = "font-size: 17px; line-height: 1.8; color: #4a5568; margin-bottom: 20px;"),

                 div(
                   style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; margin-top: 25px;",

                   div(
                     style = "background: #f7fafc; padding: 20px; border-radius: 8px; border: 1px solid #e2e8f0;",
                     div(style = "font-size: 28px; margin-bottom: 10px;", "🔍"),
                     h5("Understand Structure", style = "color: #2d3748; font-weight: 600; margin-bottom: 8px;"),
                     p("Reveal which variables behave similarly", style = "color: #718096; font-size: 14px; margin: 0;")
                   ),

                   div(
                     style = "background: #f7fafc; padding: 20px; border-radius: 8px; border: 1px solid #e2e8f0;",
                     div(style = "font-size: 28px; margin-bottom: 10px;", "🔄"),
                     h5("Identify Redundancy", style = "color: #2d3748; font-weight: 600; margin-bottom: 8px;"),
                     p("Detect redundant features in your dataset", style = "color: #718096; font-size: 14px; margin: 0;")
                   ),

                   div(
                     style = "background: #f7fafc; padding: 20px; border-radius: 8px; border: 1px solid #e2e8f0;",
                     div(style = "font-size: 28px; margin-bottom: 10px;", "⚡"),
                     h5("Reduce Complexity", style = "color: #2d3748; font-weight: 600; margin-bottom: 8px;"),
                     p("Enable feature selection for simpler models", style = "color: #718096; font-size: 14px; margin: 0;")
                   )
                 )
               ),

               # Methods Section
               div(
                 style = "background: white;
                   padding: 35px;
                   border-radius: 10px;
                   box-shadow: 0 4px 15px rgba(0,0,0,0.08);
                   border-left: 5px solid #764ba2;",

                 h3("Three Powerful Methods",
                    style = "color: #2d3748; font-weight: 600; margin-top: 0; margin-bottom: 25px;"),

                 p("The ", tags$code("ClusteringVariables", style = "background: #edf2f7; padding: 3px 8px; border-radius: 4px; font-size: 15px;"),
                   " R package offers three distinct approaches built with R6 classes:",
                   style = "font-size: 16px; color: #4a5568; margin-bottom: 30px;"),

                 # Method Cards
                 div(
                   style = "display: flex; flex-direction: column; gap: 20px;",

                   # KMeans
                   div(
                     style = "background: linear-gradient(135deg, #667eea15 0%, #667eea05 100%);
                       padding: 25px;
                       border-radius: 8px;
                       border: 2px solid #667eea30;
                       transition: transform 0.2s;",
                     div(
                       style = "display: flex; align-items: start; gap: 15px;",
                       div(style = "font-size: 36px; line-height: 1;", "🎯"),
                       div(
                         h4("KMeans", style = "color: #667eea; font-weight: 700; margin: 0 0 10px 0; font-size: 1.4em;"),
                         p("Clusters quantitative variables using a reallocation algorithm that iteratively assigns variables to clusters based on their correlations.",
                           style = "color: #4a5568; font-size: 15px; line-height: 1.7; margin: 0;"),
                         tags$span("Best for: ", style = "color: #667eea; font-weight: 600; font-size: 14px;"),
                         tags$span("Large datasets with continuous variables", style = "color: #718096; font-size: 14px;")
                       )
                     )
                   ),

                   # VarClus
                   div(
                     style = "background: linear-gradient(135deg, #48bb7815 0%, #48bb7805 100%);
                       padding: 25px;
                       border-radius: 8px;
                       border: 2px solid #48bb7830;",
                     div(
                       style = "display: flex; align-items: start; gap: 15px;",
                       div(style = "font-size: 36px; line-height: 1;", "📊"),
                       div(
                         h4("VarClus", style = "color: #48bb78; font-weight: 700; margin: 0 0 10px 0; font-size: 1.4em;"),
                         p("Clusters quantitative variables using a divisive (top-down) hierarchical method that recursively splits variable groups for maximum separation.",
                           style = "color: #4a5568; font-size: 15px; line-height: 1.7; margin: 0;"),
                         tags$span("Best for: ", style = "color: #48bb78; font-weight: 600; font-size: 14px;"),
                         tags$span("Hierarchical structure exploration", style = "color: #718096; font-size: 14px;")
                       )
                     )
                   ),

                   # HAC
                   div(
                     style = "background: linear-gradient(135deg, #ed8936150%, #ed893605 100%);
                       padding: 25px;
                       border-radius: 8px;
                       border: 2px solid #ed893630;",
                     div(
                       style = "display: flex; align-items: start; gap: 15px;",
                       div(style = "font-size: 36px; line-height: 1;", "🧩"),
                       div(
                         h4("HAC (Hierarchical Agglomerative Clustering)",
                            style = "color: #ed8936; font-weight: 700; margin: 0 0 10px 0; font-size: 1.4em;"),
                         p("Designed for qualitative variables, it uses an agglomerative (bottom-up) hierarchical approach that progressively merges similar categorical variables.",
                           style = "color: #4a5568; font-size: 15px; line-height: 1.7; margin: 0;"),
                         tags$span("Best for: ", style = "color: #ed8936; font-weight: 600; font-size: 14px;"),
                         tags$span("Categorical and nominal variables", style = "color: #718096; font-size: 14px;")
                       )
                     )
                   )
                 )
               )
             )
           ),

           # Footer Section - Compact
           fluidRow(
             style = "margin-top: 20px;",
             column(
               width = 12,
               div(
                 style = "background: linear-gradient(135deg, #2d3748 0%, #1a202c 100%);
                   padding: 20px 40px;
                   border-radius: 10px;
                   box-shadow: 0 4px 15px rgba(0,0,0,0.1);
                   text-align: center;
                   margin-bottom: 0px;",

                 p("This project is conducted as part of the Data Science curriculum at University of Lyon 2, Master 2 SISE",
                   style = "color: rgba(255,255,255,0.9); font-size: 15px; line-height: 1.5; margin: 0 0 5px 0;"),

                 p(strong("Developed by:"),
                   style = "color: rgba(255,255,255,0.9); font-size: 14px; line-height: 1.5; margin: 0 0 5px 0;"),

                 p(strong("Maissa Lajimi"), " • ", strong("Yassine Cheniour"), " • ", strong("Lamia Hatem"),
                   style = "color: rgba(255,255,255,0.8); font-size: 16px; margin: 0 0 10px 0;"),

                 # GitHub Button
                 tags$a(
                   href = "https://github.com/maissaladjimi/SISE_Clustering_Variables_R",
                   target = "_blank",
                   style = "display: inline-flex;
                     align-items: center;
                     gap: 10px;
                     background: white;
                     color: #2d3748;
                     padding: 10px 28px;
                     border-radius: 8px;
                     text-decoration: none;
                     font-weight: 600;
                     font-size: 14px;
                     box-shadow: 0 4px 12px rgba(0,0,0,0.15);
                     transition: transform 0.2s, box-shadow 0.2s;",
                   onmouseover = "this.style.transform='translateY(-2px)'; this.style.boxShadow='0 6px 16px rgba(0,0,0,0.2)';",
                   onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 4px 12px rgba(0,0,0,0.15)';",

                   HTML('<svg height="18" width="18" viewBox="0 0 16 16" fill="currentColor">
                    <path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"/>
                  </svg>'),

                   span("View on GitHub")
                 )
               )
             )
           )
  ),

  # ========================================================
  #                        DATA IMPORT TAB
  # ========================================================
  tabPanel("Data Import",

           tags$head(
             tags$style(HTML("
                .sidebar-fixed {
                  position: fixed;
                  top: 51px;
                  bottom: 0;
                  left: 0;
                  width: 20%;
                  overflow-y: auto;
                  padding: 15px;
                  background: #f8f9fa;
                  z-index: 100;
                }
                .main-panel-adjusted {
                  margin-left: 20%;
                  width: 80%;
                }
              "))
           ),

           div(class = "row",

               # Sidebar
               div(class = "col-sm-3 sidebar-fixed",

                   # ===== Header =====
                   div(
                     style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                     padding: 15px;
                     margin: -15px -15px 20px -15px;",
                     h4(
                       style = "color: white; margin: 0; font-weight: 600; font-size: 1.2em;text-align: center;",
                       "Import Your Dataset"
                     )
                   ),

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

                   # ---- Save Button ----
                   actionButton("save_dataset", "Save Dataset"),
                   uiOutput("save_msg")
               ),

               # Main Panel
               div(class = "col-sm-9 main-panel-adjusted",
                   tags$hr(style = "border: 0; border-top: 1px solid #ccc; margin: 20px 0;"),
                   fluidRow(
                     column(
                       width = 12,
                       h4("Preview of uploaded data"),
                       uiOutput("data_preview")
                     )
                   )
               )
           )

  ),

  # ========================================================
  #                        Clustering TAB
  # ========================================================
  tabPanel("Clustering",

           tags$head(
             tags$style(HTML("
                /* Fix the navbar */
                .navbar {
                  position: fixed !important;
                  top: 0 !important;
                  left: 0 !important;
                  right: 0 !important;
                  z-index: 1000 !important;
                }

                /* Add padding to body so content isn't hidden under navbar */
                body {
                  padding-top: 51px !important;
                }

                .sidebar-fixed {
                  position: fixed;
                  top: 51px;
                  bottom: 0;
                  left: 0;
                  width: 20%;
                  overflow-y: auto;
                  padding: 15px;
                  background: #f8f9fa;
                  z-index: 100;
                }

                .main-panel-adjusted {
                  margin-left: 20%;  /* Should match sidebar width */
                  width: 80%;        /* Should be 100% - sidebar width */
                }

                /* Scrollbar styling for sidebar */
                .sidebar-fixed::-webkit-scrollbar {
                  width: 8px;
                }

                .sidebar-fixed::-webkit-scrollbar-track {
                  background: #f1f1f1;
                }

                .sidebar-fixed::-webkit-scrollbar-thumb {
                  background: #888;
                  border-radius: 4px;
                }

                .sidebar-fixed::-webkit-scrollbar-thumb:hover {
                  background: #555;
                }
              "))

           ),

           div(
             class = "row",
             # Sidebar
             div(
               class = "col-sm-3 sidebar-fixed",
               useShinyjs(),

               # ===== Header =====
               div(
                 style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                 padding: 15px;
                 margin: -15px -15px 10px -15px;",
                 h4(
                   style = "color: white; margin: 0; font-weight: 600; font-size: 1.2em;text-align: center;",
                   "Choose your model"
                 )
               ),

               # ===== Dataset Selection =====
               div(
                 style = "margin-bottom: 10px;",
                 tags$label(
                   style = "font-weight: 600; color: #2d3748; margin-bottom: 6px; display: block; font-size: 1em;",
                   "📂 Dataset"
                 ),
                 selectInput(
                   "dataset_choice",
                   label = NULL,
                   choices = NULL
                 )
               ),

               # ===== Variable Selectors =====
               div(
                 style = "margin-bottom: 10px;",

                 tags$label(
                   style = "font-weight: 600; color: #2d3748; margin-bottom: 8px; display: block; font-size: 1em;",
                   "🎯 Variables Selection"
                 ),

                 selectInput(
                   "active_vars",
                   label = tags$span("Active Variables", style = "font-size: 0.85em;"),
                   choices = NULL,
                   multiple = TRUE
                 ),

                 selectInput(
                   "illustrative_vars",
                   label = tags$span("Illustrative Variables", style = "font-size: 0.85em;"),
                   choices = NULL,
                   multiple = TRUE
                 )
               ),

               # ===== Clustering Algorithm =====

               div(
                 style = "margin-bottom: 15px;",
                 tags$label(
                   style = "font-weight: 600; color: #2d3748; margin-bottom: 8px; display: block; font-size: 1em;",
                   "📊 Clustering Method"
                 ),

                 tags$head(
                   tags$style(HTML("
                    #algorithm .radio label {
                      font-size: 0.85em;
                    }
                  "))
                 ),

                 radioButtons(
                   inputId = "algorithm",
                   label = NULL,
                   choices = c(
                     "KMeans (Quant)" = "kmeans",
                     "VarClus (Quant)" = "varclus",
                     "HAC (Qualit)" = "acm_cah"
                   ),
                   selected = "kmeans"
                 )
               ),

               # ===== Number of Clusters =====
               div(
                 style = "margin-bottom: 18px;",

                 tags$label(
                   style = "font-weight: 600; color: #2d3748; margin-bottom: 8px; display: block; font-size: 1em;",
                   "#️⃣Number of Clusters K"
                 ),

                 checkboxInput(
                   inputId = "auto_k",
                   label = tags$span("Auto-detect optimal k", style = "font-size: 0.85em;"),
                   value = TRUE
                 ),

                 sliderInput(
                   inputId = "num_k",
                   label = NULL,
                   min = 2,
                   max = 15,
                   value = 3,
                   step = 1
                 )
               ),

               # ===== Run Button =====
               actionButton(
                 "run_clustering",
                 "Run Clustering",
                 style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                 color: white;
                 border: none;
                 padding: 10px 20px;
                 font-weight: 600;
                 font-size: 14px;
                 border-radius: 6px;
                 width: 100%;
                 cursor: pointer;",
                 class = "btn-primary"
               )
             ),

             # Main Panel
             div(
               class = "col-sm-9 main-panel-adjusted",
               uiOutput("clustering_output")
             )
           )
  )
)
