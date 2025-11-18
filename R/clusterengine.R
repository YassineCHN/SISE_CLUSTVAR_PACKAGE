#' ClusterEngine for Managing Clustering Algorithms
#' ClusterEngine for Managing Clustering Algorithms
#'
#' This R6 class provides a common wrapper around multiple clustering algorithms.
#' It stores the input dataset, the chosen method, and the resulting fitted model.
#'
#' @field data A data frame of variables to cluster.
#' @field method The clustering algorithm to use: "varclus", "kmeans", or "acm_cah".
#' @field n_clusters Number of clusters (NULL for automatic selection if applicable).
#' @field model The fitted clustering model object.
#'
#' @export
ClusterEngine <- R6::R6Class(

  "ClusterEngine",

  public = list(

    # ------------------------------------------------------------
    # Fields
    # ------------------------------------------------------------
    data = NULL,
    method = NULL,
    n_clusters = NULL,
    model = NULL,

    # ------------------------------------------------------------
    # Constructor
    # ------------------------------------------------------------
    #' @description Initialize a new ClusterEngine
    #' @param data A data frame containing the variables to cluster
    #' @param method Character string specifying the clustering algorithm
    #' @param n_clusters Number of clusters (default NULL for auto)
    initialize = function(data, method, n_clusters = NULL) {
      self$data <- data
      self$method <- method
      self$n_clusters <- n_clusters
    },


    # ------------------------------------------------------------
    # Fit the selected clustering algorithm
    # ------------------------------------------------------------
    #' @description Fit the selected clustering algorithm
    #' @return The fitted model object
    fit = function() {

      # ---- VarClus ------------------------------------------------
      if (self$method == "varclus") {
        vc <- VarClus$new(n_clusters = self$n_clusters)
        vc$fit(self$data)
        self$model <- vc
      }

      # ---- K-means ------------------------------------------------
      if (self$method == "kmeans") {
        km <- kmeans$new(n_clusters = self$n_clusters)
        km$fit(self$data)
        self$model <- km
      }

      # ---- ACM + CAH ----------------------------------------------
      if (self$method == "acm_cah") {
        hc <- acm_cah$new(n_clusters = self$n_clusters)
        hc$fit(self$data)
        self$model <- hc
      }

      return(self$model)
    },


    # ------------------------------------------------------------
    # Predict Method
    # ------------------------------------------------------------
    #' @description Predict new variable's class using the fitted model
    #' @param new_data A data frame of new variable to predict
    #' @return Predicted cluster assignments
    predict = function(new_data) {
      if (is.null(self$model))
        stop("Model not fitted yet.")

      if ("predict" %in% names(self$model))
        return(self$model$predict(new_data))

      stop("This model does not support prediction.")
    },


    # ------------------------------------------------------------
    # Print method
    # ------------------------------------------------------------
    #' @description Print model brief summary
    #' @return Output of the model's print method
    print = function() {
      if (is.null(self$model))
        stop("Model not fitted yet.")

      if ("print" %in% names(self$model))
        return(self$model$print())

      return(self$model)
    },

    # ------------------------------------------------------------
    # Summary method
    # ------------------------------------------------------------
    #' @description display details of the fitted clustering model results
    #' @return Output of the model's summary results
    summary = function() {
      if (is.null(self$model))
        stop("Model not fitted yet.")

      if ("summary" %in% names(self$model))
        return(self$model$summary())

      return(self$model)
    }

  )
)
