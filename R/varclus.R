library(R6)
library(Hmisc)
library(dendextend)
library(plotly)

VarClus <- R6::R6Class(
  "VarClus",

  public = list(
    similarity = NULL,
    n_clusters = NULL,
    model = NULL,
    clusters = NULL,
    data = NULL,
    dendo = NULL,
    plot_elbow = NULL,

    #-------------
    # Constructor
    #-------------
    initialize = function(similarity = "pearson", n_clusters = NULL) {
      self$similarity <- similarity
      self$n_clusters <- n_clusters
    },

    #-------------
    # Fit Method
    #-------------
    fit = function(X_num) {
      self$data <- X_num

      # Expect X to already be numeric
      if (!is.numeric(X_num) && !is.matrix(X_num) && !all(sapply(X_num, is.numeric))) {
        stop("Input X must be fully numeric")
      }

      if (ncol(X_num) < 2) stop("At least two numeric variables are required.")

      self$model <- Hmisc::varclus(x = X_num, similarity = self$similarity)

      # Compute elbow plot always
      if (!exists("varclus_elbow")) stop("Function varclus_elbow() not found.")
      res <- varclus_elbow(X_num)
      self$plot_elbow <- res$plot

      # Set n_clusters automatically only if not specified
      if (is.null(self$n_clusters)) {
        self$n_clusters <- res$optimal_k
      }

      # Cut dendrogram into a fixed number of clusters
      clust <- cutree(self$model$hclust, k = self$n_clusters)
      self$clusters <- data.frame(variable = names(clust), cluster = clust, stringsAsFactors = FALSE)

      invisible(self)
    },

    #-------------
    # Plots Methods
    #-------------

    # Get Dendrogram
    get_dendrogram = function() {
      if (is.null(self$model) || is.null(self$clusters)) stop("Model not yet fitted or clusters not computed.")

      hc <- self$model$hclust
      dend <- as.dendrogram(hc)
      k <- length(unique(self$clusters$cluster))
      cols <- rainbow(k)

      dend <- color_branches(dend, k = k, col = cols)

      # Return a function that plots it when called
      function() {
        plot(dend, horiz = FALSE, main = NULL, xlab = NULL)
      }
    },

    # get heatmap
    get_heatmap = function() {
      if (is.null(self$model)) stop("Model not yet fitted.")

      cor_mat <- self$model$sim  # correlation matrix from varclus

      function() {
        plot_ly(
          x = colnames(cor_mat),
          y = rownames(cor_mat),
          z = cor_mat,
          type = "heatmap",
          colorscale = "Oranges",
          zmin = min(cor_mat),
          zmax = max(cor_mat)
        ) %>%
          layout(
            xaxis = list(title = ""),
            yaxis = list(title = "", autorange = "reversed")
          )
      }
    },

    #-------------
    # Print Method
    #-------------
    print = function(...) {
      cat("VarClus model\n")
      cat("Similarity:", self$similarity, "\n")
      if (!is.null(self$model)) {
        cat("Number of variables clustered:", length(self$model$hclust$order), "\n")
        cat("Number of clusters:", self$n_clusters, "\n")
      } else {
        cat("Model not yet fitted.\n")
      }
    },

    #-------------
    # Summary Method
    #-------------
    summary = function() {
      if (is.null(self$model)) stop("Model not yet fitted.")
      cluster_results <- private$compute_cluster_pcs()
      cluster_details <- private$compute_cluster_R2()

      text_summary <- paste("VarClus Summary\n",
                            "Similarity measure:", self$similarity, "\n",
                            "Number of clusters:", self$n_clusters)

      list(
        text = text_summary,
        cluster_summary = cluster_results,
        R2_summary = cluster_details
      )
    }
  ),

  private = list(

    #-------------
    # Compute PC for each cluster
    #-------------
    compute_cluster_pcs_list = function() {
      clusters <- self$clusters
      cluster_pcs <- list()

      for (k in unique(clusters$cluster)) {
        vars <- clusters$variable[clusters$cluster == k]
        if (length(vars) == 1) {
          cluster_pcs[[as.character(k)]] <- scale(self$data[, vars])
        } else {
          mat <- scale(self$data[, vars, drop = FALSE])
          pca <- prcomp(mat, center = TRUE, scale. = TRUE)
          cluster_pcs[[as.character(k)]] <- pca$x[, 1]
        }
      }

      return(cluster_pcs)
    },

    #-------------
    # Compute cluster PCA summary
    #-------------
    compute_cluster_pcs = function() {
      cluster_pcs <- private$compute_cluster_pcs_list()
      cluster_results <- data.frame(
        cluster = integer(),
        n_variables = integer(),
        eigenvalue = numeric(),
        variance_explained = numeric()
      )

      for (k in names(cluster_pcs)) {
        vars_in_cluster <- self$clusters$variable[self$clusters$cluster == as.numeric(k)]
        n_vars <- length(vars_in_cluster)

        if (n_vars == 1) {
          eigenvalue <- 1.0000
          prop_explained <- 1.0000
        } else {
          # Compute eigenvalue using PCA
          mat <- scale(self$data[, vars_in_cluster, drop = FALSE])
          pca <- prcomp(mat, center = TRUE, scale. = TRUE)
          eigenvalue <- round(pca$sdev[1]^2, 3)
          prop_explained <- round(pca$sdev[1]^2 / sum(pca$sdev^2), 4)
        }

        cluster_results <- rbind(cluster_results,
                                 data.frame(cluster = as.integer(k),
                                            `nbr_Members` = n_vars,
                                            Variation_explained = formatC(eigenvalue, format="f", digits=3),
                                            Proportion_explained = formatC(prop_explained, format="f", digits=3)))
      }

      cluster_results <- rbind(cluster_results)

      return(cluster_results)
    },

    #-------------
    # Compute R² summary
    #-------------
    compute_cluster_R2 = function() {
      clusters <- self$clusters
      clusters$variable <- as.character(clusters$variable)

      # Use shared PC1 list
      cluster_pcs <- private$compute_cluster_pcs_list()

      cluster_details <- data.frame(
        Cluster = integer(),
        Member = character(),
        Own_Cluster = numeric(),
        Next_Cluster = numeric(),
        `1-R2_Ratio` = numeric(),
        stringsAsFactors = FALSE
      )

      for (i in 1:nrow(clusters)) {
        var <- clusters$variable[i]
        clust <- clusters$cluster[i]

        # R² with own cluster
        R2_own <- cor(self$data[, var], cluster_pcs[[as.character(clust)]])^2

        # R² with other clusters
        other_clusters <- setdiff(names(cluster_pcs), as.character(clust))
        R2_next <- if(length(other_clusters) > 0) {
          max(sapply(other_clusters, function(k) cor(self$data[, var], cluster_pcs[[k]])^2))
        } else {
          0
        }

        R2_all <- sapply(cluster_pcs, function(pc) cor(self$data[, var], pc)^2)
        R2_ratio <- if(length(R2_all) == 1) 0 else (1 - R2_own) / (1 - R2_next)

        cluster_details <- rbind(cluster_details,
                                 data.frame(
                                   Cluster = clust,
                                   Member = var,
                                   Own_Cluster = formatC(R2_own, format="f", digits=3),
                                   Next_Cluster = formatC(R2_next, format="f", digits=3),
                                   `1_R2_Ratio` = formatC(R2_ratio, format="f", digits=3)
                                 ))
      }

      return(cluster_details)
    }
  )

)
