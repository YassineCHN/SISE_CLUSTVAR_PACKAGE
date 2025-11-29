# =============================================================================
# KMeansVariablesQuant: K-means de variables quantitatives autour de composantes latentes
#
# VERSION AMÉLIORÉE - AJOUTS:
# 1. ✅ Méthode illustrative() pour variables quantitatives illustratives
# 2. ✅ summary() amélioré avec affichage complet
# 3. ✅ print() amélioré avec plus d'informations
#
# PRINCIPE (d'après Ricco Rakotomalala / Vigneau-Qannari):
#   - Centre d'un cluster = PC1 des variables du cluster (1ère composante principale)
#   - Allocation d'une variable j -> cluster k maximisant R²(X_j, PC1_k)
#   - Inertie d'un cluster = λ_k = Σ R²(X_j, PC1_k) pour j ∈ cluster_k
#   - Inertie totale = somme des λ_k (solution retenue = max inertie)
#   - Variance expliquée = λ_k / nombre_variables_cluster × 100
#
# =============================================================================

library(R6)

KMeansVariablesQuant <- R6::R6Class(
  "KMeansVariablesQuant",

  public = list(

    # ===========================
    # Champs publics
    # ===========================
    data = NULL,             # data.frame brut fourni à fit
    X_scaled = NULL,         # matrice standardisée (n x p)
    k = NULL,                # nombre de clusters
    max_iter = NULL,         # nombre max d'itérations par run
    n_init = NULL,           # nombre d'initialisations aléatoires
    tol = NULL,              # tolérance pour convergence
    seed = NULL,             # graine aléatoire (reproductibilité)

    # Résultats du clustering
    clusters = NULL,         # vecteur d'assignation finale (length p)
    centers = NULL,          # matrice (n x k) des PC1 de chaque cluster
    r2_matrix = NULL,        # matrice (p x k) des R² de chaque variable avec chaque PC1
    inertia_total = NULL,    # inertie totale = somme des λ_k
    inertia_by_cluster = NULL, # vecteur des λ_k (somme des R²)
    n_iter = NULL,           # nombre d'itérations du meilleur run

    # Paramètres de standardisation (pour predict)
    scale_center = NULL,
    scale_scale = NULL,

    # ===========================
    # Constructeur
    # ===========================
    initialize = function(k = 3, max_iter = 100, n_init = 10, tol = 1e-4, seed = NULL) {
      if (!is.numeric(k) || k < 2) {
        stop("'k' doit être un entier >= 2")
      }
      self$k <- as.integer(k)
      self$max_iter <- as.integer(max_iter)
      self$n_init <- as.integer(n_init)
      self$tol <- tol
      self$seed <- if (!is.null(seed)) as.integer(seed) else NULL
    },

    # ===========================
    # Validation des données
    # ===========================
    check_data = function(X, min_vars = 2) {
      if (is.matrix(X)) X <- as.data.frame(X)
      if (!is.data.frame(X)) {
        stop("X doit être un data.frame ou une matrice")
      }
      if (ncol(X) < min_vars) {
        stop(sprintf("Au moins %d variable(s) quantitative(s) requise(s)", min_vars))
      }
      if (anyNA(X)) {
        stop("Les données ne doivent pas contenir de NA")
      }
      if (!all(sapply(X, is.numeric))) {
        stop("Toutes les colonnes doivent être numériques")
      }

      # Assurer des noms de colonnes
      if (is.null(colnames(X))) {
        colnames(X) <- paste0("V", seq_len(ncol(X)))
      }

      X
    },

    # ===========================
    # FIT - Entraînement du modèle
    # ===========================
    fit = function(X, k = NULL) {
      # Validation des données
      X <- self$check_data(X)
      self$data <- X

      # Mise à jour de k si fourni
      if (!is.null(k)) {
        if (!is.numeric(k) || k < 2 || k > ncol(X)) {
          stop("k invalide: doit être entre 2 et le nombre de variables")
        }
        self$k <- as.integer(k)
      }

      # Vérifier que k <= p
      if (self$k > ncol(X)) {
        stop("k ne peut pas être supérieur au nombre de variables")
      }

      # Initialiser la graine aléatoire si fournie
      if (!is.null(self$seed)) set.seed(self$seed)

      # Standardisation des données (centrage + réduction)
      Xs <- scale(X)
      self$X_scaled <- as.matrix(Xs)
      self$scale_center <- attr(Xs, "scaled:center")
      self$scale_scale <- attr(Xs, "scaled:scale")

      n <- nrow(self$X_scaled)
      p <- ncol(self$X_scaled)
      K <- self$k

      # Initialisation multiple : tester n_init configurations
      best_inertia <- -Inf
      best <- NULL

      message(sprintf("K-means: %d initialisations avec k=%d clusters...", self$n_init, K))

      for (run in seq_len(self$n_init)) {
        # Graine différente pour chaque run
        if (!is.null(self$seed)) {
          set.seed(self$seed + run * 1000)
        }

        # Lancer un run complet
        res_run <- private$single_run(self$X_scaled, K, self$max_iter, self$tol)

        # Garder le meilleur (inertie maximale)
        if (res_run$inertia_total > best_inertia) {
          best_inertia <- res_run$inertia_total
          best <- res_run
        }
      }

      # Stocker les meilleurs résultats
      self$clusters <- best$clusters
      self$centers <- best$centers
      self$r2_matrix <- best$r2_matrix
      self$inertia_total <- best$inertia_total
      self$inertia_by_cluster <- best$inertia_by_cluster
      self$n_iter <- best$n_iter

      message(sprintf("Convergence atteinte après %d itérations (meilleur run)", self$n_iter))
      message(sprintf("Inertie totale: %.4f", self$inertia_total))

      invisible(self)
    },

    # ===========================
    # PREDICT - Affectation de nouvelles variables
    # ===========================
    predict = function(X_new) {
      if (is.null(self$clusters) || is.null(self$centers)) {
        stop("fit() doit être exécuté avant predict()")
      }

      # Accepter n'importe quel nombre de variables (même 1 seule)
      X_new <- self$check_data(X_new, min_vars = 1)

      # Vérifier le nombre d'INDIVIDUS (pas de variables)
      if (nrow(X_new) != nrow(self$data)) {
        stop(sprintf("X_new doit avoir %d individus (actuellement: %d)",
                     nrow(self$data), nrow(X_new)))
      }

      # Standardiser les nouvelles variables
      Xn <- scale(X_new, center = TRUE, scale = TRUE)
      Xn <- as.matrix(Xn)

      # Calculer R² avec chaque centre (composante latente)
      cor_mat <- cor(Xn, self$centers)
      cor_mat[is.na(cor_mat)] <- 0
      r2_new <- cor_mat^2

      # Affecter au cluster avec R² max
      cl_new <- apply(r2_new, 1, which.max)
      r2_max <- apply(r2_new, 1, max)
      dist_new <- sqrt(1 - r2_max)

      result <- data.frame(
        variable = colnames(X_new),
        cluster = cl_new,
        r2_max = round(r2_max, 4),
        distance = round(dist_new, 4),
        stringsAsFactors = FALSE
      )

      # Avertissement si R² très faible
      low_r2 <- result$r2_max < 0.30
      if (any(low_r2)) {
        warning(sprintf(
          "%d variable(s) ont un R² < 30%% : %s\nCes variables sont mal représentées par les clusters existants.",
          sum(low_r2),
          paste(result$variable[low_r2], collapse = ", ")
        ))
      }

      result[order(result$cluster, -result$r2_max), ]
    },

    # ===========================
    # ILLUSTRATIVE - Variables illustratives (analyse complète)
    # ===========================
    illustrative = function(X_illust, plot = TRUE) {
      if (is.null(self$clusters) || is.null(self$centers)) {
        stop("fit() doit être exécuté avant illustrative()")
      }

      # Validation
      X_illust <- self$check_data(X_illust, min_vars = 1)

      if (nrow(X_illust) != nrow(self$data)) {
        stop(sprintf("X_illust doit avoir %d individus (actuellement: %d)",
                     nrow(self$data), nrow(X_illust)))
      }

      # Standardiser les variables illustratives
      Xi <- scale(X_illust, center = TRUE, scale = TRUE)
      Xi <- as.matrix(Xi)

      # Calculer R² avec TOUS les clusters (composantes latentes)
      cor_mat <- cor(Xi, self$centers)
      cor_mat[is.na(cor_mat)] <- 0
      r2_mat <- cor_mat^2

      # Pour chaque variable illustrative
      K <- self$k
      var_names <- colnames(X_illust)
      p_illust <- ncol(X_illust)

      # Cluster assigné (R² max)
      cl_assigned <- apply(r2_mat, 1, which.max)
      r2_own <- apply(r2_mat, 1, max)

      # R² avec le 2ème cluster le plus proche
      r2_next <- vapply(seq_len(p_illust), function(i) {
        k_i <- cl_assigned[i]
        if (K == 1) return(0)
        max(r2_mat[i, -k_i, drop = TRUE])
      }, numeric(1))

      # Ratio (1-R²own)/(1-R²next)
      ratio <- (1 - r2_own) / pmax(1 - r2_next, .Machine$double.eps)

      # Table détaillée
      illust_table <- data.frame(
        variable = var_names,
        cluster = cl_assigned,
        R2_own_pct = round(r2_own * 100, 2),
        R2_next_pct = round(r2_next * 100, 2),
        ratio = round(ratio, 3),
        stringsAsFactors = FALSE
      )
      illust_table <- illust_table[order(illust_table$cluster, -illust_table$R2_own_pct), ]

      # Matrice R² complète (toutes variables x tous clusters)
      r2_all <- as.data.frame(round(r2_mat * 100, 2))
      colnames(r2_all) <- paste0("Cluster_", 1:K)
      rownames(r2_all) <- var_names

      # Fonction de visualisation : Barplot des corrélations
      plot_func <- function() {
        par(mfrow = c(1, min(2, p_illust)), mar = c(5, 4, 4, 2))

        for (i in 1:min(p_illust, 4)) {  # Max 4 variables pour lisibilité
          var <- var_names[i]
          r2_var <- r2_mat[i, ]

          barplot(
            r2_var * 100,
            names.arg = paste0("C", 1:K),
            main = paste("Variable illustrative:", var),
            ylab = "R² (%)",
            col = ifelse(seq_len(K) == cl_assigned[i], "steelblue", "grey80"),
            border = "black",
            ylim = c(0, 100)
          )
          abline(h = 30, lty = 2, col = "red")
          text(x = 1, y = 95, labels = sprintf("Cluster %d (R²=%.1f%%)",
                                               cl_assigned[i], r2_own[i]*100),
               pos = 4, col = "steelblue", font = 2)
        }

        par(mfrow = c(1, 1))
      }

      # Résultat
      result <- list(
        table = illust_table,
        r2_all_clusters = r2_all,
        plot = plot_func
      )

      # Afficher le plot si demandé
      if (plot && p_illust > 0) {
        plot_func()
      }

      invisible(result)
    },

    # ===========================
    # SUMMARY - Résumé complet amélioré
    # ===========================
    summary = function(print_output = TRUE) {
      if (is.null(self$clusters)) {
        stop("fit() doit être exécuté avant summary()")
      }

      X <- self$X_scaled
      var_names <- colnames(X)
      K <- self$k

      # =============================
      # 1. Cluster members (R² own/next/ratio)
      # =============================
      r2 <- self$r2_matrix

      # R² avec son propre cluster
      own <- r2[cbind(seq_len(nrow(r2)), self$clusters)]

      # R² maximum avec un autre cluster
      next_r2 <- vapply(seq_len(nrow(r2)), function(i) {
        k_i <- self$clusters[i]
        if (K == 1) return(0)
        max(r2[i, -k_i, drop = TRUE])
      }, numeric(1))

      # Ratio (1-R²own)/(1-R²next) : doit être << 1
      ratio <- (1 - own) / pmax(1 - next_r2, .Machine$double.eps)

      cluster_members <- data.frame(
        cluster = self$clusters,
        variable = var_names,
        R2_own_pct = round(own * 100, 2),
        R2_next_pct = round(next_r2 * 100, 2),
        ratio = round(ratio, 3),
        stringsAsFactors = FALSE
      )
      cluster_members <- cluster_members[order(cluster_members$cluster, -cluster_members$R2_own_pct), ]

      # =============================
      # 2. Cluster summary
      # =============================
      cl_sizes <- as.numeric(table(factor(self$clusters, levels = 1:K)))
      lambdas <- self$inertia_by_cluster

      # Proportion de variance expliquée par chaque cluster
      prop <- lambdas / sum(lambdas)

      # Variance expliquée = λ_k / p_k (d'après Ricco p.19)
      var_explained <- (lambdas / pmax(cl_sizes, 1)) * 100

      cluster_summary <- data.frame(
        cluster = 1:K,
        n_variables = cl_sizes,
        lambda = round(lambdas, 4),
        variance_explained_pct = round(var_explained, 2),
        prop_inertia = round(prop, 4),
        stringsAsFactors = FALSE
      )

      # =============================
      # 3. Corrélations entre composantes latentes
      # =============================
      cor_latent <- cor(self$centers)
      colnames(cor_latent) <- rownames(cor_latent) <- paste0("Cluster_", 1:K)

      # =============================
      # 4. Qualité globale du clustering
      # =============================
      # Proportion de variables bien assignées (ratio < 1)
      well_assigned <- sum(ratio < 1) / length(ratio) * 100

      # R² moyen par cluster
      mean_r2_by_cluster <- tapply(own, self$clusters, mean)

      global_quality <- data.frame(
        metric = c("Inertie totale", "R² moyen global", "% variables bien assignées (ratio<1)"),
        value = c(
          round(self$inertia_total, 4),
          round(mean(own) * 100, 2),
          round(well_assigned, 2)
        ),
        stringsAsFactors = FALSE
      )

      # =============================
      # Affichage optionnel
      # =============================
      if (print_output) {
        self$print()

        cat("\n========================================\n")
        cat("  QUALITÉ GLOBALE DU CLUSTERING\n")
        cat("========================================\n")
        print(global_quality, row.names = FALSE)

        cat("\n========================================\n")
        cat("  RÉSUMÉ PAR CLUSTER\n")
        cat("========================================\n")
        print(cluster_summary, row.names = FALSE)

        cat("\n========================================\n")
        cat("  MEMBRES DES CLUSTERS (top 20)\n")
        cat("========================================\n")
        print(head(cluster_members, 20), row.names = FALSE)

        cat("\n========================================\n")
        cat("  CORRÉLATIONS ENTRE COMPOSANTES LATENTES\n")
        cat("========================================\n")
        print(round(cor_latent, 3))

        cat("\n========================================\n")
        cat("  R² MOYEN PAR CLUSTER\n")
        cat("========================================\n")
        print(round(mean_r2_by_cluster * 100, 2))
      }

      invisible(list(
        global_quality = global_quality,
        cluster_summary = cluster_summary,
        cluster_members = cluster_members,
        cor_latent = round(cor_latent, 4),
        mean_r2_by_cluster = round(mean_r2_by_cluster, 4),
        r2_matrix = as.data.frame(round(self$r2_matrix, 4))
      ))
    },

    # ===========================
    # ELBOW - Courbe du coude
    # ===========================
    compute_elbow = function(k_range = 2:10) {
      if (is.null(self$X_scaled)) {
        stop("fit() doit être exécuté une fois avant compute_elbow()")
      }

      X <- self$X_scaled
      n <- nrow(X)
      p <- ncol(X)

      # Sauvegarder l'état actuel
      k_orig <- self$k
      seed_orig <- self$seed

      message(sprintf("Calcul de l'elbow pour k in [%d, %d]...", min(k_range), max(k_range)))

      results <- data.frame(
        k = integer(),
        inertia = numeric(),
        stringsAsFactors = FALSE
      )

      for (k_test in k_range) {
        if (k_test > p) {
          warning(sprintf("k=%d > p=%d, ignoré", k_test, p))
          next
        }

        # Créer une instance temporaire
        temp_km <- KMeansVariablesQuant$new(
          k = k_test,
          max_iter = self$max_iter,
          n_init = self$n_init,
          tol = self$tol,
          seed = self$seed
        )

        # Fit silencieux
        suppressMessages(temp_km$fit(as.data.frame(X)))

        results <- rbind(results, data.frame(
          k = k_test,
          inertia = temp_km$inertia_total
        ))
      }

      # Restaurer k original
      self$k <- k_orig

      results
    },

    plot_elbow = function(k_range = 2:10) {
      results <- self$compute_elbow(k_range)

      plot(results$k, results$inertia,
           type = "b", pch = 19, col = "steelblue", lwd = 2,
           xlab = "Nombre de clusters (k)",
           ylab = "Inertie totale",
           main = "Méthode du Coude - K-means de Variables")
      grid()

      invisible(results)
    },

    # ===========================
    # CORRELATION CIRCLE - Cercle des corrélations
    # ===========================
    plot_correlation_circle = function(dims = c(1, 2)) {
      if (is.null(self$clusters)) {
        stop("fit() doit être exécuté avant plot_correlation_circle()")
      }

      if (self$k < max(dims)) {
        stop(sprintf("dims[%d]=%d mais k=%d", which.max(dims), max(dims), self$k))
      }

      # Corrélations entre variables et composantes latentes
      cor_mat <- cor(self$X_scaled, self$centers[, dims])

      # Cercle
      theta <- seq(0, 2*pi, length.out = 100)
      circle_x <- cos(theta)
      circle_y <- sin(theta)

      # Calculer % de variance expliquée par chaque composante (approx)
      # On utilise les corrélations moyennes comme proxy
      r2_dim1 <- mean(cor_mat[, 1]^2) * 100
      r2_dim2 <- mean(cor_mat[, 2]^2) * 100

      plot(circle_x, circle_y, type = "l", col = "grey70",
           xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
           asp = 1,
           xlab = sprintf("Composante latente %d (R²~%.1f%%)", dims[1], r2_dim1),
           ylab = sprintf("Composante latente %d (R²~%.1f%%)", dims[2], r2_dim2),
           main = "Cercle des corrélations - Variables actives")

      abline(h = 0, v = 0, lty = 3)

      # Flèches des variables
      arrows(0, 0, cor_mat[, 1], cor_mat[, 2],
             length = 0.1, col = "steelblue", lwd = 1.5)

      # Texte des variables (colorées par cluster)
      cols <- rainbow(self$k)[self$clusters]
      text(cor_mat[, 1], cor_mat[, 2],
           labels = colnames(self$X_scaled),
           pos = 3, cex = 0.7, col = cols)

      invisible(cor_mat)
    },

    # ===========================
    # BIPLOT - Variables sur axes factoriels
    # ===========================
    plot_biplot = function(dims = c(1, 2)) {
      if (is.null(self$clusters)) {
        stop("fit() doit être exécuté avant plot_biplot()")
      }

      if (self$k < max(dims)) {
        stop(sprintf("dims[%d]=%d mais k=%d", which.max(dims), max(dims), self$k))
      }

      # ACP des variables (transposée de X)
      X_t <- t(self$X_scaled)
      pca <- prcomp(X_t, center = TRUE, scale. = FALSE)

      # Coordonnées des variables sur les axes factoriels
      coords <- pca$x[, dims]

      # % variance expliquée
      var_expl <- summary(pca)$importance[2, dims] * 100

      plot(coords[, 1], coords[, 2],
           type = "n",
           xlab = sprintf("PC%d (%.1f%%)", dims[1], var_expl[1]),
           ylab = sprintf("PC%d (%.1f%%)", dims[2], var_expl[2]),
           main = "Biplot - Variables dans l'espace factoriel")

      abline(h = 0, v = 0, lty = 3, col = "grey70")

      # Points colorés par cluster
      cols <- rainbow(self$k)[self$clusters]
      points(coords[, 1], coords[, 2], pch = 19, col = cols, cex = 1.2)
      text(coords[, 1], coords[, 2],
           labels = colnames(self$X_scaled),
           pos = 3, cex = 0.7, col = cols)

      # Légende
      legend("topright", legend = paste("Cluster", 1:self$k),
             col = rainbow(self$k), pch = 19, cex = 0.8)

      invisible(coords)
    },

    # ===========================
    # ELBOW with AUTO-DETECTION
    # ===========================
    elbow = function(k_range = 2:10, plot = TRUE) {
      if (is.null(self$X_scaled)) {
        stop("fit() doit être exécuté une fois avant elbow()")
      }

      X <- self$X_scaled
      n <- nrow(X)
      p <- ncol(X)

      # Sauvegarder l'état actuel
      k_orig <- self$k
      seed_orig <- self$seed

      message(sprintf("Calcul de l'elbow pour k in [%d, %d]...", min(k_range), max(k_range)))

      results <- data.frame(
        k = integer(),
        inertia = numeric(),
        gain = numeric(),
        stringsAsFactors = FALSE
      )

      prev_inertia <- NULL

      for (k_test in k_range) {
        if (k_test > p) {
          warning(sprintf("k=%d > p=%d, ignoré", k_test, p))
          next
        }

        # Créer une instance temporaire
        temp_km <- KMeansVariablesQuant$new(
          k = k_test,
          max_iter = self$max_iter,
          n_init = self$n_init,
          tol = self$tol,
          seed = self$seed
        )

        # Fit silencieux
        suppressMessages(temp_km$fit(as.data.frame(X)))

        # Calculer le gain marginal
        gain <- if (!is.null(prev_inertia)) {
          temp_km$inertia_total - prev_inertia
        } else {
          NA
        }

        results <- rbind(results, data.frame(
          k = k_test,
          inertia = temp_km$inertia_total,
          gain = gain
        ))

        prev_inertia <- temp_km$inertia_total
      }

      # Restaurer k original
      self$k <- k_orig

      # Détection automatique du coude (gain marginal maximal)
      optimal_k <- results$k[which.max(results$gain[-1])] + 1  # +1 car gain décalé

      if (is.na(optimal_k) || optimal_k < min(k_range)) {
        optimal_k <- results$k[which.max(results$inertia)]
      }

      # Fonction de plot
      plot_func <- function() {
        par(mar = c(5, 5, 4, 2))
        plot(results$k, results$inertia,
             type = "b", pch = 19, col = "steelblue", lwd = 2,
             xlab = "Nombre de clusters (k)",
             ylab = "Inertie totale (Σ λₖ)",
             main = "Méthode du Coude - K-means de Variables",
             cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1,
             ylim = range(results$inertia) * c(0.95, 1.05))

        grid(col = "gray90", lty = 1)

        # Marquer le k optimal
        points(optimal_k, results$inertia[results$k == optimal_k],
               pch = 21, cex = 2, col = "red", bg = "yellow", lwd = 2)

        text(optimal_k, results$inertia[results$k == optimal_k],
             labels = sprintf("k = %d\n(optimal)", optimal_k),
             pos = 3, col = "red", font = 2, cex = 0.9)

        # Ligne pointillée verticale
        abline(v = optimal_k, lty = 2, col = "red", lwd = 1.5)
      }

      # Afficher le plot si demandé
      if (plot) {
        plot_func()
      }

      # Print summary
      cat("\n=== K-Means Elbow Analysis ===\n")
      cat(sprintf("Optimal k: %d\n", optimal_k))
      cat(sprintf("Range tested: %d to %d\n", min(k_range), max(k_range)))
      cat("\nResults table:\n")
      print(results)

      # Retourner la liste de résultats
      invisible(list(
        optimal_k = optimal_k,
        results = results,
        plot = plot_func
      ))
    },

    # ===========================
    # PRINT - Affichage amélioré
    # ===========================
    print = function(...) {
      cat("========================================\n")
      cat("  K-MEANS DE VARIABLES QUANTITATIVES\n")
      cat("========================================\n")
      cat(sprintf("Paramètres:\n"))
      cat(sprintf("  - Nombre de clusters (k)    : %d\n", self$k))
      cat(sprintf("  - Itérations max par run    : %d\n", self$max_iter))
      cat(sprintf("  - Nombre d'initialisations  : %d\n", self$n_init))
      cat(sprintf("  - Tolérance convergence     : %.1e\n", self$tol))

      if (!is.null(self$seed)) {
        cat(sprintf("  - Graine aléatoire          : %d\n", self$seed))
      }

      if (!is.null(self$data)) {
        cat(sprintf("\nDonnées:\n"))
        cat(sprintf("  - Individus                 : %d\n", nrow(self$data)))
        cat(sprintf("  - Variables                 : %d\n", ncol(self$data)))
      }

      if (!is.null(self$clusters)) {
        cat(sprintf("\nRésultats:\n"))
        cat(sprintf("  - Inertie totale            : %.4f\n", self$inertia_total))
        cat(sprintf("  - Itérations (meilleur run) : %d\n", self$n_iter))
        cat(sprintf("\nTaille des clusters:\n"))
        tbl <- table(self$clusters)
        for (i in 1:length(tbl)) {
          cat(sprintf("  - Cluster %d                 : %d variables\n", i, tbl[i]))
        }
      } else {
        cat("\nStatut: Modèle non ajusté. Utilisez fit() pour entraîner.\n")
      }

      cat("========================================\n")

      invisible(self)
    },

    get_clusters_table = function() {
      if (is.null(self$clusters))
        stop("fit() doit être exécuté avant get_clusters_table()")

      var_names <- colnames(self$data)

      df <- data.frame(
        variable = var_names,
        cluster = self$clusters,
        stringsAsFactors = FALSE
      )

      df[order(df$cluster, df$variable), ]
    }
  ),

  # =========================================================================
  # METHODES PRIVEES
  # =========================================================================
  private = list(

    # ---------------------------
    # Calcul de la composante latente (PC1) d'un cluster
    # ---------------------------
    latent_center = function(X_cluster) {
      n_vars <- ncol(X_cluster)
      n_obs <- nrow(X_cluster)

      # CAS 1: Cluster vide
      if (n_vars == 0) {
        warning("Cluster vide détecté - retour d'un centre aléatoire")
        fake <- rnorm(n_obs)
        fake <- scale(fake, center = TRUE, scale = TRUE)
        return(list(center = as.numeric(fake)))
      }

      # CAS 2: Une seule variable
      if (n_vars == 1) {
        v <- X_cluster[, 1]
        if (sd(v) < .Machine$double.eps) {
          warning("Variable avec variance nulle détectée")
          fake <- rnorm(n_obs)
          fake <- scale(fake, center = TRUE, scale = TRUE)
          return(list(center = as.numeric(fake)))
        }
        center <- as.numeric(scale(v, center = TRUE, scale = TRUE))
        return(list(center = center))
      }

      # CAS 3: Plusieurs variables -> ACP
      pca <- prcomp(X_cluster, center = TRUE, scale. = FALSE, rank. = 1)

      # Extraire le PC1 (scores factoriels)
      center <- pca$x[, 1]

      # Normaliser le PC1 (moyenne 0, écart-type 1)
      center <- as.numeric(scale(center, center = TRUE, scale = TRUE))

      # CAS 4: PC1 dégénéré
      if (anyNA(center) || sd(center) < .Machine$double.eps) {
        warning("PC1 avec variance nulle détecté")
        fake <- rnorm(n_obs)
        fake <- scale(fake, center = TRUE, scale = TRUE)
        return(list(center = as.numeric(fake)))
      }

      list(center = center)
    },

    # ---------------------------
    # Un run complet de K-means
    # ---------------------------
    single_run = function(X, K, max_iter, tol) {
      n <- nrow(X)
      p <- ncol(X)

      # Initialisation aléatoire
      clusters <- sample.int(K, p, replace = TRUE)

      # S'assurer qu'aucun cluster n'est vide
      for (k in 1:K) {
        if (sum(clusters == k) == 0) {
          clusters[sample.int(p, 1)] <- k
        }
      }

      # Structures pour stocker les résultats
      inertia_old <- -Inf
      centers <- matrix(0, nrow = n, ncol = K)
      r2_mat <- matrix(0, nrow = p, ncol = K)

      # Boucle principale
      for (iter in seq_len(max_iter)) {

        # ETAPE 1: Recalculer les centres (PC1) de chaque cluster
        for (k in 1:K) {
          idx <- which(clusters == k)
          lc <- private$latent_center(X[, idx, drop = FALSE])
          centers[, k] <- lc$center
        }

        # ETAPE 2: Calculer les R² entre chaque variable et chaque centre
        cor_mat <- cor(X, centers)
        cor_mat[is.na(cor_mat)] <- 0
        r2_mat <- cor_mat^2

        # ETAPE 3: Réaffecter chaque variable au cluster avec R² max
        clusters_new <- apply(r2_mat, 1, which.max)

        # ETAPE 4: Gérer les clusters vides
        for (k in 1:K) {
          if (sum(clusters_new == k) == 0) {
            farthest <- which.min(apply(r2_mat, 1, max))
            clusters_new[farthest] <- k
          }
        }

        # Calculer l'inertie comme SOMME des R²
        lambdas <- numeric(K)
        for (k in 1:K) {
          idx <- which(clusters_new == k)
          if (length(idx) > 0) {
            lambdas[k] <- sum(r2_mat[idx, k])
          }
        }
        inertia_new <- sum(lambdas)

        # CONVERGENCE
        if (identical(clusters_new, clusters)) {
          clusters <- clusters_new
          inertia_old <- inertia_new
          break
        }

        if (abs(inertia_new - inertia_old) < tol) {
          clusters <- clusters_new
          inertia_old <- inertia_new
          break
        }

        clusters <- clusters_new
        inertia_old <- inertia_new
      }

      # Recalcul final propre
      for (k in 1:K) {
        idx <- which(clusters == k)
        lc <- private$latent_center(X[, idx, drop = FALSE])
        centers[, k] <- lc$center
      }

      cor_mat <- cor(X, centers)
      cor_mat[is.na(cor_mat)] <- 0
      r2_mat <- cor_mat^2

      # Lambda final = somme des R² dans chaque cluster
      lambdas <- numeric(K)
      for (k in 1:K) {
        idx <- which(clusters == k)
        if (length(idx) > 0) {
          lambdas[k] <- sum(r2_mat[idx, k])
        }
      }

      # Noms
      colnames(centers) <- paste0("Cluster_", 1:K)
      colnames(r2_mat) <- paste0("Cluster_", 1:K)
      rownames(r2_mat) <- colnames(X)

      list(
        clusters = as.integer(clusters),
        centers = centers,
        r2_matrix = r2_mat,
        inertia_total = sum(lambdas),
        inertia_by_cluster = lambdas,
        n_iter = iter
      )
    }
  )
)
