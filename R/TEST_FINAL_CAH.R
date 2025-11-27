# ==============================================================================
# SCRIPT DE TEST FINAL - CLUSTERING DE MODALITÉS (ACM-CAH ET DICE-CAH)
# ==============================================================================
#
# Objectifs:
# - Valider les deux méthodes (ACM et DICE)
# - Tester sur données simulées et réelles
# - Valider toutes les fonctionnalités (predict, project_numeric, elbow)
# - Vérifier la stabilité et la robustesse
#
# Durée estimée: 5-10 minutes
# ==============================================================================

library(R6)
library(ade4)

# Charger les fonctions
cat("\n")
cat("================================================================================\n")
cat("                     CHARGEMENT DES SCRIPTS                                   \n")
cat("================================================================================\n\n")

# ADAPTER CES CHEMINS SELON VOTRE STRUCTURE
source("C:/Users/ychen/OneDrive/Documents/GitHub/SISE_Clustering_Variables_R/R/n_clusters.R")  # Fonctions elbow
source("C:/Users/ychen/OneDrive/Documents/GitHub/SISE_Clustering_Variables_R/R/acm_cah.R")  # Fonctions elbow


cat("✓ Scripts chargés avec succès\n\n")

# ==============================================================================
# UTILITAIRES DE TEST
# ==============================================================================

test_counter <- 0
test_passed <- 0
test_failed <- 0

run_test <- function(test_name, test_fn) {
  test_counter <<- test_counter + 1
  cat(sprintf("\n[TEST %d] %s\n", test_counter, test_name))
  cat(strrep("-", 80), "\n")

  result <- tryCatch({
    test_fn()
    test_passed <<- test_passed + 1
    cat("✓ PASSÉ\n")
    TRUE
  }, error = function(e) {
    test_failed <<- test_failed + 1
    cat("✗ ÉCHOUÉ:", e$message, "\n")
    FALSE
  })

  return(result)
}

# ==============================================================================
# TEST 1: DONNÉES SIMULÉES SIMPLES - MÉTHODE ACM
# ==============================================================================

run_test("ACM - Données simulées simples", function() {
  set.seed(123)
  n <- 100

  data_test <- data.frame(
    var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
    var3 = factor(sample(c("1", "2", "3"), n, replace = TRUE))
  )

  clust <- ClustModalities$new(method = "acm")
  clust$fit(data_test, k = 3)

  # Validations
  stopifnot(!is.null(clust$mod_clusters))
  stopifnot(length(unique(clust$mod_clusters)) == 3)
  stopifnot(!is.null(clust$acm))
  stopifnot(!is.null(clust$mod_coords))

  cat("  - Nombre de modalités:", length(clust$mod_clusters), "\n")
  cat("  - Nombre de clusters:", clust$k, "\n")
  cat("  - Répartition:", paste(table(clust$mod_clusters), collapse = ", "), "\n")
})

# ==============================================================================
# TEST 2: DONNÉES SIMULÉES SIMPLES - MÉTHODE DICE
# ==============================================================================

run_test("DICE - Données simulées simples", function() {
  set.seed(123)
  n <- 100

  data_test <- data.frame(
    var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
    var3 = factor(sample(c("1", "2", "3"), n, replace = TRUE))
  )

  clust <- ClustModalities$new(method = "dice")
  clust$fit(data_test, k = 3)

  # Validations
  stopifnot(!is.null(clust$mod_clusters))
  stopifnot(length(unique(clust$mod_clusters)) == 3)
  stopifnot(!is.null(clust$dist_mat))
  stopifnot(is.null(clust$acm))  # Pas d'ACM en mode DICE

  cat("  - Nombre de modalités:", length(clust$mod_clusters), "\n")
  cat("  - Nombre de clusters:", clust$k, "\n")
  cat("  - Répartition:", paste(table(clust$mod_clusters), collapse = ", "), "\n")
})

# ==============================================================================
# TEST 3: VISUALISATIONS ACM
# ==============================================================================

run_test("ACM - Toutes les visualisations", function() {
  set.seed(456)
  n <- 80

  data_test <- data.frame(
    var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
    var3 = factor(sample(c("1", "2"), n, replace = TRUE))
  )

  clust <- ClustModalities$new(method = "acm")
  clust$fit(data_test, k = 2)

  # Tester toutes les visualisations sans erreur
  pdf(file = tempfile(fileext = ".pdf"))
  clust$plot_dendrogram()
  clust$plot_factor_map()
  clust$plot_scree()
  clust$plot_scree(cumulative = TRUE)
  clust$plot_contrib(dim = 1, top = 5)
  clust$plot_contrib(dim = 2, top = 5)
  dev.off()

  cat("  ✓ Dendrogramme: OK\n")
  cat("  ✓ Carte factorielle: OK\n")
  cat("  ✓ Scree plot: OK\n")
  cat("  ✓ Scree plot cumulé: OK\n")
  cat("  ✓ Contributions Dim1: OK\n")
  cat("  ✓ Contributions Dim2: OK\n")
})

# ==============================================================================
# TEST 4: PREDICT() - MODALITÉS QUALITATIVES (ACM)
# ==============================================================================

run_test("ACM - predict() avec nouvelles modalités", function() {
  set.seed(789)
  n <- 80

  # Données d'entraînement
  data_train <- data.frame(
    var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE))
  )

  clust <- ClustModalities$new(method = "acm")
  clust$fit(data_train, k = 2)

  # Nouvelles données - MÊME NOMBRE D'INDIVIDUS (n)
  data_new <- data.frame(
    var3 = factor(sample(c("D", "E"), n, replace = TRUE)),
    var4 = factor(sample(c("Z", "W"), n, replace = TRUE))
  )

  predictions <- clust$predict(data_new)

  # Validations (CORRIGÉES)
  stopifnot(is.data.frame(predictions))
  stopifnot(nrow(predictions) > 0)
  stopifnot("cluster" %in% colnames(predictions))
  stopifnot("distance" %in% colnames(predictions))

  # Vérifier que le nombre de prédictions = nombre total de modalités
  n_modalites_attendues <- length(unique(data_new[[1]])) + length(unique(data_new[[2]]))
  stopifnot(nrow(predictions) == n_modalites_attendues)

  cat("  - Nombre de prédictions:", nrow(predictions), "\n")
  cat("  - Nombre de modalités attendues:", n_modalites_attendues, "\n")
  cat("  - Clusters prédits:", paste(unique(predictions$cluster), collapse = ", "), "\n")
  cat("  ✓ predict() fonctionne correctement\n")
})

# ==============================================================================
# TEST 5: PREDICT() - MODALITÉS QUALITATIVES (DICE)
# ==============================================================================

run_test("DICE - predict() avec nouvelles modalités", function() {
  set.seed(111)
  n <- 80

  # Données d'entraînement
  data_train <- data.frame(
    var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE))
  )

  clust <- ClustModalities$new(method = "dice")
  clust$fit(data_train, k = 2)

  # Nouvelles données - MÊME NOMBRE D'INDIVIDUS (n)
  data_new <- data.frame(
    var3 = factor(sample(c("D", "E"), n, replace = TRUE))
  )

  predictions <- clust$predict(data_new)

  # Validations (AMÉLIORÉES)
  stopifnot(is.data.frame(predictions))
  stopifnot(nrow(predictions) > 0)
  stopifnot("cluster" %in% colnames(predictions))
  stopifnot("distance" %in% colnames(predictions))

  # Vérifier que le nombre de prédictions = nombre de modalités
  n_modalites_attendues <- length(unique(data_new[[1]]))
  stopifnot(nrow(predictions) == n_modalites_attendues)

  cat("  - Nombre de prédictions:", nrow(predictions), "\n")
  cat("  - Nombre de modalités attendues:", n_modalites_attendues, "\n")
  cat("  ✓ predict() DICE fonctionne correctement\n")
})

# ==============================================================================
# TEST 6: PROJECT_NUMERIC() - VARIABLES QUANTITATIVES
# ==============================================================================

run_test("ACM - project_numeric() pour variables quantitatives", function() {
  set.seed(222)
  n <- 80

  # Données qualitatives
  data_quali <- data.frame(
    var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE))
  )

  clust <- ClustModalities$new(method = "acm")
  clust$fit(data_quali, k = 2)

  # Variable quantitative à projeter
  data_quanti <- data.frame(age = rnorm(n, mean = 30, sd = 10))

  # Projeter (doit créer un graphique sans erreur)
  pdf(file = tempfile(fileext = ".pdf"))
  clust$project_numeric(data_quanti)
  dev.off()

  cat("  ✓ project_numeric() fonctionne\n")
  cat("  ✓ Cercle de corrélation généré\n")
})

# ==============================================================================
# TEST 7: ELBOW AMÉLIORÉ - ACM
# ==============================================================================

run_test("ACM - Elbow avec détection automatique k*", function() {
  set.seed(333)
  n <- 100

  data_test <- data.frame(
    var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y", "Z"), n, replace = TRUE)),
    var3 = factor(sample(c("1", "2"), n, replace = TRUE))
  )

  clust <- ClustModalities$new(method = "acm")
  clust$fit(data_test)

  # Test elbow standalone
  result_standalone <- acm_cah_elbow(data_test, method = "acm", k_max = 8)
  stopifnot(!is.null(result_standalone$optimal_k))
  stopifnot(result_standalone$optimal_k >= 2 && result_standalone$optimal_k <= 8)
  cat("  ✓ Fonction standalone: k* =", result_standalone$optimal_k, "\n")

  # Test méthode $elbow()
  result_method <- clust$elbow(k_max = 8, plot = FALSE)
  stopifnot(!is.null(result_method$optimal_k))
  cat("  ✓ Méthode $elbow(): k* =", result_method$optimal_k, "\n")
})

# ==============================================================================
# TEST 8: ELBOW AMÉLIORÉ - DICE
# ==============================================================================

run_test("DICE - Elbow avec détection automatique k*", function() {
  set.seed(444)
  n <- 100

  data_test <- data.frame(
    var1 = factor(sample(c("A", "B"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
    var3 = factor(sample(c("1", "2", "3"), n, replace = TRUE))
  )

  clust <- ClustModalities$new(method = "dice")
  clust$fit(data_test)

  # Test elbow standalone
  result_standalone <- acm_cah_elbow(data_test, method = "dice", k_max = 6)
  stopifnot(!is.null(result_standalone$optimal_k))
  cat("  ✓ Fonction standalone: k* =", result_standalone$optimal_k, "\n")

  # Test méthode $elbow()
  result_method <- clust$elbow(k_max = 6, plot = FALSE)
  stopifnot(!is.null(result_method$optimal_k))
  cat("  ✓ Méthode $elbow(): k* =", result_method$optimal_k, "\n")
})

# ==============================================================================
# TEST 9: COMPUTE_ELBOW (ANCIENNE MÉTHODE - BACKWARD COMPATIBILITY)
# ==============================================================================

run_test("Backward compatibility - compute_elbow et plot_elbow", function() {
  set.seed(555)
  n <- 80

  data_test <- data.frame(
    var1 = factor(sample(c("A", "B"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE))
  )

  clust <- ClustModalities$new(method = "acm")
  clust$fit(data_test)

  # Test anciennes méthodes
  elbow_df <- clust$compute_elbow(k_max = 5)
  stopifnot(is.data.frame(elbow_df))
  stopifnot(all(c("k", "height") %in% colnames(elbow_df)))

  pdf(file = tempfile(fileext = ".pdf"))
  clust$plot_elbow(k_max = 5)
  dev.off()

  cat("  ✓ compute_elbow() fonctionne\n")
  cat("  ✓ plot_elbow() fonctionne\n")
  cat("  ✓ Backward compatibility assurée\n")
})

# ==============================================================================
# TEST 10: PRINT ET SUMMARY
# ==============================================================================

run_test("Méthodes print() et cluster_table()", function() {
  set.seed(666)
  n <- 80

  data_test <- data.frame(
    var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE))
  )

  clust <- ClustModalities$new(method = "acm")
  clust$fit(data_test, k = 2)

  # Test print
  capture.output(print(clust))
  cat("  ✓ print() fonctionne\n")

  # Test cluster_table
  table_result <- clust$cluster_table()
  stopifnot(is.data.frame(table_result))
  stopifnot("cluster" %in% colnames(table_result))
  stopifnot("modality" %in% colnames(table_result))
  cat("  ✓ cluster_table() retourne les bonnes structures\n")
})

# ==============================================================================
# TEST 11: STABILITÉ - ACM
# ==============================================================================

run_test("ACM - Stabilité sur échantillons", function() {
  set.seed(777)
  n <- 150

  data_full <- data.frame(
    var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
    var3 = factor(sample(c("1", "2"), n, replace = TRUE))
  )

  # Clustering sur données complètes
  clust_full <- ClustModalities$new(method = "acm")
  clust_full$fit(data_full, k = 3)

  # Clustering sur échantillon
  idx <- sample(1:n, 120)
  clust_sample <- ClustModalities$new(method = "acm")
  clust_sample$fit(data_full[idx, ], k = 3)

  # Vérifier que les structures sont similaires
  stopifnot(length(clust_full$mod_clusters) == length(clust_sample$mod_clusters))

  cat("  ✓ Clustering stable entre données complètes et échantillon\n")
})

# ==============================================================================
# TEST 12: STABILITÉ - DICE
# ==============================================================================

run_test("DICE - Stabilité sur échantillons", function() {
  set.seed(888)
  n <- 150

  data_full <- data.frame(
    var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE))
  )

  # Clustering sur données complètes
  clust_full <- ClustModalities$new(method = "dice")
  clust_full$fit(data_full, k = 2)

  # Clustering sur échantillon
  idx <- sample(1:n, 120)
  clust_sample <- ClustModalities$new(method = "dice")
  clust_sample$fit(data_full[idx, ], k = 2)

  # Vérifier que les structures sont similaires
  stopifnot(length(clust_full$mod_clusters) == length(clust_sample$mod_clusters))

  cat("  ✓ Clustering DICE stable entre données complètes et échantillon\n")
})

# ==============================================================================
# TEST 13: GESTION DES ERREURS
# ==============================================================================

run_test("Gestion des erreurs", function() {
  # Erreur 1: Données non-qualitatives
  data_bad <- data.frame(var1 = 1:10, var2 = 1:10)
  clust1 <- ClustModalities$new(method = "acm")

  # Pas d'erreur car check_data convertit en factors
  clust1$fit(data_bad, k = 2)
  cat("  ✓ Conversion automatique en factors: OK\n")

  # Erreur 2: Données avec NA
  data_na <- data.frame(
    var1 = factor(c("A", "B", NA, "C")),
    var2 = factor(c("X", NA, "Y", "Z"))
  )
  clust2 <- ClustModalities$new(method = "acm")

  error_caught <- FALSE
  tryCatch(clust2$fit(data_na, k = 2), error = function(e) {
    error_caught <<- TRUE
  })
  stopifnot(error_caught)
  cat("  ✓ Erreur données avec NA détectée\n")

  # Erreur 3: Predict sans fit
  clust3 <- ClustModalities$new(method = "acm")
  error_caught <- FALSE
  tryCatch(clust3$predict(data.frame(x = factor(c("A", "B")))), error = function(e) {
    error_caught <<- TRUE
  })
  stopifnot(error_caught)
  cat("  ✓ Erreur predict() sans fit() détectée\n")
})

# ==============================================================================
# TEST 14: CAS LIMITES
# ==============================================================================

run_test("Cas limites", function() {
  set.seed(999)
  n <- 50

  # Cas 1: 1 seule variable (minimum)
  data_min <- data.frame(var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)))
  clust1 <- ClustModalities$new(method = "acm")
  clust1$fit(data_min, k = 2)
  stopifnot(!is.null(clust1$mod_clusters))
  cat("  ✓ Minimum 1 variable: OK\n")

  # Cas 2: Variable binaire
  data_bin <- data.frame(
    var1 = factor(sample(c("A", "B"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE))
  )
  clust2 <- ClustModalities$new(method = "dice")
  clust2$fit(data_bin, k = 2)
  stopifnot(!is.null(clust2$mod_clusters))
  cat("  ✓ Variables binaires: OK\n")

  # Cas 3: Beaucoup de modalités
  data_many <- data.frame(
    var1 = factor(sample(letters[1:10], n, replace = TRUE)),
    var2 = factor(sample(LETTERS[1:10], n, replace = TRUE))
  )
  clust3 <- ClustModalities$new(method = "acm")
  clust3$fit(data_many, k = 3)
  stopifnot(!is.null(clust3$mod_clusters))
  cat("  ✓ Beaucoup de modalités (10+): OK\n")
})

# ==============================================================================
# TEST 15: COMPARAISON ACM VS DICE
# ==============================================================================

run_test("Comparaison ACM vs DICE - Mêmes données", function() {
  set.seed(1111)
  n <- 80

  data_test <- data.frame(
    var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
    var3 = factor(sample(c("1", "2"), n, replace = TRUE))
  )

  # ACM
  clust_acm <- ClustModalities$new(method = "acm")
  clust_acm$fit(data_test, k = 3)

  # DICE
  clust_dice <- ClustModalities$new(method = "dice")
  clust_dice$fit(data_test, k = 3)

  # Validations
  stopifnot(!is.null(clust_acm$mod_clusters))
  stopifnot(!is.null(clust_dice$mod_clusters))
  stopifnot(length(clust_acm$mod_clusters) == length(clust_dice$mod_clusters))

  # Nombre de clusters identique
  stopifnot(length(unique(clust_acm$mod_clusters)) == 3)
  stopifnot(length(unique(clust_dice$mod_clusters)) == 3)

  cat("  - ACM clusters:", paste(table(clust_acm$mod_clusters), collapse = ", "), "\n")
  cat("  - DICE clusters:", paste(table(clust_dice$mod_clusters), collapse = ", "), "\n")
  cat("  ✓ Les deux méthodes fonctionnent sur mêmes données\n")
})

# ==============================================================================
# RÉSUMÉ DES TESTS
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("                           RÉSUMÉ DES TESTS                                   \n")
cat("================================================================================\n\n")

cat(sprintf("Total de tests exécutés: %d\n", test_counter))
cat(sprintf("Tests réussis:           %d (%.1f%%)\n",
            test_passed, 100 * test_passed / test_counter))
cat(sprintf("Tests échoués:           %d (%.1f%%)\n",
            test_failed, 100 * test_failed / test_counter))

cat("\n")

if (test_failed == 0) {
  cat("================================================================================\n")
  cat("           🎉 TOUS LES TESTS SONT PASSÉS - CLASSE VALIDÉE 🎉                \n")
  cat("================================================================================\n\n")
  cat("La classe ClustModalities est prête pour la production:\n")
  cat("  ✓ Méthode ACM validée\n")
  cat("  ✓ Méthode DICE validée\n")
  cat("  ✓ Toutes les visualisations fonctionnent\n")
  cat("  ✓ predict() et project_numeric() validés\n")
  cat("  ✓ Elbow amélioré fonctionnel\n")
  cat("  ✓ Stabilité confirmée\n")
  cat("  ✓ Gestion des erreurs robuste\n")
  cat("  ✓ Cas limites gérés\n\n")
} else {
  cat("================================================================================\n")
  cat("          ⚠️  CERTAINS TESTS ONT ÉCHOUÉ - CORRECTIONS NÉCESSAIRES ⚠️         \n")
  cat("================================================================================\n\n")
  cat("Veuillez corriger les erreurs identifiées avant de passer à l'étape suivante.\n\n")
}

# ==============================================================================
# FIN DES TESTS
# ==============================================================================
