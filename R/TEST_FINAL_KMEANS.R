# ==============================================================================
# SCRIPT DE TEST FINAL - K-MEANS VARIABLES QUANTITATIVES
# ==============================================================================
#
# Objectifs:
# - Valider toutes les corrections de bugs
# - Tester sur données simulées et réelles
# - Valider les nouvelles fonctionnalités (elbow amélioré, predict flexible)
# - Vérifier la stabilité et la robustesse
#
# Durée estimée: 5-10 minutes
# ==============================================================================

library(R6)

# Charger les fonctions
cat("\n")
cat("================================================================================\n")
cat("                     CHARGEMENT DES SCRIPTS                                   \n")
cat("================================================================================\n\n")

# ADAPTER CES CHEMINS SELON VOTRE STRUCTURE
source("C:/Users/ychen/OneDrive/Documents/GitHub/SISE_Clustering_Variables_R/R/n_clusters.R")  # Fonctions elbow
source("C:/Users/ychen/OneDrive/Documents/GitHub/SISE_Clustering_Variables_R/R/kmeans.R")  # Fonctions elbow

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
# TEST 1: DONNÉES SIMULÉES - STRUCTURE CLAIRE (3 GROUPES)
# ==============================================================================

run_test("Données simulées - 3 groupes clairs", function() {
  set.seed(123)
  n <- 100

  # Groupe 1: 3 variables corrélées
  X1 <- rnorm(n)
  var1 <- X1 + rnorm(n, 0, 0.3)
  var2 <- X1 + rnorm(n, 0, 0.3)
  var3 <- X1 + rnorm(n, 0, 0.3)

  # Groupe 2: 2 variables corrélées
  X2 <- rnorm(n)
  var4 <- X2 + rnorm(n, 0, 0.3)
  var5 <- X2 + rnorm(n, 0, 0.3)

  # Groupe 3: 3 variables corrélées
  X3 <- rnorm(n)
  var6 <- X3 + rnorm(n, 0, 0.3)
  var7 <- X3 + rnorm(n, 0, 0.3)
  var8 <- X3 + rnorm(n, 0, 0.3)

  data_test <- data.frame(var1, var2, var3, var4, var5, var6, var7, var8)

  # Fit
  km <- KMeansVariablesQuant$new(k = 3, n_init = 20, seed = 42)
  km$fit(data_test)

  # Validations
  stopifnot(!is.null(km$clusters))
  stopifnot(length(km$clusters) == 8)
  stopifnot(length(unique(km$clusters)) == 3)
  stopifnot(!is.null(km$inertia_total))

  cat("  - Nombre de variables:", ncol(data_test), "\n")
  cat("  - Nombre de clusters:", km$k, "\n")
  cat("  - Inertie totale:", round(km$inertia_total, 4), "\n")
  cat("  - Répartition:", paste(table(km$clusters), collapse = ", "), "\n")
})

# ==============================================================================
# TEST 2: VALIDATION BUG #1 - FORMULE LAMBDA (INERTIE CROISSANTE)
# ==============================================================================

run_test("Bug #1 - Inertie croissante avec k", function() {
  set.seed(123)
  n <- 100
  data_test <- data.frame(
    var1 = rnorm(n), var2 = rnorm(n), var3 = rnorm(n),
    var4 = rnorm(n), var5 = rnorm(n), var6 = rnorm(n)
  )

  inertias <- numeric()
  for (k in 2:5) {
    km <- KMeansVariablesQuant$new(k = k, n_init = 10, seed = 42)
    km$fit(data_test)
    inertias <- c(inertias, km$inertia_total)
  }

  # Vérifier croissance stricte
  diffs <- diff(inertias)
  stopifnot(all(diffs > 0))

  cat("  - Inertie k=2:", round(inertias[1], 4), "\n")
  cat("  - Inertie k=3:", round(inertias[2], 4), "(+", round(diffs[1], 4), ")\n")
  cat("  - Inertie k=4:", round(inertias[3], 4), "(+", round(diffs[2], 4), ")\n")
  cat("  - Inertie k=5:", round(inertias[4], 4), "(+", round(diffs[3], 4), ")\n")
  cat("  ✓ Inertie strictement croissante\n")
})

# ==============================================================================
# TEST 3: VALIDATION BUG #2 - VARIANCE EXPLIQUÉE < 100%
# ==============================================================================

run_test("Bug #2 - Variance expliquée entre 0-100%", function() {
  set.seed(456)
  n <- 80
  data_test <- data.frame(
    var1 = rnorm(n), var2 = rnorm(n), var3 = rnorm(n),
    var4 = rnorm(n), var5 = rnorm(n)
  )

  km <- KMeansVariablesQuant$new(k = 3, n_init = 10, seed = 42)
  km$fit(data_test)

  results <- km$summary(print_summary = FALSE)
  var_expl <- results$clust_summary$variance_explained_pct

  # Vérifier bornes
  stopifnot(all(var_expl >= 0 & var_expl <= 100))

  cat("  - Variances expliquées:\n")
  for (i in seq_along(var_expl)) {
    cat(sprintf("    Cluster %d: %.2f%%\n", i, var_expl[i]))
  }
  cat("  ✓ Toutes les variances dans [0, 100%]\n")
})

# ==============================================================================
# TEST 4: VALIDATION BUG #3 - PREDICT() FLEXIBLE
# ==============================================================================

run_test("Bug #3 - predict() avec nombre variable de variables", function() {
  set.seed(789)
  n <- 50

  # Données d'entraînement
  data_train <- data.frame(
    var1 = rnorm(n), var2 = rnorm(n), var3 = rnorm(n),
    var4 = rnorm(n), var5 = rnorm(n)
  )

  km <- KMeansVariablesQuant$new(k = 2, n_init = 10, seed = 42)
  km$fit(data_train)

  # Test 1: Même nombre de variables (OK)
  data_new_ok <- data.frame(
    new1 = rnorm(n), new2 = rnorm(n), new3 = rnorm(n),
    new4 = rnorm(n), new5 = rnorm(n)
  )
  pred1 <- suppressWarnings(km$predict(data_new_ok))
  stopifnot(nrow(pred1) == ncol(data_new_ok))  # 1 ligne par variable
  cat("  ✓ Prédiction avec 5 variables: OK\n")

  # Test 2: Plus de variables (OK avec warning)
  data_new_more <- data.frame(
    new1 = rnorm(n), new2 = rnorm(n), new3 = rnorm(n),
    new4 = rnorm(n), new5 = rnorm(n), new6 = rnorm(n)
  )
  pred2 <- suppressWarnings(km$predict(data_new_more))
  stopifnot(nrow(pred2) == ncol(data_new_more))
  cat("  ✓ Prédiction avec 6 variables: OK (avec warning)\n")

  # Test 3: Moins de variables (OK avec warning si R² < 30%)
  data_new_less <- data.frame(
    new1 = rnorm(n), new2 = rnorm(n), new3 = rnorm(n)
  )
  pred3 <- suppressWarnings(km$predict(data_new_less))
  stopifnot(nrow(pred3) == ncol(data_new_less))
  cat("  ✓ Prédiction avec 3 variables: OK (avec warning)\n")

  # Test 4: 1 seule variable (OK avec warning)
  data_new_single <- data.frame(new1 = rnorm(n))
  pred4 <- suppressWarnings(km$predict(data_new_single))
  stopifnot(nrow(pred4) == 1)
  cat("  ✓ Prédiction avec 1 variable: OK (avec warning)\n")
})

# ==============================================================================
# TEST 5: VISUALISATIONS (CERCLE, PLAN, CENTRES)
# ==============================================================================

run_test("Visualisations - Cercle, Plan factoriel, Centres", function() {
  set.seed(111)
  n <- 60
  data_test <- data.frame(
    var1 = rnorm(n), var2 = rnorm(n), var3 = rnorm(n),
    var4 = rnorm(n), var5 = rnorm(n)
  )

  km <- KMeansVariablesQuant$new(k = 2, n_init = 10, seed = 42)
  km$fit(data_test)

  # Tester que les fonctions s'exécutent sans erreur
  pdf(file = tempfile(fileext = ".pdf"))
  km$plot_correlation_circle()
  km$plot_variable_map()
  km$plot_center_map()
  dev.off()

  cat("  ✓ Cercle de corrélation: OK\n")
  cat("  ✓ Plan factoriel variables: OK\n")
  cat("  ✓ Projection centres: OK\n")
})

# ==============================================================================
# TEST 6: ELBOW AMÉLIORÉ AVEC DÉTECTION AUTO
# ==============================================================================

run_test("Elbow amélioré - Détection automatique k*", function() {
  set.seed(222)
  n <- 80

  # Créer données avec structure claire (k=3)
  X1 <- rnorm(n)
  X2 <- rnorm(n)
  X3 <- rnorm(n)

  data_test <- data.frame(
    var1 = X1 + rnorm(n, 0, 0.2),
    var2 = X1 + rnorm(n, 0, 0.2),
    var3 = X2 + rnorm(n, 0, 0.2),
    var4 = X2 + rnorm(n, 0, 0.2),
    var5 = X3 + rnorm(n, 0, 0.2),
    var6 = X3 + rnorm(n, 0, 0.2)
  )

  km <- KMeansVariablesQuant$new(k = 3, seed = 42)
  km$fit(data_test)

  # Test elbow standalone
  result_standalone <- kmeans_elbow(data_test, k_range = 2:5, n_init = 10, seed = 42)
  stopifnot(!is.null(result_standalone$optimal_k))
  stopifnot(result_standalone$optimal_k >= 2 && result_standalone$optimal_k <= 5)
  cat("  ✓ Fonction standalone: k* =", result_standalone$optimal_k, "\n")

  # Test méthode $elbow()
  result_method <- km$elbow(k_range = 2:5, n_init = 10, plot = FALSE)
  stopifnot(!is.null(result_method$optimal_k))
  cat("  ✓ Méthode $elbow(): k* =", result_method$optimal_k, "\n")
})

# ==============================================================================
# TEST 7: COMPUTE_ELBOW (ANCIENNE MÉTHODE - BACKWARD COMPATIBILITY)
# ==============================================================================

run_test("compute_elbow - Compatibilité ancienne méthode", function() {
  set.seed(333)
  n <- 50
  data_test <- data.frame(
    var1 = rnorm(n), var2 = rnorm(n), var3 = rnorm(n)
  )

  km <- KMeansVariablesQuant$new(k = 2, seed = 42)
  km$fit(data_test)

  # Tester ancienne méthode
  elbow_df <- km$compute_elbow(k_range = 2:3)
  stopifnot(is.data.frame(elbow_df))
  stopifnot(all(c("k", "inertia") %in% colnames(elbow_df)))

  cat("  ✓ compute_elbow() fonctionne\n")
  cat("  ✓ Backward compatibility assurée\n")
})

# ==============================================================================
# TEST 8: PRINT ET SUMMARY
# ==============================================================================

run_test("Méthodes print() et summary()", function() {
  set.seed(444)
  n <- 60
  data_test <- data.frame(
    var1 = rnorm(n), var2 = rnorm(n), var3 = rnorm(n),
    var4 = rnorm(n)
  )

  km <- KMeansVariablesQuant$new(k = 2, seed = 42)
  km$fit(data_test)

  # Test print
  capture.output(km$print())
  cat("  ✓ print() fonctionne\n")

  # Test summary
  results <- km$summary(print_summary = FALSE)
  stopifnot(is.list(results))
  stopifnot("clust_summary" %in% names(results))
  stopifnot("clust_members" %in% names(results))
  stopifnot("cor_latent" %in% names(results))
  stopifnot("inertia_total" %in% names(results))
  stopifnot("r2_matrix" %in% names(results))
  cat("  ✓ summary() retourne les bonnes structures\n")
})


# ==============================================================================
# TEST 9: STABILITÉ - DIFFÉRENTES INITIALISATIONS
# ==============================================================================

run_test("Stabilité - Plusieurs initialisations", function() {
  set.seed(555)
  n <- 80

  # Données avec structure claire
  X1 <- rnorm(n)
  X2 <- rnorm(n)

  data_test <- data.frame(
    var1 = X1 + rnorm(n, 0, 0.2),
    var2 = X1 + rnorm(n, 0, 0.2),
    var3 = X2 + rnorm(n, 0, 0.2),
    var4 = X2 + rnorm(n, 0, 0.2)
  )

  # Plusieurs runs
  inertias <- numeric(5)
  for (i in 1:5) {
    km <- KMeansVariablesQuant$new(k = 2, n_init = 20, seed = i * 100)
    km$fit(data_test)
    inertias[i] <- km$inertia_total
  }

  # Vérifier stabilité (variance faible)
  cv <- sd(inertias) / mean(inertias)
  stopifnot(cv < 0.01)  # Coefficient de variation < 1%

  cat("  - Inertie moyenne:", round(mean(inertias), 4), "\n")
  cat("  - Écart-type:", round(sd(inertias), 6), "\n")
  cat("  - CV:", round(cv * 100, 4), "%\n")
  cat("  ✓ Résultats stables (CV < 1%)\n")
})

# ==============================================================================
# TEST 10: DONNÉES RÉELLES - USARRESTS
# ==============================================================================

run_test("Données réelles - USArrests", function() {
  data(USArrests)

  km <- KMeansVariablesQuant$new(k = 2, n_init = 20, seed = 123)
  km$fit(USArrests)

  # Validations
  stopifnot(length(km$clusters) == 4)
  results <- km$summary(print_summary = FALSE)
  var_expl <- results$clust_summary$variance_explained_pct
  stopifnot(all(var_expl >= 0 & var_expl <= 100))

  cat("  - Variables:", paste(colnames(USArrests), collapse = ", "), "\n")
  cat("  - Inertie totale:", round(km$inertia_total, 4), "\n")
  cat("  - Variance moyenne:", round(mean(var_expl), 2), "%\n")
  cat("  ✓ Traitement des données réelles: OK\n")
})

# ==============================================================================
# TEST 11: GESTION DES ERREURS
# ==============================================================================

run_test("Gestion des erreurs", function() {
  # Erreur 1: k trop grand
  data_test <- data.frame(var1 = rnorm(50), var2 = rnorm(50))
  km <- KMeansVariablesQuant$new(k = 5, seed = 42)

  error_caught <- FALSE
  tryCatch(km$fit(data_test), error = function(e) {
    error_caught <<- TRUE
  })
  stopifnot(error_caught)
  cat("  ✓ Erreur k > ncol détectée\n")

  # Erreur 2: Données non-numériques
  data_bad <- data.frame(var1 = letters[1:10], var2 = 1:10)
  km2 <- KMeansVariablesQuant$new(k = 2)

  error_caught <- FALSE
  tryCatch(km2$fit(data_bad), error = function(e) {
    error_caught <<- TRUE
  })
  stopifnot(error_caught)
  cat("  ✓ Erreur données non-numériques détectée\n")

  # Erreur 3: Predict sans fit
  km3 <- KMeansVariablesQuant$new(k = 2)
  error_caught <- FALSE
  tryCatch(km3$predict(data.frame(x = 1:10)), error = function(e) {
    error_caught <<- TRUE
  })
  stopifnot(error_caught)
  cat("  ✓ Erreur predict() sans fit() détectée\n")
})

# ==============================================================================
# TEST 12: CAS LIMITES
# ==============================================================================

run_test("Cas limites", function() {
  set.seed(666)
  n <- 30

  # Cas 1: 2 variables seulement (minimum)
  data_min <- data.frame(var1 = rnorm(n), var2 = rnorm(n))
  km1 <- KMeansVariablesQuant$new(k = 2, seed = 42)
  km1$fit(data_min)
  stopifnot(!is.null(km1$clusters))
  cat("  ✓ Minimum 2 variables: OK\n")

  # Cas 2: k=2 (minimum)
  data_test <- data.frame(var1 = rnorm(n), var2 = rnorm(n), var3 = rnorm(n))
  km2 <- KMeansVariablesQuant$new(k = 2, seed = 42)
  km2$fit(data_test)
  stopifnot(length(unique(km2$clusters)) == 2)
  cat("  ✓ Minimum k=2: OK\n")

  # Cas 3: k=p-1 (maximum)
  km3 <- KMeansVariablesQuant$new(k = 2, seed = 42)  # k = ncol - 1 = 2
  km3$fit(data_test)
  stopifnot(!is.null(km3$clusters))
  cat("  ✓ Maximum k=p-1: OK\n")
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
  cat("La classe KMeansVariablesQuant est prête pour la production:\n")
  cat("  ✓ Tous les bugs corrigés\n")
  cat("  ✓ Nouvelles fonctionnalités validées\n")
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
