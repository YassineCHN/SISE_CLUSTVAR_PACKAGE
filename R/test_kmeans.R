# =============================================================================
# SCRIPT DE TEST - K-means FINAL (VERSION CORRIGÉE)
#
# Ce script teste les corrections de bugs et les nouvelles fonctionnalités
# =============================================================================

library(R6)
library(xlsx)  # Pour lire les fichiers Excel

# IMPORTANT: Charger la version FINALE corrigée
source("C:/Users/ychen/OneDrive/Documents/GitHub/SISE_Clustering_Variables_R/R/kmeans.R")

cat("\n")
cat("=============================================================================\n")
cat("         TESTS DE VALIDATION DES CORRECTIONS DE BUGS                       \n")
cat("=============================================================================\n\n")

cat("Ce script teste les 6 bugs corrigés:\n")
cat("1. Formule lambda correcte (inertie croissante)\n")
cat("2. Variance expliquée < 100%\n")
cat("3. Fonction predict() corrigée\n")
cat("4. Vrai cercle de corrélation\n")
cat("5. Échelles graphiques ajustées\n")
cat("6. Stabilité courbe elbow\n\n")

cat("\n")
cat("=============================================================================\n")
cat("               TEST 1: Données simulées simples (3 groupes clairs)         \n")
cat("=============================================================================\n\n")

# Créer des données avec 3 groupes de variables corrélées
set.seed(123)
n <- 100

# Groupe 1: variables corrélées positivement
X1 <- rnorm(n)
var1 <- X1 + rnorm(n, 0, 0.3)
var2 <- X1 + rnorm(n, 0, 0.3)
var3 <- X1 + rnorm(n, 0, 0.3)

# Groupe 2: variables corrélées positivement (indépendantes du groupe 1)
X2 <- rnorm(n)
var4 <- X2 + rnorm(n, 0, 0.3)
var5 <- X2 + rnorm(n, 0, 0.3)

# Groupe 3: variables corrélées positivement (indépendantes des groupes 1 et 2)
X3 <- rnorm(n)
var6 <- X3 + rnorm(n, 0, 0.3)
var7 <- X3 + rnorm(n, 0, 0.3)
var8 <- X3 + rnorm(n, 0, 0.3)

# Combiner dans un data.frame
data_test <- data.frame(
  var1, var2, var3,  # Groupe 1
  var4, var5,        # Groupe 2
  var6, var7, var8   # Groupe 3
)

cat("Structure attendue:\n")
cat("- Groupe 1: var1, var2, var3 (3 variables)\n")
cat("- Groupe 2: var4, var5 (2 variables)\n")
cat("- Groupe 3: var6, var7, var8 (3 variables)\n\n")

# Créer et ajuster le modèle K-means
kmeans_model <- KMeansVariablesQuant$new(k = 3, n_init = 20, seed = 42)
kmeans_model$fit(data_test)

cat("\n")
kmeans_model$print()

# Récupérer le summary
results <- kmeans_model$summary(print_summary = TRUE)

cat("\n")
cat("=============================================================================\n")
cat("     VALIDATION BUG #1 et #2: Inertie croissante et Variance < 100%        \n")
cat("=============================================================================\n\n")

# TEST: Vérifier que la variance expliquée est < 100%
var_expl <- results$clust_summary$variance_explained_pct

cat("Variance expliquée par cluster:\n")
print(results$clust_summary[, c("cluster", "variance_explained_pct")])
cat("\n")

if (all(var_expl >= 0 & var_expl <= 100)) {
  cat("✓ BUG #2 CORRIGÉ: Toutes les variances sont entre 0 et 100%\n")
} else {
  cat("✗ PROBLÈME: Certaines variances sont > 100%\n")
  print(var_expl[var_expl > 100])
}

cat("\n")
cat("=============================================================================\n")
cat("     VALIDATION BUG #1: Courbe elbow (inertie doit AUGMENTER)              \n")
cat("=============================================================================\n\n")

# Tester la courbe elbow
elbow_df <- kmeans_model$compute_elbow(k_range = 2:6)
cat("Inertie par nombre de clusters:\n")
print(elbow_df)
cat("\n")

# TEST: Vérifier que l'inertie augmente avec k
diff_inertia <- diff(elbow_df$inertia)

cat("Différences d'inertie (k+1 - k):\n")
for (i in seq_along(diff_inertia)) {
  cat(sprintf("  k=%d -> k=%d : %+.4f\n",
              elbow_df$k[i], elbow_df$k[i+1], diff_inertia[i]))
}
cat("\n")

if (all(diff_inertia > 0)) {
  cat("✓ BUG #1 CORRIGÉ: L'inertie augmente bien avec k\n")
} else {
  cat("✗ PROBLÈME: L'inertie ne croît pas monotonement\n")
  cat("Indices où ça descend:", which(diff_inertia <= 0), "\n")
}

cat("\n>> Visualisation de la courbe elbow\n")
kmeans_model$plot_elbow(k_range = 2:6)

cat("\n")
cat("=============================================================================\n")
cat("     VALIDATION BUG #3: Fonction predict() corrigée                        \n")
cat("=============================================================================\n\n")

# Test 1: Predict avec le bon nombre de variables (doit marcher)
cat("Test 3.1: Prédiction avec le bon nombre de variables\n")
set.seed(456)
var_new1 <- X1 + rnorm(n, 0, 0.4)
var_new2 <- X2 + rnorm(n, 0, 0.4)

data_new_ok <- data.frame(
  var_new1, var_new2,
  rnorm(n), rnorm(n), rnorm(n), rnorm(n), rnorm(n), rnorm(n)
)
names(data_new_ok) <- paste0("new_var", 1:8)

tryCatch({
  predictions <- kmeans_model$predict(data_new_ok)
  cat("✓ Prédiction réussie avec 8 variables\n")
  cat("\nRésultats:\n")
  print(head(predictions, 5))
}, error = function(e) {
  cat("✗ ERREUR:", e$message, "\n")
})

cat("\n")

# Test 2: Predict avec mauvais nombre de variables (doit échouer proprement)
cat("Test 3.2: Prédiction avec un mauvais nombre de variables (doit échouer)\n")
data_new_bad <- data.frame(var_new1, var_new2)

tryCatch({
  predictions_bad <- kmeans_model$predict(data_new_bad)
  cat("✗ PROBLÈME: La prédiction aurait dû échouer\n")
}, error = function(e) {
  cat("✓ BUG #3 CORRIGÉ: Erreur capturée correctement\n")
  cat("   Message:", e$message, "\n")
})

cat("\n")
cat("=============================================================================\n")
cat("     VALIDATION BUG #4: Cercle de corrélation (variables, pas centres)     \n")
cat("=============================================================================\n\n")

cat("Génération du cercle de corrélation...\n")
cat("Vérifiez visuellement que:\n")
cat("  - Un cercle unité est visible\n")
cat("  - Les VARIABLES sont affichées (pas les centres)\n")
cat("  - Des flèches partent de l'origine vers les variables\n")
cat("  - Les variables sont colorées par cluster\n\n")

kmeans_model$plot_correlation_circle()

cat("\n")
cat("=============================================================================\n")
cat("     VALIDATION BUG #5: Échelles ajustées (tous les points visibles)       \n")
cat("=============================================================================\n\n")

cat("Génération du plan factoriel...\n")
cat("Vérifiez visuellement que:\n")
cat("  - Tous les points sont visibles (pas coupés)\n")
cat("  - Les marges sont suffisantes\n")
cat("  - Les labels ne se chevauchent pas\n\n")

kmeans_model$plot_variable_map()

cat("\n")
cat("Génération de la projection des centres...\n")
kmeans_model$plot_center_map()

cat("\n")
cat("=============================================================================\n")
cat("     VISUALISATIONS SÉPARÉES (comme demandé)                               \n")
cat("=============================================================================\n\n")

cat("Les 3 graphiques peuvent maintenant être appelés séparément:\n")
cat("  1. kmeans_model$plot_correlation_circle() - Cercle de corrélation\n")
cat("  2. kmeans_model$plot_variable_map()       - Plan factoriel variables\n")
cat("  3. kmeans_model$plot_center_map()         - Projection des centres\n\n")

cat("\n")
cat("=============================================================================\n")
cat("               TEST 2: Données réelles - USArrests                          \n")
cat("=============================================================================\n\n")

data(USArrests)
cat("Dataset USArrests:\n")
cat(sprintf("- %d états (individus)\n", nrow(USArrests)))
cat(sprintf("- %d variables: %s\n", ncol(USArrests), paste(colnames(USArrests), collapse = ", ")))
cat("\n")

# K-means sur ces données
kmeans_arrests <- KMeansVariablesQuant$new(k = 2, n_init = 20, seed = 123)
kmeans_arrests$fit(USArrests)

cat("\n")
kmeans_arrests$print()

results_arrests <- kmeans_arrests$summary(print_summary = TRUE)

# Vérifier variance < 100%
var_expl_arr <- results_arrests$clust_summary$variance_explained_pct
if (all(var_expl_arr >= 0 & var_expl_arr <= 100)) {
  cat("\n✓ Variance expliquée OK pour USArrests\n")
}

cat("\n>> Visualisations pour USArrests\n")
cat("Cercle de corrélation:\n")
kmeans_arrests$plot_correlation_circle()

cat("\nPlan factoriel:\n")
kmeans_arrests$plot_variable_map()

cat("\n")
cat("=============================================================================\n")
cat("               TEST 3: Dataset CRIMES (données du prof)                    \n")
cat("=============================================================================\n\n")

tryCatch({
  cat("========== CRIMES : Chargement ==========\n")

  crime_data <- read.xlsx(
    "C:/Users/ychen/Downloads/crime_dataset_from_DASL.xls",
    sheetIndex = 1
  )

  crime_quanti <- crime_data[sapply(crime_data, is.numeric)]

  cat(sprintf("Données: %d individus, %d variables\n",
              nrow(crime_quanti), ncol(crime_quanti)))
  cat("Variables:", paste(colnames(crime_quanti), collapse = ", "), "\n\n")

  cat("========== CRIMES : K-means avec k=3 ==========\n")
  km_crime <- KMeansVariablesQuant$new(k = 3, n_init = 20, seed = 123)
  km_crime$fit(crime_quanti)

  cat("\n")
  results_crime <- km_crime$summary(print_summary = TRUE)

  # VALIDATION: Vérifier variance < 100%
  var_crime <- results_crime$clust_summary$variance_explained_pct
  cat("\n--- VALIDATION BUGS ---\n")
  cat("Variance expliquée:\n")
  print(results_crime$clust_summary[, c("cluster", "variance_explained_pct")])

  if (all(var_crime >= 0 & var_crime <= 100)) {
    cat("\n✓ Variance expliquée OK pour CRIMES (toutes < 100%)\n")
  } else {
    cat("\n✗ PROBLÈME: Certaines variances > 100%\n")
  }

  # Courbe elbow
  cat("\n========== CRIMES : Courbe elbow ==========\n")
  elbow_crime <- km_crime$compute_elbow(k_range = 2:min(6, ncol(crime_quanti)))
  print(elbow_crime)

  # Vérifier croissance
  diff_crime <- diff(elbow_crime$inertia)
  if (all(diff_crime > 0)) {
    cat("✓ Inertie croissante pour CRIMES\n")
  } else {
    cat("✗ PROBLÈME: Inertie non croissante\n")
  }

  km_crime$plot_elbow(k_range = 2:min(6, ncol(crime_quanti)))

  # Visualisations séparées
  cat("\n========== CRIMES : Visualisations ==========\n")
  cat("\n1. Cercle de corrélation:\n")
  km_crime$plot_correlation_circle()

  cat("\n2. Plan factoriel:\n")
  km_crime$plot_variable_map()

  cat("\n3. Projection des centres:\n")
  km_crime$plot_center_map()

  cat("\n✓ TEST CRIMES REUSSI\n")

}, error = function(e) {
  cat("✗ Erreur avec le dataset CRIMES:", e$message, "\n")
  cat("Vérifiez que le fichier existe à l'emplacement spécifié\n")
})

cat("\n")
cat("=============================================================================\n")
cat("               TEST 4: Dataset AUTOS2005 (données du prof)                 \n")
cat("=============================================================================\n\n")

tryCatch({
  cat("========== AUTOS2005 : Chargement ==========\n")

  X1_auto <- read.table(
    "C:/Users/ychen/Downloads/AUTOS2005.txt",
    header = TRUE, sep = "\t", fill = TRUE
  )

  cat("Structure des données brutes:\n")
  str(X1_auto)

  cat("\n========== AUTOS2005 : SÉLECTION QUANTI ==========\n")
  autos_quanti <- X1_auto[, 2:9]

  cat(sprintf("Données: %d individus, %d variables\n",
              nrow(autos_quanti), ncol(autos_quanti)))

  # Supprimer NA si nécessaire
  if (any(is.na(autos_quanti))) {
    cat("⚠️  Suppression des lignes avec NA\n")
    autos_quanti <- na.omit(autos_quanti)
    cat(sprintf("Après suppression: %d individus\n", nrow(autos_quanti)))
  }

  cat("\n========== AUTOS2005 : K-means avec k=3 ==========\n")
  km_autos <- KMeansVariablesQuant$new(k = 3, n_init = 20, seed = 123)
  km_autos$fit(autos_quanti)

  cat("\n")
  results_autos <- km_autos$summary(print_summary = TRUE)

  # VALIDATION: Vérifier variance < 100%
  var_autos <- results_autos$clust_summary$variance_explained_pct
  cat("\n--- VALIDATION BUGS ---\n")
  cat("Variance expliquée:\n")
  print(results_autos$clust_summary[, c("cluster", "variance_explained_pct")])

  if (all(var_autos >= 0 & var_autos <= 100)) {
    cat("\n✓ Variance expliquée OK pour AUTOS2005 (toutes < 100%)\n")
  } else {
    cat("\n✗ PROBLÈME: Certaines variances > 100%\n")
  }

  # Courbe elbow
  cat("\n========== AUTOS2005 : Courbe elbow ==========\n")
  elbow_autos <- km_autos$compute_elbow(k_range = 2:min(6, ncol(autos_quanti)))
  print(elbow_autos)

  # Vérifier croissance
  diff_autos <- diff(elbow_autos$inertia)
  if (all(diff_autos > 0)) {
    cat("✓ Inertie croissante pour AUTOS2005\n")
  } else {
    cat("✗ PROBLÈME: Inertie non croissante\n")
  }

  km_autos$plot_elbow(k_range = 2:min(6, ncol(autos_quanti)))

  # Visualisations séparées
  cat("\n========== AUTOS2005 : Visualisations ==========\n")
  cat("\n1. Cercle de corrélation:\n")
  km_autos$plot_correlation_circle()

  cat("\n2. Plan factoriel:\n")
  km_autos$plot_variable_map()

  cat("\n3. Projection des centres:\n")
  km_autos$plot_center_map()

  cat("\n✓ TEST AUTOS2005 REUSSI\n")

}, error = function(e) {
  cat("✗ Erreur avec le dataset AUTOS2005:", e$message, "\n")
  cat("Vérifiez que le fichier existe à l'emplacement spécifié\n")
})


cat("=============================================================================\n")
cat("            TESTS TERMINÉS - Vérifier les graphiques générés               \n")
cat("=============================================================================\n\n")

