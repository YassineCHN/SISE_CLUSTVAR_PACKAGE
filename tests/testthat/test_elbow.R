# ==============================================================================
# TESTS UNITAIRES POUR LES MÉTHODES ELBOW
# ==============================================================================

library(testthat)

# ==============================================================================
# TESTS POUR KMEANS_ELBOW
# ==============================================================================

test_that("kmeans_elbow returns valid results", {
  # Create test data
  set.seed(123)
  n <- 100
  X1 <- rnorm(n)
  var1 <- X1 + rnorm(n, 0, 0.3)
  var2 <- X1 + rnorm(n, 0, 0.3)
  var3 <- X1 + rnorm(n, 0, 0.3)

  X2 <- rnorm(n)
  var4 <- X2 + rnorm(n, 0, 0.3)
  var5 <- X2 + rnorm(n, 0, 0.3)

  data_test <- data.frame(var1, var2, var3, var4, var5)

  # Run elbow
  result <- kmeans_elbow(data_test, k_range = 2:5, n_init = 10, seed = 42)

  # Tests
  expect_type(result, "list")
  expect_true("optimal_k" %in% names(result))
  expect_true("results" %in% names(result))
  expect_true("plot" %in% names(result))

  expect_true(result$optimal_k >= 2 && result$optimal_k <= 5)
  expect_equal(nrow(result$results), 4)  # 4 values in k_range
  expect_true(all(c("k", "inertia", "gain") %in% colnames(result$results)))

  # Inertia should be increasing
  expect_true(all(diff(result$results$inertia) > 0))
})

test_that("kmeans_elbow validates input", {
  # Non-numeric data
  expect_error(
    kmeans_elbow(data.frame(x = letters[1:10])),
    "numeric"
  )

  # Less than 2 variables
  expect_error(
    kmeans_elbow(data.frame(x = 1:10)),
    "at least 2"
  )
})

test_that("kmeans_elbow plot function works", {
  set.seed(123)
  data_test <- data.frame(
    var1 = rnorm(50),
    var2 = rnorm(50),
    var3 = rnorm(50)
  )

  result <- kmeans_elbow(data_test, k_range = 2:4, n_init = 5)

  # Test that plot function doesn't error
  expect_silent(result$plot())
})

# ==============================================================================
# TESTS POUR ACM_CAH_ELBOW
# ==============================================================================

test_that("acm_cah_elbow returns valid results", {
  # Create test data
  set.seed(123)
  n <- 50
  data_quali <- data.frame(
    var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
    var3 = factor(sample(c("1", "2", "3", "4"), n, replace = TRUE))
  )

  # Run elbow
  result <- acm_cah_elbow(data_quali, method = "acm", k_max = 5)

  # Tests
  expect_type(result, "list")
  expect_true("optimal_k" %in% names(result))
  expect_true("results" %in% names(result))
  expect_true("plot" %in% names(result))

  expect_true(result$optimal_k >= 2 && result$optimal_k <= 5)
  expect_equal(nrow(result$results), 5)
  expect_true(all(c("k", "height", "gain") %in% colnames(result$results)))

  # Heights should be decreasing
  expect_true(all(diff(result$results$height) <= 0))
})

test_that("acm_cah_elbow validates input", {
  # Non-dataframe
  expect_error(
    acm_cah_elbow(matrix(1:10, ncol = 2)),
    "data.frame"
  )

  # Empty dataframe
  expect_error(
    acm_cah_elbow(data.frame()),
    "at least 1"
  )
})

test_that("acm_cah_elbow plot function works", {
  set.seed(123)
  data_quali <- data.frame(
    var1 = factor(sample(c("A", "B"), 30, replace = TRUE)),
    var2 = factor(sample(c("X", "Y", "Z"), 30, replace = TRUE))
  )

  result <- acm_cah_elbow(data_quali, k_max = 4)

  # Test that plot function doesn't error
  expect_silent(result$plot())
})

# ==============================================================================
# TESTS POUR LA MÉTHODE $elbow() DE KMeansVariablesQuant
# ==============================================================================

test_that("KMeansVariablesQuant$elbow() works", {
  # Create test data
  set.seed(123)
  n <- 100
  data_test <- data.frame(
    var1 = rnorm(n),
    var2 = rnorm(n),
    var3 = rnorm(n),
    var4 = rnorm(n)
  )

  # Create instance
  km <- KMeansVariablesQuant$new(k = 3, seed = 42)
  km$fit(data_test)

  # Run elbow (without plotting to avoid test issues)
  result <- km$elbow(k_range = 2:5, plot = FALSE)

  # Tests
  expect_type(result, "list")
  expect_true(result$optimal_k >= 2 && result$optimal_k <= 5)
  expect_equal(nrow(result$results), 4)
})

test_that("KMeansVariablesQuant$elbow() requires data", {
  km <- KMeansVariablesQuant$new(k = 3)

  expect_error(
    km$elbow(),
    "No data"
  )
})

# ==============================================================================
# TESTS POUR LA MÉTHODE $elbow() DE ClustModalities
# ==============================================================================

test_that("ClustModalities$elbow() works", {
  # Create test data
  set.seed(123)
  n <- 50
  data_quali <- data.frame(
    var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE))
  )

  # Create instance
  clust <- ClustModalities$new(method = "acm")
  clust$fit(data_quali)

  # Run elbow (without plotting)
  result <- clust$elbow(k_max = 5, plot = FALSE)

  # Tests
  expect_type(result, "list")
  expect_true(result$optimal_k >= 2 && result$optimal_k <= 5)
  expect_equal(nrow(result$results), 5)
})

test_that("ClustModalities$elbow() requires data", {
  clust <- ClustModalities$new(method = "acm")

  expect_error(
    clust$elbow(),
    "No data"
  )
})

# ==============================================================================
# TEST DE LA FONCTION HELPER detect_elbow
# ==============================================================================

test_that("detect_elbow finds maximum perpendicular distance", {
  # Simple test case with clear elbow at k=3
  k_vals <- 2:6
  y_vals <- c(10, 7, 5, 4.5, 4.2)  # Sharp drop at k=3

  optimal_k <- detect_elbow(k_vals, y_vals)

  expect_equal(optimal_k, 3)
})
