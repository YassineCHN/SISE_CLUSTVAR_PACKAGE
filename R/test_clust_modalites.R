#############################################
# SCRIPT DE TEST COMPLET - ClustModalities
#############################################

cat("\n========== CHARGEMENT DE LA CLASSE ==========\n")

source("C:/Users/ychen/OneDrive/Documents/GitHub/SISE_Clustering_Variables_R/R/acm_cah.R")

library(ade4)
library(xlsx)   # pour vote
# (loisirs est en .txt -> pas besoin de xlsx)

############################################################
# ==========================================================
#   PARTIE 1 — TEST SUR LE DATASET "VOTE"
# ==========================================================
############################################################

cat("\n========== LECTURE DU DATASET VOTE ==========\n")

vote.data <- read.xlsx(
  "C:/Users/ychen/OneDrive/Bureau/M2 SISE/S1/Programmation statistique sous R/datasets_clustering/vote/vote_catvarclus.xls",
  sheetIndex = 1
)

vote.active <- subset(vote.data, select = 2:7)   # actives
vote.illus  <- vote.data$affiliation             # illustrative

str(vote.active)
str(vote.illus)


####vote.illus############################################################
# ------------ TEST 1 : ACM — FIT --------------------------
############################################################

cat("\n========== TEST ACM : FIT ==========\n")

mod_acm <- ClustModalities$new(method = "acm")
mod_acm$fit(vote.active, k = 3)

print(mod_acm)
print(table(mod_acm$mod_clusters))
print(mod_acm$cluster_table())


############################################################
# ------------ TEST 2 : Visualisations ACM -----------------
############################################################

cat("\n========== TEST ACM : VISUALISATIONS ==========\n")

mod_acm$plot_dendrogram()
mod_acm$plot_factor_map()
mod_acm$plot_scree()                 # % inertie
mod_acm$plot_scree(cumulative = TRUE) # inertie cumulée
mod_acm$plot_contrib(dim=1, top=10)
mod_acm$plot_contrib(dim=2, top=10)


############################################################
# ------------ TEST 3 : predict() qualitatif ---------------
############################################################

cat("\n========== TEST ACM : PREDICT (QUALITATIF) ==========\n")

aff <- data.frame(affiliation = vote.illus)
pred_acm <- mod_acm$predict(aff)
print(pred_acm)


############################################################
# ------------ TEST 4 : predict — erreurs ------------------
############################################################

cat("\n========== TEST ACM : ERREURS PREDICT ==========\n")

try(mod_acm$predict(aff[-1, , drop=FALSE]))  # erreur : mauvaise dimension


############################################################
# ------------ TEST 5 : Elbow ACM --------------------------
############################################################

cat("\n========== TEST ACM : ELBOW ==========\n")

mod_acm$plot_elbow()
print(mod_acm$compute_elbow())


############################################################
# ------------ TEST 6 : Robustesse / stabilité -------------
############################################################

cat("\n========== TEST ACM : STABILITÉ ==========\n")

set.seed(1)
idx_vote <- sample(1:nrow(vote.active), 350)

mod_acm2 <- ClustModalities$new(method="acm")
mod_acm2$fit(vote.active[idx_vote,], k=3)

print(table(mod_acm2$mod_clusters))

centers1 <- rowsum(mod_acm$mod_coords, mod_acm$mod_clusters) /
  as.numeric(table(mod_acm$mod_clusters))

centers2 <- rowsum(mod_acm2$mod_coords, mod_acm2$mod_clusters) /
  as.numeric(table(mod_acm2$mod_clusters))

cat("\nDistances entre centres (dataset complet) :\n")
print(dist(centers1))

cat("\nDistances entre centres (échantillon) :\n")
print(dist(centers2))


############################################################
# ==========================================================
#   PARTIE 2 — TEST SUR LE DATASET "LOISIRS"
# ==========================================================
############################################################

cat("\n\n========== LECTURE DU DATASET LOISIRS ==========\n")

loisir.data <- read.table(
  "C:/Users/ychen/OneDrive/Bureau/M2 SISE/S1/Programmation statistique sous R/datasets_clustering/loisir/loisirs.txt",
  header = TRUE, sep = "\t"
)

# Variables actives qualitatives = colonnes 1 à 18
loisir.quali <- subset(loisir.data, select = 1:18)

# Variables illustratives qualitatives
loisir.illus.qual <- subset(loisir.data, select = c(Marital_status, Profession))

# Variable illustrative quantitative
# -> Age est catégorielle → inutile
# -> on utilise : nb_activitees
loisir.quant <- data.frame(nb_activitees = loisir.data$nb_activitees)

str(loisir.quali)


############################################################
# ------------ TEST 7 : ACM LOISIRS ------------------------
############################################################

cat("\n========== TEST ACM LOISIRS : FIT ==========\n")

mod_loisir <- ClustModalities$new(method="acm")
mod_loisir$fit(loisir.quali, k = 3)

print(table(mod_loisir$mod_clusters))
print(mod_loisir$cluster_table())


############################################################
# ------------ TEST 8 : Visualisations LOISIRS --------------
############################################################

mod_loisir$plot_dendrogram()
mod_loisir$plot_factor_map()
mod_loisir$plot_scree()
mod_loisir$plot_scree(cumulative = TRUE)
mod_loisir$plot_contrib(dim=1, top=15)
mod_loisir$plot_contrib(dim=2, top=15)


############################################################
# ------------ TEST 9 : predict qualitatif LOISIRS ----------
############################################################

cat("\n========== TEST PREDICT QUALITATIF LOISIRS ==========\n")

pred_loisir_qual <- mod_loisir$predict(loisir.illus.qual)
print(pred_loisir_qual)


############################################################
# ------------ TEST 10 : projection quantitative ------------
############################################################

cat("\n========== TEST project_numeric (nb_activitees) ==========\n")

mod_loisir$project_numeric(loisir.quant)


############################################################
# ==========================================================
#   PARTIE 3 — TEST DU PIPELINE DICE
# ==========================================================
############################################################

cat("\n\n========== TEST DICE SUR VOTE ==========\n")

mod_dice <- ClustModalities$new(method="dice")
mod_dice$fit(vote.active, k = 3)

print(table(mod_dice$mod_clusters))
mod_dice$plot_dendrogram()

cat("\n========== TEST DICE : PREDICT ==========\n")
pred_dice <- mod_dice$predict(aff)
print(pred_dice)


############################################################
# FIN
############################################################

cat("\n========== FIN DU SCRIPT DE TEST ==========\n")


mod_dice <- ClustModalities$new(method="dice")
mod_dice$fit(vote.active, k=3)

cat("OLD DISJ :", nrow(mod_dice$disj), "x", ncol(mod_dice$disj), "\n")
print(colnames(mod_dice$disj))

disj_new_test <- ade4::acm.disjonctif(data.frame(affiliation = vote.illus))
cat("NEW DISJ :", nrow(disj_new_test), "x", ncol(disj_new_test), "\n")
print(colnames(disj_new_test))

print(names(mod_dice$mod_clusters))

old_disj <- as.matrix(mod_dice$disj)
new_disj <- as.matrix(ade4::acm.disjonctif(data.frame(affiliation = vote.illus)))

vec_test <- new_disj[,1]  # première modalité

d2_test <- apply(old_disj, 2, function(col_old) 0.5 * sum((vec_test - col_old)^2))

print(d2_test)
print(which.min(d2_test))
print(names(d2_test)[which.min(d2_test)])
print(mod_dice$mod_clusters[names(d2_test)[which.min(d2_test)]])

test_out <- lapply(seq_len(ncol(new_disj)), function(j) {
  vec <- new_disj[,j]
  d2 <- apply(old_disj, 2, function(col_old) 0.5 * sum((vec - col_old)^2))
  ix <- which.min(d2)
  closest <- names(d2)[ix]
  c(cluster = mod_dice$mod_clusters[closest], distance = sqrt(d2[ix]))
})

print(test_out)
