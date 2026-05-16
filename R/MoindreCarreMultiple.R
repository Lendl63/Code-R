cat("\n")
cat("   RÉGRESSION LINÉAIRE MULTIPLE\n")
cat("   Calcul manuel par les équations normales (X'X)b = X'Y\n")
cat("\n")

# ----- 1. SAISIE DES DIMENSIONS -----
n <- as.integer(readline(prompt = "Nombre d'observations (n) : "))
p <- as.integer(readline(prompt = "Nombre de variables explicatives (p, sans la constante) : "))

# ----- 2. SAISIE DE LA VARIABLE DÉPENDANTE Y -----
cat("\nEntrez les", n, "valeurs de Y (variable dépendante), séparées par des espaces :\n")
Y <- scan(n = n, what = numeric(), quiet = TRUE)

# ----- 3. SAISIE DES VARIABLES EXPLICATIVES Xj -----
X <- matrix(NA, nrow = n, ncol = p)
colnames(X) <- paste0("X", 1:p)

for (j in 1:p) {
  cat(sprintf("Entrez les %d valeurs de X%d :\n", n, j))
  X[, j] <- scan(n = n, what = numeric(), quiet = TRUE)
}

# ----- 4. AJOUT DE LA CONSTANTE (INTERCEPT) -----
# Matrice de design : [1 X]
X_design <- cbind(Intercept = rep(1, n), X)

# ----- 5. RÉSOLUTION DES ÉQUATIONS NORMALES -----
# Coefficients estimés b = (X'X)^{-1} X'Y
# On utilise solve() pour l'inverse ; en cas de singularité, on avertit.
XX <- t(X_design) %*% X_design
XY <- t(X_design) %*% Y

# Essayer de calculer l'inverse ; si échec (multicollinéarité parfaite), arrêter.
inv_XX <- tryCatch(solve(XX), error = function(e) NULL)
if (is.null(inv_XX)) {
  stop("La matrice X'X est singulière (multicollinéarité parfaite). Le modèle ne peut être estimé.")
}
b <- inv_XX %*% XY

# ----- 6. VALEURS PRÉDITES, RÉSIDUS ET SOMMES DES CARRÉS -----
Y_pred <- X_design %*% b
residus <- Y - Y_pred

# Somme des carrés résiduelle (SSE)
SSE <- sum(residus^2)

# Somme des carrés totale (SST)
SST <- sum((Y - mean(Y))^2)

# Somme des carrés due à la régression (SSR)
SSR <- SST - SSE

# Degrés de liberté
df_reg <- p           # p variables explicatives (hors constante)
df_res <- n - p - 1   # n - (p+1)
df_tot <- n - 1

# Carrés moyens
MSR <- SSR / df_reg
MSE <- SSE / df_res

# F global
F_global <- MSR / MSE

# p-valeur du test F global
p_value_F <- pf(F_global, df_reg, df_res, lower.tail = FALSE)

# ----- 7. COEFFICIENT DE DÉTERMINATION -----
R2 <- SSR / SST
R2_adj <- 1 - (MSE / (SST / df_tot))

# ----- 8. AFFICHAGE DES RÉSULTATS -----
cat("\n===========================================================\n")
cat("              RÉSULTATS DE LA RÉGRESSION MULTIPLE\n")
cat("===========================================================\n")
cat("Coefficients estimés :\n")
for (j in 1:(p+1)) {
  cat(sprintf("   %-10s : %10.4f\n", colnames(X_design)[j], b[j]))
}

cat("\nAnalyse de la variance :\n")
cat(sprintf("   %-20s %8s %4s %10s %10s\n", "Source", "SS", "df", "MS", "F"))
cat(sprintf("   %-20s %8.3f %4d %10.3f %10.3f\n", "Régression", SSR, df_reg, MSR, F_global))
cat(sprintf("   %-20s %8.3f %4d %10.3f\n", "Résidus", SSE, df_res, MSE))
cat(sprintf("   %-20s %8.3f %4d\n", "Total", SST, df_tot))

cat(sprintf("\nR²     = %.4f\n", R2))
cat(sprintf("R² ajusté = %.4f\n", R2_adj))
cat(sprintf("F global = %.4f, p-value = %.4f\n", F_global, p_value_F))

# ----- 9. VALIDATION AVEC lm() (optionnelle) -----
cat("\n--- Vérification avec la fonction lm() de R ---\n")
# Convertir en data frame pour lm()
donnees <- as.data.frame(cbind(Y, X))
colnames(donnees) <- c("Y", paste0("X", 1:p))
formule <- as.formula(paste("Y ~", paste(colnames(donnees)[-1], collapse = " + ")))
modele_lm <- lm(formule, data = donnees)
print(summary(modele_lm))