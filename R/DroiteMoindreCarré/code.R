# =============================================================================
#  RÉGRESSION LINÉAIRE SIMPLE (MOINDRES CARRÉS)
#  Calcul manuel selon le support - Chapitre 5, pages 37-40
# =============================================================================

cat("\n===========================================================\n")
cat("   RÉGRESSION LINÉAIRE SIMPLE - MOINDRES CARRÉS\n")
cat("   Calcul manuel de a, b, R², etc.\n")
cat("===========================================================\n\n")

# ----- 1. SAISIE DES DONNÉES -----
n <- as.integer(readline(prompt = "Nombre d'observations (paires X,Y) : "))

cat("\nEntrez les valeurs de X (variable explicative) :\n")
X <- scan(n = n, what = numeric(), quiet = TRUE)

cat("Entrez les valeurs de Y (variable réponse) :\n")
Y <- scan(n = n, what = numeric(), quiet = TRUE)

# ----- 2. MOYENNES -----
x_bar <- mean(X)
y_bar <- mean(Y)

# ----- 3. CALCUL DU COEFFICIENT DE CORRÉLATION r -----
num_r <- sum((X - x_bar) * (Y - y_bar))
den_r <- sqrt(sum((X - x_bar)^2) * sum((Y - y_bar)^2))
r <- num_r / den_r

# ----- 4. CALCUL DE LA PENTE b ET DE L'ORDONNÉE À L'ORIGINE a -----
# Pente (méthode 1 : via r)
s_x <- sd(X)   # écart-type échantillon (n-1)
s_y <- sd(Y)
b <- r * (s_y / s_x)

# Pente (méthode 2 : formule directe des moindres carrés) - donne la même chose
b_direct <- sum((X - x_bar) * (Y - y_bar)) / sum((X - x_bar)^2)

# Ordonnée à l'origine
a <- y_bar - b * x_bar

# ----- 5. PRÉDICTIONS ET RÉSIDUS -----
Y_pred <- a + b * X
residus <- Y - Y_pred

# ----- 6. SOMMES DES CARRÉS POUR R² -----
SS_res <- sum(residus^2)            # Somme des carrés résiduelle
SS_tot <- sum((Y - y_bar)^2)        # Somme totale
R2 <- 1 - (SS_res / SS_tot)         # Coefficient de détermination (identique à r²)

# ----- 7. AFFICHAGE DES RÉSULTATS -----
cat("\n===========================================================\n")
cat("              RÉSULTATS DE LA RÉGRESSION\n")
cat("===========================================================\n")
cat(sprintf("Moyenne de X : %.4f\n", x_bar))
cat(sprintf("Moyenne de Y : %.4f\n", y_bar))
cat(sprintf("Coefficient de corrélation r : %.4f\n", r))
cat(sprintf("Coefficient de détermination R² : %.4f\n", R2))
cat(sprintf("Pente b : %.4f\n", b))
cat(sprintf("Ordonnée à l'origine a : %.4f\n", a))
cat("\nDroite de régression estimée :\n")
cat(sprintf("   Y_chapeau = %.4f + %.4f * X\n", a, b))

# ----- 8. VALIDATION AVEC lm() (optionnelle) -----
cat("\n--- Vérification avec la fonction lm() de R ---\n")
modele_lm <- lm(Y ~ X)
print(summary(modele_lm))