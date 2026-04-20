# =============================================================================
#  ANOVA À DEUX FACTEURS AVEC RÉPÉTITION
#  SAISIE COLONNE PAR COLONNE (MACHINE PAR MACHINE)
# =============================================================================

cat("\n===========================================================\n")
cat("   ANOVA À DEUX FACTEURS AVEC RÉPÉTITION\n")
cat("   Saisie colonne par colonne (Machine par Machine)\n")
cat("===========================================================\n\n")

# -----------------------------------------------------------------------------
# 1. SAISIE DES PARAMÈTRES
# -----------------------------------------------------------------------------
r <- as.integer(readline(prompt = "Nombre de niveaux du facteur LIGNE (ex: Opérateurs) : "))
c <- as.integer(readline(prompt = "Nombre de niveaux du facteur COLONNE (ex: Machines) : "))
n <- as.integer(readline(prompt = "Nombre de RÉPÉTITIONS par cellule : "))

N <- r * c * n
cat("\nLe nombre total d'observations sera :", N, "\n\n")

# -----------------------------------------------------------------------------
# 2. SAISIE DES DONNÉES COLONNE PAR COLONNE
# -----------------------------------------------------------------------------
# Initialisation du vecteur des observations dans l'ordre COLONNE par COLONNE
# Pour chaque colonne j, on parcourt les lignes i et pour chaque cellule on saisit n valeurs
observations <- numeric(N)
idx <- 1

for (j in 1:c) {
  cat("\n========================================\n")
  cat("   COLONNE (Machine)", j, "\n")
  cat("========================================\n")
  for (i in 1:r) {
    prompt_cell <- paste0("Ligne (Opérateur) ", i, " - Entrez les ", n, " répétitions séparées par des espaces : ")
    vals <- scan(text = readline(prompt = prompt_cell), what = numeric(), quiet = TRUE)
    if (length(vals) != n) {
      stop("Erreur : Vous devez entrer exactement ", n, " valeurs.")
    }
    observations[idx:(idx + n - 1)] <- vals
    idx <- idx + n
  }
}

# Construction du data.frame (ordre de saisie : colonne > ligne > répétition)
# Nous générons les étiquettes dans le même ordre pour faciliter les calculs
colonnes <- rep(1:c, each = r * n)                # c * r * n
lignes   <- rep(rep(1:r, each = n), times = c)    # pour chaque colonne, r lignes avec n rép
rep_id   <- rep(1:n, times = r * c)

df <- data.frame(
  Colonne = factor(colonnes, labels = paste0("M", 1:c)),
  Ligne   = factor(lignes, labels = paste0("Op", 1:r)),
  Repetition = rep_id,
  Valeur = observations
)

# Affichage du tableau récapitulatif des données saisies
cat("\n\n--- Données saisies (aperçu) ---\n")
print(df, max = 20)

# -----------------------------------------------------------------------------
# 3. CALCUL DES TOTAUX (conformément au support)
# -----------------------------------------------------------------------------
T_total <- sum(observations)

# Totaux par ligne T_i.. (Opérateurs)
T_i <- tapply(df$Valeur, df$Ligne, sum)

# Totaux par colonne T_.j. (Machines)
T_j <- tapply(df$Valeur, df$Colonne, sum)

# Totaux par cellule T_ij.
cell_totals <- tapply(df$Valeur, list(df$Ligne, df$Colonne), sum)

# -----------------------------------------------------------------------------
# 4. CALCUL DES SOMMES DES CARRÉS (FORMULES PAGE 31)
# -----------------------------------------------------------------------------
CF <- T_total^2 / N

SST <- sum(observations^2) - CF

SSR <- sum(T_i^2) / (c * n) - CF

SSC <- sum(T_j^2) / (r * n) - CF

term1 <- sum(cell_totals^2) / n
term2 <- sum(T_i^2) / (c * n)
term3 <- sum(T_j^2) / (r * n)
SS_RC <- term1 - term2 - term3 + CF

SSE <- SST - SSR - SSC - SS_RC

# -----------------------------------------------------------------------------
# 5. DEGRÉS DE LIBERTÉ ET CARRÉS MOYENS
# -----------------------------------------------------------------------------
df_SSR   <- r - 1
df_SSC   <- c - 1
df_SS_RC <- (r - 1) * (c - 1)
df_SSE   <- r * c * (n - 1)
df_SST   <- N - 1

MSR   <- SSR / df_SSR
MSC   <- SSC / df_SSC
MS_RC <- SS_RC / df_SS_RC
MSE   <- SSE / df_SSE

# -----------------------------------------------------------------------------
# 6. VALEURS DE F OBSERVÉES
# -----------------------------------------------------------------------------
F_ligne       <- MSR / MSE
F_colonne     <- MSC / MSE
F_interaction <- MS_RC / MSE

# -----------------------------------------------------------------------------
# 7. AFFICHAGE DU TABLEAU ANOVA
# -----------------------------------------------------------------------------
cat("\n\n")
cat("===========================================================\n")
cat("              TABLEAU D'ANALYSE DE VARIANCE\n")
cat("===========================================================\n")
cat(sprintf("%-20s %8s %4s %10s %10s\n", "Source", "SS", "df", "MS", "F"))
cat("-----------------------------------------------------------\n")
cat(sprintf("%-20s %8.3f %4d %10.3f %10.3f\n", "Lignes (Opérateur)", SSR, df_SSR, MSR, F_ligne))
cat(sprintf("%-20s %8.3f %4d %10.3f %10.3f\n", "Colonnes (Machine)", SSC, df_SSC, MSC, F_colonne))
cat(sprintf("%-20s %8.3f %4d %10.3f %10.3f\n", "Interaction", SS_RC, df_SS_RC, MS_RC, F_interaction))
cat(sprintf("%-20s %8.3f %4d %10.3f\n", "Erreur", SSE, df_SSE, MSE))
cat("-----------------------------------------------------------\n")
cat(sprintf("%-20s %8.3f %4d\n", "Total", SST, df_SST))
cat("===========================================================\n")

# -----------------------------------------------------------------------------
# 8. CALCUL DES P-VALEURS ET DÉCISION
# -----------------------------------------------------------------------------
alpha <- 0.05
cat("\nNiveau de signification alpha utilisé :", alpha, "\n")

p_ligne       <- pf(F_ligne, df_SSR, df_SSE, lower.tail = FALSE)
p_colonne     <- pf(F_colonne, df_SSC, df_SSE, lower.tail = FALSE)
p_interaction <- pf(F_interaction, df_SS_RC, df_SSE, lower.tail = FALSE)

cat("\n--- P-valeurs ---\n")
cat(sprintf("Lignes (Opérateur) : p = %.4f\n", p_ligne))
cat(sprintf("Colonnes (Machine) : p = %.4f\n", p_colonne))
cat(sprintf("Interaction        : p = %.4f\n", p_interaction))

cat("\n--- Décisions (H0 rejetée si p < alpha) ---\n")
if (p_ligne < alpha) {
  cat("  -> Effet LIGNE significatif (rejet de H0).\n")
} else {
  cat("  -> Effet LIGNE non significatif (acceptation de H0).\n")
}
if (p_colonne < alpha) {
  cat("  -> Effet COLONNE significatif (rejet de H0).\n")
} else {
  cat("  -> Effet COLONNE non significatif (acceptation de H0).\n")
}
if (p_interaction < alpha) {
  cat("  -> Interaction significative (rejet de H0).\n")
} else {
  cat("  -> Interaction non significative (acceptation de H0).\n")
}

# -----------------------------------------------------------------------------
# 9. SAUVEGARDE OPTIONNELLE DES RÉSULTATS
# -----------------------------------------------------------------------------
save_choice <- readline(prompt = "\nVoulez-vous sauvegarder les résultats dans un fichier texte ? (o/n) : ")
if (tolower(save_choice) == "o") {
  filename <- readline(prompt = "Nom du fichier (ex: resultats_anova.txt) : ")
  sink(filename)
  cat("Résultats de l'ANOVA à deux facteurs avec répétition\n")
  cat("Données brutes :\n")
  print(df)
  cat("\nTableau ANOVA :\n")
  cat(sprintf("%-20s %8s %4s %10s %10s\n", "Source", "SS", "df", "MS", "F"))
  cat(sprintf("%-20s %8.3f %4d %10.3f %10.3f\n", "Lignes", SSR, df_SSR, MSR, F_ligne))
  cat(sprintf("%-20s %8.3f %4d %10.3f %10.3f\n", "Colonnes", SSC, df_SSC, MSC, F_colonne))
  cat(sprintf("%-20s %8.3f %4d %10.3f %10.3f\n", "Interaction", SS_RC, df_SS_RC, MS_RC, F_interaction))
  cat(sprintf("%-20s %8.3f %4d %10.3f\n", "Erreur", SSE, df_SSE, MSE))
  cat(sprintf("%-20s %8.3f %4d\n", "Total", SST, df_SST))
  cat("\nP-valeurs :\n")
  cat("Lignes :", p_ligne, "\nColonnes :", p_colonne, "\nInteraction :", p_interaction, "\n")
  sink()
  cat("Résultats sauvegardés dans", filename, "\n")
}

cat("\nFin du script.\n")