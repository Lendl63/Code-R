cat("\n\n")
cat("   TEST DE COMPARAISON DE DEUX MOYENNES\n")
cat("\n\n")

# Fonction pour saisir un nombre avec gestion de l'erreur et valeur par défaut
saisir_nombre <- function(message, defaut = NULL) {
  while (TRUE) {
    saisie <- readline(message)
    if (saisie == "" && !is.null(defaut)) {
      return(defaut)
    }
    if (suppressWarnings(!is.na(as.numeric(saisie)))) {
      return(as.numeric(saisie))
    }
    cat("Valeur invalide. Veuillez entrer un nombre.\n")
  }
}

# Fonction pour saisir un booléen (Oui/Non)
saisir_oui_non <- function(message) {
  while (TRUE) {
    saisie <- tolower(readline(message))
    if (saisie %in% c("o", "oui")) return(TRUE)
    if (saisie %in% c("n", "non")) return(FALSE)
    cat("Répondez par 'Oui' ou 'Non'.\n")
  }
}

cat("\n--- TEST DE COMPARAISON DE DEUX MOYENNES ---\n\n")

cat("=== GROUPE 1 ===\n")
n1 <- saisir_nombre("  Taille de l'échantillon (n1) : ")
x1 <- saisir_nombre("  Moyenne (x̄1) : ")
var1 <- saisir_nombre("  Variance de L'échantillon (s1²) : ")
s1 <- sqrt(var1) # nolint

cat("\n=== GROUPE 2 ===\n")
n2 <- saisir_nombre("  Taille de l'échantillon (n2) : ")
x2 <- saisir_nombre("  Moyenne (x̄2) : ")
var2 <- saisir_nombre("  Variance de L'échantillon (s2²) : ")
s2 <- sqrt(var2) # nolint

# Vérification des cas mixtes (non traités)
if ((n1 >= 30 && n2 < 30) || (n1 < 30 && n2 >= 30)) {
  stop("ERREUR : Ce programme ne traite pas le cas où un échantillon est grand et l'autre petit.") # nolint: line_length_linter.
}

# Paramètres du test
alpha <- saisir_nombre("\nSeuil alpha (défaut = 0.05) : ", defaut = 0.05)
if (alpha <= 0 || alpha >= 1) {
  cat("Valeur invalide, utilisation de 0.05 par défaut.\n")
  alpha <- 0.05
}
unilateral <- saisir_oui_non("Test unilatéral ? (Oui/Non) : ")
if (unilateral) {
  direction <- tolower(readline("  Direction : 'g' pour μ1 < μ2 , 'd' pour μ1 > μ2 : ")) # nolint: line_length_linter.
  while (!(direction %in% c("g", "d"))) {
    direction <- tolower(readline("  Tapez 'g' ou 'd' : "))
  }
}

#CHOIX DE LA LOI

if (n1 >= 30 && n2 >= 30) {
  loi <- "normale"
  cat("\n→ Grands échantillons : utilisation de la loi NORMALE (Z).\n")
} else {
  loi <- "student"
  cat("\n→ Petits échantillons : utilisation de la loi de STUDENT (t).\n")
}

#STATISTIQUE DE TEST

diff_moy <- x1 - x2
erreur_std <- sqrt(s1^2/n1 + s2^2/n2) # nolint: infix_spaces_linter.

if (loi == "normale") {
  # Loi normale : pas de test F, on utilise directement Z
  z_obs <- diff_moy / erreur_std
  # Valeur critique
  if (unilateral) {
    if (direction == "d") {
      z_crit <- qnorm(1 - alpha)
      zone_inf <- -Inf
      zone_sup <- z_crit
      cat("\nTest unilatéral à droite (H1: μ1 > μ2)\n")
    } else {
      z_crit <- qnorm(alpha)
      zone_inf <- z_crit
      zone_sup <- Inf
      cat("\nTest unilatéral à gauche (H1: μ1 < μ2)\n")
    }
  } else {
    z_crit <- qnorm(1 - alpha/2) # nolint: infix_spaces_linter.
    zone_inf <- -z_crit
    zone_sup <- z_crit
    cat("\nTest bilatéral (H1: μ1 ≠ μ2)\n")
  }
  # Décision
  rejet <- (z_obs < zone_inf) || (z_obs > zone_sup)
  # Intervalle de confiance (bilatéral)
  if (unilateral) {
    # Pour unilatéral, l'IC n'est pas standard ; on affiche un message
    ic_inf <- NULL
    ic_sup <- NULL
  } else {
    z_ic <- qnorm(1 - alpha/2) # nolint: infix_spaces_linter.
    ic_inf <- diff_moy - z_ic * erreur_std
    ic_sup <- diff_moy + z_ic * erreur_std
  }
  # Affichage
  cat("\n\n")
  cat("RÉSULTATS DU TEST\n")
  cat("\n")
  cat("Loi utilisée                : Normale (Z)\n")
  cat("Statistique observée (Z)    :", round(z_obs, 4), "\n")
  cat("Valeur critique             :", round(z_crit, 4), "\n")
  cat("Zone de non-rejet de H0     : [", round(zone_inf, 3), ";", round(zone_sup, 3), "]\n") # nolint: line_length_linter.
  cat("----------------------------------------------\n")
  cat("VERDICT :", ifelse(rejet, "REJET DE H0", "NON-REJET DE H0"), "\n")
  if (!unilateral && !is.null(ic_inf)) {
    cat(
      "Intervalle de confiance à",
      (1 - alpha) * 100,
      "% pour μ1-μ2 : [",
      round(ic_inf, 3), ";",
      round(ic_sup, 3), "]\n"
    )
  }
} else {  # loi de Student
  # --- TEST F D'ÉGALITÉ DES VARIANCES ---
  if (var1 >= var2) {
    F_calc <- var1 / var2 # nolint: object_name_linter.
    df_num <- n1 - 1
    df_den <- n2 - 1
  } else {
    F_calc <- var2 / var1 # nolint: object_name_linter.
    df_num <- n2 - 1
    df_den <- n1 - 1
  }
  # Valeur critique de F au seuil α/2 (car test bilatéral pour les variances)
  F_crit <- qf(1 - alpha/2, df_num, df_den) # nolint
  variances_egales <- (F_calc < F_crit)
  if (variances_egales) {
    cat("\n→ Test F : F =", round(F_calc,4), "<", round(F_crit,4), "→ variances ÉGALES (test de Fisher).\n") # nolint
    #  Ecart type
    Sp2 <- ((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2)
    # Variance
    Sp <- sqrt(Sp2)
    erreur_std_pool <- Sp * sqrt(1 / n1 + 1 / n2)
    t_obs <- diff_moy / erreur_std_pool
    ddl <- n1 + n2 - 2
    # Valeur critique t
    if (unilateral) {
      if (direction == "d") {
        t_crit <- qt(1 - alpha, ddl)
        zone_inf <- -Inf
        zone_sup <- t_crit
      } else {
        t_crit <- qt(alpha, ddl)
        zone_inf <- t_crit
        zone_sup <- Inf
      }
    } else {
      t_crit <- qt(1 - alpha / 2, ddl)
      zone_inf <- -t_crit
      zone_sup <- t_crit
    }
    rejet <- (t_obs < zone_inf) || (t_obs > zone_sup)
    # IC
    if (unilateral) {
      ic_inf <- ic_sup <- NULL
    } else {
      t_ic <- qt(1 - alpha / 2, ddl)
      ic_inf <- diff_moy - t_ic * erreur_std_pool
      ic_sup <- diff_moy + t_ic * erreur_std_pool
    }
  } else {
    cat("\n→ Test F : F =", round(F_calc,4), ">=", round(F_crit,4), "→ variances INÉGALES (test de Fisher).\n") # nolint
    # Valeur observer
    t_obs <- diff_moy / erreur_std
    

    mu <- (var1 / n1) / (var1 / n1 + var2 / n2)
    ddl <- 1 / (mu^2 / (n1 - 1) + (1 - mu)^2 / (n2 - 1))
    # Valeur critique t
    if (unilateral) {
      if (direction == "d") {
        t_crit <- qt(1 - alpha, ddl)
        zone_inf <- -Inf
        zone_sup <- t_crit
      } else {
        t_crit <- qt(alpha, ddl)
        zone_inf <- t_crit
        zone_sup <- Inf
      }
    } else {
      t_crit <- qt(1 - alpha / 2, ddl)
      zone_inf <- -t_crit
      zone_sup <- t_crit
    }
    rejet <- (t_obs < zone_inf) || (t_obs > zone_sup)
    # IC
    if (unilateral) {
      ic_inf <- ic_sup <- NULL
    } else {
      t_ic <- qt(1 - alpha / 2, ddl)
      ic_inf <- diff_moy - t_ic * erreur_std
      ic_sup <- diff_moy + t_ic * erreur_std
    }
  }
  # Affichage des résultats (loi de Student)
  cat("\n\n")
  cat("RÉSULTATS DU TEST\n")
  cat("\n")
  cat("Loi utilisée                : Student (t)\n")
  cat("Statistique observée (t)    :", round(t_obs, 4), "\n")
  cat("Valeur critique             :", round(t_crit, 4), "\n")
  cat("Degrés de liberté           :", round(ddl, 2), "\n")
  cat("Zone de non-rejet de H0     : [", round(zone_inf, 4), ";", round(zone_sup, 4), "]\n") # nolint
  cat("----------------------------------------------\n")
  cat("VERDICT :", ifelse(rejet, "REJET DE H0", "NON-REJET DE H0"), "\n")
  if (!unilateral && !is.null(ic_inf)) {
    cat(
      "Intervalle de confiance à",
      (1 - alpha) * 100, "% pour μ1-μ2 : [",
      round(ic_inf, 3), ";",
      round(ic_sup, 3), "]\n"
    )
  }
}