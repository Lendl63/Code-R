# ----- Fonction a utiliser -----

choisir_test_statisque <- function(n) {
  # On se base sur la taille de l'echantillon
  if (n < 30) {
    nom_test <- "Test t de Student"
    loi <- "Loi de Student (t)"
  } else {
    nom_test <- "Test Z"
    loi <- "Loi Normal (Z)"
  }

  return(list(test = nom_test, loi = loi))
}

calculer_valeur_observer <- function(x_bar, mu_zero, s, n) {
  # Calcul de sigma x_bar (erreur)
  erreur <- s / sqrt(n)

  # Calcul de la valeur observé
  valeur_observer <- (x_bar - mu_zero) / erreur

  return(valeur_observer)
}

calculer_point_critique <- function(n, alpha, is_unilateral) {
  # Determiner la probabiliter
  # 1 - alpha si unilaterale et 1 - alpha/2 sinon

  proba <- if(is_unilateral) 1 - alpha else 1 - (alpha / 2)

  # Calcul du point critique
  if (n < 30) {
    # Loi de Student
    point_critique <- pt(proba, df = n - 1)
    type_loi <- "t_critique"
  } else {
    # Loi Normal
    point_critique <- qnorm(proba)
    type_loi <- "Z_critique"
  }

  return(list(valeur = point_critique, label = type_loi))
}

definir_zone_non_rejet <- function(point_critique, is_unilateral, direction = "greater") { # nolint: line_length_linter.
  if (!is_unilateral) {
    # Test Bilateral : intervale = [-k, k]
    borne_inf <- (-1) * point_critique
    borne_sup <- point_critique
    type <- "Bilateral"
  } else {
    # Test unilateral
    type <- "Unilateral"
    if (direction == "greater") {
      # Unilateral a GAUCHE
      borne_inf <- (-1) * Inf
      borne_sup <- point_critique
    } else {
      # Unilateral a DROITE
      borne_inf <- point_critique
      borne_sup <- Inf
    }
  }

  return(list(
    inf = borne_inf,
    sup = borne_sup,
    label = paste("Zone ne non rejet de l'hypothese nul (", type, ")")
  ))
}

# ----- declaration des variables -----

# Moyenne de l'echantillon
x_bar <- 12.0

# Taille de l'echantillon
n <- 35

# L'ecart type sigma (s)
s <- 2.0

# Moyenne theorique
mu_zero <- 9.0

# Seuil de signification
alpha <- 0.05

# Type de test
# TRUE = unilareral et FALSE = bilaterl
is_unilateral <- FALSE

# Direction du test si uniteral
# greater ou less
direction <- "greater"

# ----- Implementation test -----

# 2. Choie du test
type_test <- choisir_test_statisque(n)
cat("La loie de probabiliter à utiliser ici est celle de ", type_test$loi, "\n")

# 3. Valeur observer
valeur_observer <- calculer_valeur_observer(x_bar, mu_zero, s, n)
cat("LA valeur observer du test est ", valeur_observer, "\n")

# 4. Point critique
point_critique <- calculer_point_critique(n, alpha, is_unilateral)
cat("Point critique : ", point_critique$valeur, "\n")

# 5. Zone de non rejet
zone_non_rejet <- definir_zone_non_rejet(point_critique$valeur, is_unilateral, direction) # nolint: line_length_linter.

# Test d'affichage
cat(zone_non_rejet$label, ": [", zone_non_rejet$inf, ";", zone_non_rejet$sup, "] \n") # nolint: line_length_linter.