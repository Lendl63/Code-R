# --- FONCTIONS LOGIQUES ---

choisir_loi <- function(n1, n2) {
  if (n1 < 30 || n2 < 30) list(test = "Student (t)", loi = "t") else list(test = "Normale (Z)", loi = "Z") # nolint: line_length_linter.
}

calculer_obs <- function(x1, x2, s1, s2, n1, n2, eq) {
  if (eq) {
    sp2 <- ((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2) # nolint
    (x1 - x2) / sp2
  } else {
    err <- sqrt((s1^2 / n1) + (s2^2/n2)) # nolint
    (x1 - x2) / err
  }
}

calculer_critique <- function(n1, n2, s1, s2, a, uni, eq) {
  p <- ifelse(uni, 1-a, 1-(a/2)) # nolint
  if (n1 < 30 || n2 < 30) {
    df <- if(eq) n1+n2-2 else ((s1^2/n1+s2^2/n2)^2) / ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1)) # nolint
    return(list(val=qt(p, df), label="t-critique", df=df)) # nolint
  } else {
    return(list(val=qnorm(p), label="Z-critique", df=Inf)) # nolint
  }
}

definir_znr <- function(k, uni, dir) {
  if (!uni) return(list(inf=-k, sup=k, type="Bilatéral")) # nolint
  if (dir == "greater") return(list(inf=-Inf, sup=k, type="Unilatéral (>)")) # nolint # nolint
  return(list(inf=-k, sup=Inf, type="Unilatéral (<)")) # nolint # nolint
}

# --- SAISIE INTERACTIVE ---

cat("--- CONFIGURATION DU TEST DE COMPARAISON DE DEUX MOYENNES ---\n\n")

# --- GROUPE 1 ---
cat("Groupe 1 :\n")
n1     <- as.numeric(readline("  Taille de l'échantillon (n1) : "))
x_bar1 <- as.numeric(readline("  Moyenne observée (x_bar1)    : "))
s1     <- as.numeric(readline("  Écart-type (s1)              : "))

# --- GROUPE 2 ---
cat("\nGroupe 2 :\n")
n2     <- as.numeric(readline("  Taille de l'échantillon (n2) : "))
x_bar2 <- as.numeric(readline("  Moyenne observée (x_bar2)    : "))
s2     <- as.numeric(readline("  Écart-type (s2)              : "))

# --- PARAMÈTRES DU TEST ---
cat("\nParamètres du test :\n")
alpha  <- as.numeric(readline("  Seuil alpha (ex: 0.05)       : "))

# Pour les booléens, on demande TRUE ou FALSE
is_unilateral <- as.logical(toupper(readline("  Test unilatéral ? ( T: Vrai / F: Faux ) : "))) # nolint # nolint

# On ne demande la direction que si le test est unilatéral
direction <- "greater" # Valeur par défaut
if (is_unilateral) {
  tmp_direction <- readline("  Direction ('D: Droite ou G: Gauche') : ")
  direction <- ifelse(tmp_direction == "D", "less", "greater")
}

variances_egales <- as.logical(toupper(readline("  Variances supposées égales ? ( T: Vrai / F: Faux ) : "))) # nolint # nolint

cat("\n--- Fin de la saisie, calcul en cours... ---\n")

# --- CALCULS ET RÉSULTATS ---
loi <- choisir_loi(n1, n2)
v_obs <- calculer_obs(x_bar1, x_bar2, s1, s2, n1, n2, variances_egales)
crit <- calculer_critique(n1, n2, s1, s2, alpha, is_unilateral, variances_egales) # nolint # nolint
znr  <- definir_znr(crit$val, is_unilateral, direction)

# Affichage du rapport final
cat("\n==============================================\n")
cat("            RÉSULTATS DU TEST\n")
cat("==============================================\n")
cat("Valeur observée (t)  :", round(v_obs, 4), "\n")
cat("Point critique (k)   :", round(crit$val, 4), "\n")
cat("Zone de non-rejet    : [", round(znr$inf, 3), ";", round(znr$sup, 3), "]\n") # nolint
cat("----------------------------------------------\n")

rejeter <- v_obs < znr$inf || v_obs > znr$sup
cat("VERDICT : ", ifelse(rejeter, "REJET DE H0", "NON-REJET DE H0"), "\n")
cat("==============================================\n")