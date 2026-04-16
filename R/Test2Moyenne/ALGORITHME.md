# ALGORITHME : Test de Comparaison de Deux Moyennes

## 1. SAISIE DES DONNÉES
   - Lire n1, x̄1, s1 (groupe 1)
   - Lire n2, x̄2, s2 (groupe 2)
   - Lire alpha (seuil de significativité, défaut = 0.05)
   - Lire type test : bilatéral ou unilatéral
   - Si unilatéral : lire direction ('g' ou 'd')

## 2. VALIDATION
   - Vérifier que n1 et n2 ne sont pas de tailles mixtes (l'un < 30, l'autre ≥ 30)
   - Si erreur → STOP

## 3. CHOIX DE LA LOI DE DISTRIBUTION
   SI n1 ≥ 30 ET n2 ≥ 30 ALORS
      Utiliser LOI NORMALE (Z)
   SINON
      Utiliser LOI DE STUDENT (t)
   FIN SI

## 4. CALCUL DE LA STATISTIQUE DE TEST

### Cas A : LOI NORMALE (Z)
   - Calculer diff_moy = x̄1 - x̄2
   - Calculer erreur_std = √(s1²/n1 + s2²/n2)
   - Calculer Z_obs = diff_moy / erreur_std
   
   - SI test bilatéral ALORS
        Z_crit = Z(1 - α/2)
        zone = [-Z_crit ; Z_crit]
     SINON SI direction = 'd' (droite) ALORS
        Z_crit = Z(1 - α)
        zone = [-∞ ; Z_crit]
     SINON (direction = 'g', gauche) ALORS
        Z_crit = Z(α)
        zone = [Z_crit ; +∞]
     FIN SI

### Cas B : LOI DE STUDENT (t)
   **Étape B1 : TEST F D'ÉGALITÉ DES VARIANCES**
   - Calculer F_calc = max(s1², s2²) / min(s1², s2²)
   - Calculer ddl_num et ddl_den (n1-1 et n2-1)
   - F_crit = F(1 - α/2, ddl_num, ddl_den)
   
   - SI F_calc < F_crit ALORS
        Variances ÉGALES → Aller à B2a
     SINON
        Variances INÉGALES → Aller à B2b
     FIN SI

   **Étape B2a : Variances égales (Test de Student classique)**
   - Sp² = [(n1-1)·s1² + (n2-1)·s2²] / (n1+n2-2)
   - Sp = √Sp²
   - erreur_std_pool = Sp · √(1/n1 + 1/n2)
   - t_obs = diff_moy / erreur_std_pool
   - ddl = n1 + n2 - 2
   - Déterminer t_crit selon type test (bilatéral/unilatéral)
   - Définir zone de rejet

   **Étape B2b : Variances inégales (Test de Welch)**
   - t_obs = diff_moy / √(s1²/n1 + s2²/n2)
   - μ = (s1²/n1) / (s1²/n1 + s2²/n2)
   - ddl = 1 / [μ²/(n1-1) + (1-μ)²/(n2-1)]
   - Déterminer t_crit selon type test
   - Définir zone de rejet

## 5. DÉCISION DU TEST
   - SI stat_obs ∈ zone_rejet ALORS
        REJET DE H0
     SINON
        NON-REJET DE H0
     FIN SI

## 6. CALCUL DE L'INTERVALLE DE CONFIANCE (IC)
   - SI test bilatéral ALORS
        Pour loi normale :
           IC = [diff_moy - Z(1-α/2)·erreur_std ; diff_moy + Z(1-α/2)·erreur_std]
        Pour loi Student :
           IC = [diff_moy - t(1-α/2, ddl)·erreur_std ; diff_moy + t(1-α/2, ddl)·erreur_std]
     SINON
        IC non calculé (cas unilatéral)
     FIN SI

## 7. AFFICHAGE DES RÉSULTATS
   - Loi utilisée
   - Statistique observée
   - Valeur critique
   - Degrés de liberté (si applicable)
   - Zone de non-rejet
   - VERDICT (Rejet ou Non-rejet)
   - Intervalle de confiance (si bilatéral)