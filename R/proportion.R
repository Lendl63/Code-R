# --- FONCTION 1 : Choix de la loi ---
# Pour les proportions, on vérifie si l'approximation normale est valide
choisir_loi_proportion <- function(n, p0) {
  if (n * p0 >= 5 && n * (1 - p0) >= 5) {
    return("Normale (Z)")
  } else {
    return("Exacte (Binomiale)") 
  }
}

# --- FONCTION 2 : Valeur Observée ---
calculer_z_obs_prop <- function(k, n, p0) {
  p_hat <- k / n
  erreur_standard <- sqrt((p0 * (1 - p0)) / n)
  return((p_hat - p0) / erreur_standard)
}

# --- FONCTION 3 : Point Critique ---
calculer_critique_prop <- function(alpha, is_unilateral) {
  prob <- if(is_unilateral) 1 - alpha else 1 - (alpha / 2)
  return(qnorm(prob))
}

# --- FONCTION 4 : Zone de Non-Rejet ---
definir_znr_prop <- function(z_crit, is_unilateral, direction) {
  if (!is_unilateral) {
    return(c(-z_crit, z_crit))
  } else {
    if (direction == "greater") return(c(-Inf, z_crit))
    else return(c(-z_crit, Inf))
  }
}

library(shiny)
library(ggplot2)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "242F73"
  ),
  
  titlePanel("Analyseur de Proportions Théoriques"),
  
  sidebarLayout(
    sidebarPanel(
      card(
        card_header("Données de l'échantillon"),
        numericInput("k", "Succès observés (k)", 45),
        numericInput("n", "Taille (n)", 1000),
        sliderInput("p0", "Proportion théorique (p₀)", 0, 1, 0.05)
      ),
      card(
        card_header("Paramètres du Test"),
        selectInput("alpha", "Seuil α", c(0.01, 0.05, 0.1), 0.05),
        checkboxInput("unilat", "Test Unilatéral", FALSE),
        conditionalPanel(
          condition = "input.unilat == true",
          radioButtons("dir", "Direction", c("Supérieur" = "greater", "Inférieur" = "less")) # nolint: line_length_linter.
        )
      )
    ),
    mainPanel(
      navset_card_underline(
        nav_panel("Visualisation", plotOutput("distPlot")),
        nav_panel("Rapport détaillé", verbatimTextOutput("report"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Calculs réactifs utilisant tes fonctions
  data_ana <- reactive({
    alpha <- as.numeric(input$alpha)
    z_obs <- calculer_z_obs_prop(input$k, input$n, input$p0)
    z_crit <- calculer_critique_prop(alpha, input$unilat)
    znr <- definir_znr_prop(z_crit, input$unilat, input$dir)
    loi <- choisir_loi_proportion(input$n, input$p0)
    
    list(z_obs = z_obs, z_crit = z_crit, znr = znr, loi = loi)
  })

  output$distPlot <- renderPlot({
    res <- data_ana()
    x <- seq(-4, 4, length.out = 300)
    df <- data.frame(x = x, y = dnorm(x))
    
    p <- ggplot(df, aes(x, y)) +
      geom_line(color = "#1D2659", size = 1) +
      theme_minimal() +
      labs(title = paste("Loi utilisée :", res$loi), x = "Z-score", y = "")

    # Dessin des zones de rejet (Rouge)
    if (!input$unilat) {
      p <- p + geom_area(data = subset(df, x > res$z_crit), fill = "#E63946", alpha = 0.4) +
               geom_area(data = subset(df, x < -res$z_crit), fill = "#E63946", alpha = 0.4)
    } else if (input$dir == "greater") {
      p <- p + geom_area(data = subset(df, x > res$z_crit), fill = "#E63946", alpha = 0.4)
    } else {
      p <- p + geom_area(data = subset(df, x < -res$z_crit), fill = "#E63946", alpha = 0.4)
    }

    # Ligne de la valeur observée (Bleu tech)
    p + geom_vline(xintercept = res$z_obs, color = "#242F73", linetype = "dashed", size = 1.2) +
        annotate("label", x = res$z_obs, y = 0.35, label = "Valeur Observée", fill = "white")
  })

  output$report <- renderPrint({
    res <- data_ana()
    decision <- if(res$z_obs < res$znr[1] || res$z_obs > res$znr[2]) "REJET DE H0" else "CONSERVATION DE H0"
    
    cat("--- RÉSUMÉ DU TEST ---\n")
    cat("Loi de probabilité :", res$loi, "\n")
    cat("Z-observé          :", round(res$z_obs, 4), "\n")
    cat("Zone de non-rejet  : [", round(res$znr[1], 3), ",", round(res$znr[2], 3), "]\n")
    cat("Verdict            :", decision)
  })
}

shinyApp(ui = ui, server = server)