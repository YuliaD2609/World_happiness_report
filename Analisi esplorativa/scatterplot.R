df <- read.csv(file.choose(), header = TRUE, sep = ",")

vars <- c("log_gdp_per_capita",
          "social_support",
          "healthy_life_expectancy_at_birth",
          "freedom_to_make_life_choices",
          "generosity",
          "perceptions_of_corruption",
          "positive_affect",
          "negative_affect")

# Scatterplot
for (var in vars) {
  
  df_plot <- df[!is.na(df[[var]]) & !is.na(df$happiness_score), ]
  
  plot(df_plot[[var]],
       df_plot$happiness_score,
       main = paste("Relazione tra", var, "e punteggio di felicità"),
       xlab = var,
       ylab = "Punteggio di felicità",
       col = rgb(27/255, 158/255, 119/255, 0.4),
       pch = 16,
       cex = 0.5)
  
  # Modello lineare
  lm_model <- lm(happiness_score ~ df_plot[[var]], data = df_plot)
  abline(lm_model, col = "#00441b", lwd = 2, lty = 2)
  grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")
  
  # Statistiche sintetiche
  cat("Modello per:", var, "\n")
  print(summary(lm_model))
  
  cor_val <- cor(df_plot[[var]], df_plot$happiness_score)
  cat("Correlazione (Pearson):", round(cor_val, 3), "\n\n")
}

# Residui

for (var in vars) {
  
  df_plot <- df[!is.na(df[[var]]) & !is.na(df$happiness_score), ]
  
  lm_model <- lm(happiness_score ~ df_plot[[var]], data = df_plot)
  
  maintext <- paste("Diagramma dei residui:", var)
  
  plot(lm_model$fitted.values,
       lm_model$residuals,
       main = maintext,
       xlab = "Valori stimati",
       ylab = "Residui",
       col = rgb(1, 0, 0, 0.6),
       pch = 19,
       cex = 0.5)
  
  abline(h = 0, col = "blue", lty = 2, lwd = 2)
  grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")
}

# Matrice di scatterplot
pairs(df[vars], cex = 0.1, pch = 16, col="darkgreen",
      main = "Scatterplot matrix delle variabili")

