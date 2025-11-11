df <- read.csv(file.choose(), header = TRUE, sep = ",")


# Ciclo per generare scatterplot + grafico dei residui
for (var in vars) {
  
  # Rimozione valori mancanti
  df_plot <- df[!is.na(df[[var]]) & !is.na(df$happiness_score), ]
  
  # --- SCATTERPLOT PRINCIPALE ---
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
  
  # Calcolo e stampa informazioni sintetiche
  cat("lm model: \n")
  print(lm_model$coefficients)
  cat("\n")
  
  cat("Media residui:\n")
  print(median(lm_model$residuals))
  cat("\nVarianza residui:\n")
  print(var(lm_model$residuals))
  cat("\nDeviazione standard residui:\n")
  print(sd(lm_model$residuals))
  cat("\n")
  
  cor_val <- cor(df_plot[[var]], df_plot$happiness_score, use = "complete.obs")
  cov_val <- cov(df_plot[[var]], df_plot$happiness_score, use = "complete.obs")
  
  cat("Variabile:", var, "\n")
  cat("Correlazione (Pearson):", round(cor_val, 3), "\n\n")
  print(summary(lm_model))
  
  # --- GRAFICO DEI RESIDUI ---
  maintext <- paste("Diagramma dei residui ", var)
  maintext
  plot(lm_model$fitted.values, lm_model$residuals,
       main = maintext,
       xlab = "Valori stimati (Medie)",
       ylab = "Residui",
       col = rgb(1, 0, 0, 0.6),
       pch = 4,   # crocette
       cex = 1.2)
  
  abline(h = 0, col = "blue", lty = 2, lwd = 2)  # linea orizzontale a 0
  grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")
}

