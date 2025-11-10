df <- read.csv(file.choose(), header = TRUE, sep = ",")


# Visualizza i nomi delle colonne per capire quali sono disponibili
str(df)

# Selezione solo colonne numeriche (esclude stringhe e fattori)
numeric_vars <- sapply(df, is.numeric)
df_num <- df[, numeric_vars]

vars <- c("log_gdp_per_capita",
                  "social_support",
                  "healthy_life_expectancy_at_birth",
                  "freedom_to_make_life_choices",
                  "generosity",
                  "perceptions_of_corruption",
                  "positive_affect",
                  "negative_affect"
)

# Ciclo per generare uno scatterplot per ciascuna variabile numerica
for (var in vars) {
  
  # Rimozione valori mancanti
  df_plot <- df[!is.na(df[[var]]) & !is.na(df$happiness_score), ]
  
  # Crea scatterplot con stesso stile
  plot(df_plot[[var]],
       df_plot$happiness_score,
       main = paste("Relazione tra", var, "e punteggio di felicità"),
       xlab = var,
       ylab = "Punteggio di felicità",
       col = rgb(27/255, 158/255, 119/255, 0.4),  # stesso colore trasparente
       pch = 16,
       cex = 0.5)
  
  # Regressione lineare
  lm_model <- lm(happiness_score ~ df_plot[[var]], data = df_plot)
  abline(lm_model, col = "#00441b", lwd = 2, lty = 2)
  
  # Aggiunge la griglia
  grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")
  
  # Calcolo e stampa correlazione
  cor_val <- cor(df_plot[[var]], df_plot$happiness_score, use = "complete.obs")
  
  # Calcolo e stampa covarianza
  cov_val <- cov(df_plot[[var]], df_plot$happiness_score, use = "complete.obs")
  
  cat("Variabile:", var, "\n")
  cat("Correlazione (Pearson) con happiness_score:", round(cor_val, 3), "\n")
  cat("Covarianza con happiness_score:" , round(cor_val,3) , "\n"),
  cat("Risultati modello lineare:\n")
  print(summary(lm_model))
}

