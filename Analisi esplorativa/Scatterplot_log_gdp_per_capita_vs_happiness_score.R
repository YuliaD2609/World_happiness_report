#Lettura del dataset
df <- read.csv(file.choose(), header = TRUE, sep = ",")

#Rimozione dei valori mancanti
df_plot <- df[!is.na(df$log_gdp_per_capita) & !is.na(df$happiness_score), ]

#Scatterplot
plot(df_plot$log_gdp_per_capita,
     df_plot$happiness_score,
     main = "Relazione tra PIL pro capite (log) e punteggio di felicità",
     xlab = "Log PIL pro capite",
     ylab = "Punteggio di felicità",
     col = rgb(27/255, 158/255, 119/255, 0.4),
     pch = 16,
     cex = 0.5)

#regressione lineare
lm_model <- lm(happiness_score ~ log_gdp_per_capita, data = df_plot)
abline(lm_model, col = "#00441b", lwd = 2, lty = 2)

grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")

#Calcolo della correlazione
cor_val <- cor(df_plot$log_gdp_per_capita,
               df_plot$happiness_score,
               use = "complete.obs")

cat("Correlazione (Pearson) tra log_gdp_per_capita e happiness_score:", round(cor_val, 3), "\n")
cat("Risultati del modello lineare:\n")
print(summary(lm_model))




# Visualizza i nomi delle colonne per capire quali sono disponibili
str(df)

# Selezione solo colonne numeriche (esclude stringhe e fattori)
numeric_vars <- sapply(df, is.numeric)
df_num <- df[, numeric_vars]

# Esclude 'happiness_score' dalle variabili indipendenti
other_vars <- setdiff(names(df_num), c("year","happiness_score","happiness_score_sc","log_gdp_per_capita_sc","social_support_sc","healthy_life_expectancy_at_birth_sc","freedom_to_make_life_choices_sc","generosity_sc","perceptions_of_corruption_sc","positive_affect_sc","negative_affect_sc"))

# Ciclo per generare uno scatterplot per ciascuna variabile numerica
for (var in other_vars) {
  
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
  
  cat("\n-----------------------------------------------\n")
  cat("Variabile:", var, "\n")
  cat("Correlazione (Pearson) con happiness_score:", round(cor_val, 3), "\n")
  cat("Risultati modello lineare:\n")
  print(summary(lm_model))
}

