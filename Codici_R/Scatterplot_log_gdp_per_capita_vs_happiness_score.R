# 1. Lettura del dataset
df <- read.csv(file.choose(), header = TRUE, sep = ",")

# 2. Rimozione dei valori mancanti
df_plot <- df[!is.na(df$log_gdp_per_capita) & !is.na(df$happiness_score), ]

# 3. Creazione scatterplot base
plot(df_plot$log_gdp_per_capita,
     df_plot$happiness_score,
     main = "Relazione tra PIL pro capite (log) e punteggio di felicità",
     xlab = "Log PIL pro capite",
     ylab = "Punteggio di felicità",
     col = rgb(44/255, 127/255, 184/255, 0.4),  # blu semi-trasparente
     pch = 16,
     cex = 1.1)

# 4. Aggiunta linea di regressione lineare
lm_model <- lm(happiness_score ~ log_gdp_per_capita, data = df_plot)
abline(lm_model, col = "#084594", lwd = 2, lty = 2)

# 5. Aggiunta griglia per leggibilità
grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")

# 6. Calcolo della correlazione
cor_val <- cor(df_plot$log_gdp_per_capita,
               df_plot$happiness_score,
               use = "complete.obs")

# 7. Output dei risultati
cat("Correlazione (Pearson) tra log_gdp_per_capita e happiness_score:", round(cor_val, 3), "\n")
cat("Risultati del modello lineare:\n")
print(summary(lm_model))

