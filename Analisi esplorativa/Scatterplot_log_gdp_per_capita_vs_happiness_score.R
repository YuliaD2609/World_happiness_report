# Lettura del dataset
df <- read.csv(file.choose(), header = TRUE, sep = ",")

# Rimozione dei valori mancanti
df_plot <- df[!is.na(df$log_gdp_per_capita) & !is.na(df$happiness_score), ]

# Scatterplot
plot(df_plot$log_gdp_per_capita,
     df_plot$happiness_score,
     main = "Relazione tra PIL pro capite (log) e punteggio di felicità",
     xlab = "Log PIL pro capite",
     ylab = "Punteggio di felicità",
     col = rgb(27/255, 158/255, 119/255, 0.4),
     pch = 16,
     cex = 0.5)

# Regressione lineare
lm_model <- lm(happiness_score ~ log_gdp_per_capita, data = df_plot)
abline(lm_model, col = "#00441b", lwd = 2, lty = 2)

grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")

# Calcolo della correlazione
cor_val <- cor(df_plot$log_gdp_per_capita,
               df_plot$happiness_score,
               use = "complete.obs")

cat("Correlazione (Pearson) tra log_gdp_per_capita e happiness_score:", round(cor_val, 3), "\n")
cat("Risultati del modello lineare:\n")
print(summary(lm_model))
