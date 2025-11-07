#Lettura del dataset
df <- read.csv(file.choose(), header = TRUE, sep = ",")

#Rimozione dei valori mancanti
df_health <- df[!is.na(df$healthy_life_expectancy_at_birth) & !is.na(df$happiness_score), ]

#Scatterplot
plot(df_health$healthy_life_expectancy_at_birth,
     df_health$happiness_score,
     main = "Relazione tra aspettativa di vita sana e punteggio di felicità",
     xlab = "Aspettativa di vita sana alla nascita",
     ylab = "Punteggio di felicità",
     col = rgb(27/255, 158/255, 119/255, 0.4),
     pch = 16,
     cex = 0.5)

#linea di regressione lineare
lm_health <- lm(happiness_score ~ healthy_life_expectancy_at_birth, data = df_health)
abline(lm_health, col = "#00441b", lwd = 2, lty = 2)

grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")

#Calcolo e stampa della correlazione
cor_health <- cor(df_health$healthy_life_expectancy_at_birth,
                  df_health$happiness_score,
                  use = "complete.obs")

cat("Correlazione (Pearson) tra aspettativa di vita sana e felicità:", round(cor_health, 3), "\n")

cat("Risultati del modello lineare:\n")
print(summary(lm_health))

