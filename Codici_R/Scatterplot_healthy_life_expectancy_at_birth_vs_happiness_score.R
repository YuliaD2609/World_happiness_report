# 1. Caricamento libreria
library(ggplot2)

#creazione dataframe
df <- read.csv(file.choose(), header = TRUE, sep = ",")

# 2. Rimozione dei valori mancanti
df_health <- df[!is.na(df$healthy_life_expectancy_at_birth) & !is.na(df$happiness_score), ]

# 3. Scatterplot con linea di regressione
ggplot(df_health, aes(x = healthy_life_expectancy_at_birth, y = happiness_score)) +
  geom_point(alpha = 0.3, color = "#1b9e77") +
  geom_smooth(method = "lm", se = TRUE, color = "#00441b", linetype = "dashed") +
  labs(title = "Relazione tra aspettativa di vita sana e punteggio di felicità",
       x = "Aspettativa di vita sana alla nascita",
       y = "Punteggio di felicità") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# 4. Calcolo della correlazione e modello lineare
cor_health <- cor(df_health$healthy_life_expectancy_at_birth, df_health$happiness_score, use = "complete.obs")
lm_health <- lm(happiness_score ~ healthy_life_expectancy_at_birth, data = df_health)
summary(lm_health)

# 5. Output riassuntivo
cat("Correlazione (Pearson) tra healthy_life_expectancy_at_birth e happiness_score:", round(cor_health, 3), "\n")
cat("Risultati del modello lineare:\n")
print(summary(lm_health))
