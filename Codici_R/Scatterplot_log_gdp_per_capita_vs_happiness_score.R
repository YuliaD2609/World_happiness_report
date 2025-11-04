#creazione dataframe
df <- read.csv(file.choose(), header = TRUE, sep = ",")

library(ggplot2)

# 2. Facciamo un filtro base (escludere NAs)
df_plot <- df[!is.na(df$log_gdp_per_capita) & !is.na(df$happiness_score), ]

# 3. Scatterplot con linea di regressione
ggplot(df_plot, aes(x = log_gdp_per_capita, y = happiness_score)) +
  geom_point(alpha = 0.3, color = "#2c7fb8") +
  geom_smooth(method = "lm", se = TRUE, color = "#084594", linetype = "dashed") +
  labs(title = "Relazione tra PIL pro capite (log) e punteggio di felicità",
       x = "Log PIL pro capite",
       y = "Punteggio felicità") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# 4. Calcolo della correlazione e della regressione lineare
cor_val <- cor(df_plot$log_gdp_per_capita, df_plot$happiness_score, use = "complete.obs")
lm_model <- lm(happiness_score ~ log_gdp_per_capita, data = df_plot)
summary(lm_model)

# 5. Stampa dei risultati
cat("Correlazione (Pearson) tra log_gdp_per_capita e happiness_score:", round(cor_val, 3), "\n")
cat("Risultati del modello lineare:\n")
print(summary(lm_model))
