# Caricamento librerie
library(ggplot2)

#creazione dataframe
df <- read.csv(file.choose(), header = TRUE, sep = ",")

# Rimuovo eventuali valori mancanti per sicurezza
df_box <- df[!is.na(df$happiness_score) & !is.na(df$year), ]

# Creazione del boxplot
ggplot(df_box, aes(x = factor(year), y = happiness_score)) +
  geom_boxplot(fill = "#5611F7", color = "#132B43", outlier.color = "red", outlier.shape = 16) +
  labs(title = "Distribuzione del punteggio di felicità per anno",
       x = "Anno",
       y = "Punteggio di felicità (Happiness Score)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
