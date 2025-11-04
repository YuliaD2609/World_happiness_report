# ---------------------------------------------------------
# ANALISI DELLA DISTRIBUZIONE DI HAPPINESS_SCORE
# ---------------------------------------------------------
#creazione dataframe
df <- read.csv(file.choose(), header = TRUE, sep = ",")

# 1. Controllo base dei dati
summary(df$happiness_score)

# 2. Istogramma della distribuzione della variabile target
library(ggplot2)

ggplot(df, aes(x = happiness_score)) +
  geom_histogram(binwidth = 0.3, 
                 fill = "#56B117", 
                 color = "white",
                 alpha = 0.9) +
  geom_density(aes(y = ..count.. * 0.3),   # sovrappone la densità all’istogramma
               color = "red",
               linewidth = 1) +
  labs(title = "Distribuzione della variabile Happiness Score",
       x = "Punteggio di felicità",
       y = "Frequenza") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
