# 1. Caricamento del dataset
df <- read.csv(file.choose(), header = TRUE, sep = ",")

# 2. Rimuovo eventuali valori mancanti
df_box <- df[!is.na(df$happiness_score) & !is.na(df$year), ]

# 3. Creazione del boxplot base
boxplot(happiness_score ~ factor(year),
        data = df_box,
        col = "#5611F7",
        border = "#132B43",
        outcol = "red",           # colore punti outlier
        pch = 16,                 # forma outlier (punto pieno)
        main = "Distribuzione del punteggio di felicità per anno",
        xlab = "Anno",
        ylab = "Punteggio di felicità (Happiness Score)",
        cex.lab = 1.1,
        cex.axis = 0.8,
        las = 2)                  # ruota etichette anni per leggibilità

grid(nx = NA, ny = NULL, col = "gray80", lty = "dotted")
title(main = "Distribuzione del punteggio di felicità per anno", font.main = 2)
