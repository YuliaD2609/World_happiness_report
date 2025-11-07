#Caricamento del dataset
df <- read.csv(file.choose(), header = TRUE, sep = ",")

#Rimuovo eventuali valori mancanti
df_box <- df[!is.na(df$happiness_score) & !is.na(df$year), ]

#Boxplot
boxplot(happiness_score ~ factor(year),
        data = df_box,
        col = "#74c476",
        border = "#132B43",
        outcol = "blue",           
        pch = 16,                 
        main = "Distribuzione del punteggio di felicità per anno",
        xlab = "Anno",
        ylab = "Punteggio di felicità",
        cex.lab = 1.1,
        cex.axis = 0.8,
        las = 2)              

grid(nx = NA, ny = NULL, col = "gray80", lty = "dotted")
title(main = "Distribuzione del punteggio di felicità per anno", font.main = 2)

