#creazione dataframe
df <- read.csv(file.choose(), header = TRUE, sep = ",")

# Calcolo media, min e max per paese
happiness_summary <- aggregate(happiness_score ~ country, data = df,
                               FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                                   min = min(x, na.rm = TRUE),
                                                   max = max(x, na.rm = TRUE)))

# Trasforma in data frame leggibile
happiness_summary <- data.frame(
  country = happiness_summary$country,
  mean = happiness_summary$happiness_score[, "mean"],
  min = happiness_summary$happiness_score[, "min"],
  max = happiness_summary$happiness_score[, "max"]
)

# Ordina per felicità media e prendi i primi 50
top50 <- happiness_summary[order(-happiness_summary$mean), ][1:50, ]

# Imposta margini
par(mar = c(8, 5, 4, 2))

# Calcolo limite superiore per evitare overflow
y_max <- max(top50$max) + 0.5

# Disegno barplot con limite visivo
bar_positions <- barplot(top50$mean,
                         names.arg = top50$country,
                         las = 2,
                         col = "#56B117",
                         ylim = c(min(top50$min) - 0.5, y_max),
                         main = "Top 50 Paesi per Felicità Media (2005–2023)",
                         ylab = "Happiness Score",
                         cex.names = 0.8,
                         border = "gray40")

# Linee continue per minimo e massimo
lines(bar_positions, top50$min, type = "o", col = "red", lwd = 2, pch = 19)
lines(bar_positions, top50$max, type = "o", col = "green", lwd = 2, pch = 19)


# Legenda
legend("topleft",
       legend = c("Minimo", "Massimo"),
       col = c("red", "green"),
       pch = c(19, 19),
       lty = c(1, 1),
       lwd = c(2, 2),
       bty = "n")

