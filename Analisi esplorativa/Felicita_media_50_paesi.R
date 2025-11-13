#Lettura dati
df <- read.csv(file.choose(), header = TRUE, sep = ",")

# Calcolo media, min e max per paese
happiness_summary <- aggregate(happiness_score ~ country, data = df,
                               FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                                   min = min(x, na.rm = TRUE),
                                                   max = max(x, na.rm = TRUE)))

#Trasformazione in dataframe leggibile
happiness_summary <- data.frame(
  country = happiness_summary$country,
  mean = happiness_summary$happiness_score[, "mean"],
  min = happiness_summary$happiness_score[, "min"],
  max = happiness_summary$happiness_score[, "max"]
)

#Selezione dei 50 paesi con felicità media più alta
top50 <- happiness_summary[order(-happiness_summary$mean), ][1:50, ]

par(mar = c(10, 5, 4, 2))   # margine inferiore aumentato

colors_green <- colorRampPalette(c("#00441b", "#238b45", "#74c476", "#c7e9c0", "#f7fcf5"))(nrow(missing_by_country_initial))

#barplot
bap_positions <- barplot(top50$mean,
              names.arg = top50$country,
              las = 2,
              col = colors_green,
              ylim = c(0, 9),
              main = expression(bold("Top 50 Paesi per Felicità Media (2005–2023)")),
              ylab = "Happiness Score",
              cex.names = 0.8,
              )

#Aggiungi linee per min e max
lines(bar_positions, top50$min, type = "o", col = "red", lwd = 2, pch = 19)
lines(bar_positions, top50$max, type = "o", col = "blue", lwd = 2, pch = 19)

#Legenda
legend("topleft",
       legend = c("Minimo", "Massimo"),
       col = c("red", "blue"),
       pch = 19,
       lty = 1,
       lwd = 2,
       bty = "n")




bottom50 <- happiness_summary[order(happiness_summary$mean), ][1:50, ]

par(mar = c(10, 5, 4, 2))

# Barplot
bar_positions <- barplot(bottom50$mean,
                         names.arg = bottom50$country,
                         las = 2,
                         col = colors_green,
                         ylim = c(0, 9),
                         main = expression(bold("Bottom 50 Paesi per Felicità Media (2005–2023)")),
                         ylab = "Happiness Score",
                         cex.names = 0.8)

# Linee min/max
lines(bar_positions, bottom50$min, type = "o", col = "red", lwd = 2, pch = 19)
lines(bar_positions, bottom50$max, type = "o", col = "blue", lwd = 2, pch = 19)

legend("topleft",
       legend = c("Minimo", "Massimo"),
       col = c("red", "blue"),
       pch = 19,
       lty = 1,
       lwd = 2,
       bty = "n")



# Analisi log_gdp_per_capita per paese


gdp_summary <- aggregate(log_gdp_per_capita ~ country, data = df,
                         FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                             min = min(x, na.rm = TRUE),
                                             max = max(x, na.rm = TRUE)))

# Trasformazione in dataframe leggibile
gdp_summary <- data.frame(
  country = gdp_summary$country,
  mean = gdp_summary$log_gdp_per_capita[, "mean"],
  min = gdp_summary$log_gdp_per_capita[, "min"],
  max = gdp_summary$log_gdp_per_capita[, "max"]
)


# 50 Paesi log_gdp_per_capita medio più alto


top50_gdp <- gdp_summary[order(-gdp_summary$mean), ][1:50, ]

par(mar = c(10, 5, 4, 2))

bar_positions <- barplot(top50_gdp$mean,
                         names.arg = top50_gdp$country,
                         las = 2,
                         col = colors_green,
                         ylim = c(0,14),
                         main = expression(bold("Top 50 Paesi per log(GDP pro capite) medio")),
                         ylab = "log(GDP pro capite)",
                         cex.names = 0.8)

lines(bar_positions, top50_gdp$min, type = "o", col = "red", lwd = 2, pch = 19)
lines(bar_positions, top50_gdp$max, type = "o", col = "blue", lwd = 2, pch = 19)

legend("topleft",
       legend = c("Minimo", "Massimo"),
       col = c("red", "blue"),
       pch = 19,
       lty = 1,
       lwd = 2,
       bty = "n")



# 50 Paesi log_gdp_per_capita medio più basso

bottom50_gdp <- gdp_summary[order(gdp_summary$mean), ][1:50, ]

par(mar = c(10, 5, 4, 2))

bar_positions <- barplot(bottom50_gdp$mean,
                         names.arg = bottom50_gdp$country,
                         las = 2,
                         col = colors_green,
                         ylim = c(0,15),
                         main = expression(bold("Bottom 50 Paesi per log(GDP pro capite) medio")),
                         ylab = "log(GDP pro capite)",
                         cex.names = 0.8)

lines(bar_positions, bottom50_gdp$min, type = "o", col = "red", lwd = 2, pch = 19)
lines(bar_positions, bottom50_gdp$max, type = "o", col = "blue", lwd = 2, pch = 19)

legend("topleft",
       legend = c("Minimo", "Massimo"),
       col = c("red", "blue"),
       pch = 19,
       lty = 1,
       lwd = 2,
       bty = "n")

