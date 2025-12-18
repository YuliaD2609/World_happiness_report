#Lettura dati
df <- read.csv(file.choose(), header = TRUE, sep = ",")
par(mfrow = c(1,1))
# Calcolo media, min e max per paese
happiness_summary <- aggregate(happiness_score ~ country, data = df,
                               FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                                   min = min(x, na.rm = TRUE),
                                                   max = max(x, na.rm = TRUE)))

# Trasformazione in dataframe
happiness_summary <- data.frame(
  country = happiness_summary$country,
  mean = happiness_summary$happiness_score[, "mean"],
  min = happiness_summary$happiness_score[, "min"],
  max = happiness_summary$happiness_score[, "max"]
)

# Selezione dei 50 paesi con felicità media più alta
top50 <- happiness_summary[order(-happiness_summary$mean), ][1:50, ]

# Margini
par(mar = c(10, 5, 4, 2))

y_min <- min(top50$min) - 0.2
y_max <- max(top50$max) + 0.5

colors_green <- colorRampPalette(c("#00441b", "#238b45", "#74c476", "#c7e9c0", "#f7fcf5"))(nrow(missing_by_country_initial))

# Barplot
bar_positions <- barplot(top50$mean,
              names.arg = top50$country,
              las = 2,
              col = colors_green,
              ylim = c(0, 9),
              main = expression(bold("Top 50 Paesi per Felicità Media (2005–2023)")),
              ylab = "Felicità",
              cex.names = 0.8,
              )

# Aggiunta linee per min e max
lines(bar_positions, top50$min, type = "o", col = "red", lwd = 2, pch = 19)
lines(bar_positions, top50$max, type = "o", col = "blue", lwd = 2, pch = 19)

# Legenda 
legend("topleft",
       legend = c("Minimo", "Massimo"),
       col = c("red", "blue"),
       pch = 19,
       lty = 1,
       lwd = 2,
       bty = "n")

bottom50 <- happiness_summary[order(happiness_summary$mean), ][1:50, ]

# Margini e intervallo Y
par(mar = c(10, 5, 4, 2))
y_min <- min(bottom50$min) - 0.2
y_max <- max(bottom50$max) + 0.5

# Barplot
bar_positions <- barplot(bottom50$mean,
                         names.arg = bottom50$country,
                         las = 2,
                         col = colors_green,
                         ylim = c(0, 9),
                         main = expression(bold("Bottom 50 Paesi per Felicità Media (2005–2023)")),
                         ylab = "Felicità",
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


# Log_gdp_per_capita
gdp_summary <- aggregate(log_gdp_per_capita ~ country, data = df,
                         FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                             min = min(x, na.rm = TRUE),
                                             max = max(x, na.rm = TRUE)))

gdp_summary <- data.frame(
  country = gdp_summary$country,
  mean = gdp_summary$log_gdp_per_capita[, "mean"],
  min = gdp_summary$log_gdp_per_capita[, "min"],
  max = gdp_summary$log_gdp_per_capita[, "max"]
)

# Log_gdp_per_capita top
top50_gdp <- gdp_summary[order(-gdp_summary$mean), ][1:50, ]
par(mar = c(10, 5, 4, 2))
bar_positions <- barplot(top50_gdp$mean,
                         names.arg = top50_gdp$country,
                         las = 2,
                         col = colors_green,
                         ylim = c(0,14),
                         main = expression(bold("Top 50 Paesi per log(GDP pro capite) medio")),
                         ylab = "PIL pro capite",
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


# Log_gdp_per_capita bottom
bottom50_gdp <- gdp_summary[order(gdp_summary$mean), ][1:50, ]
par(mar = c(10, 5, 4, 2))
bar_positions <- barplot(bottom50_gdp$mean,
                         names.arg = bottom50_gdp$country,
                         las = 2,
                         col = colors_green,
                         ylim = c(0,15),
                         main = expression(bold("Bottom 50 Paesi per log(GDP pro capite) medio")),
                         ylab = "PIL pro capite",
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

# Social_support
social_summary <- aggregate(social_support ~ country, data = df,
                            FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                                min = min(x, na.rm = TRUE),
                                                max = max(x, na.rm = TRUE)))
social_summary <- data.frame(
  country = social_summary$country,
  mean = social_summary$social_support[, "mean"],
  min = social_summary$social_support[, "min"],
  max = social_summary$social_support[, "max"]
)

# Social_ support top 50
top50_social <- social_summary[order(-social_summary$mean), ][1:50, ]
par(mar = c(10, 5, 4, 2))
bar_positions <- barplot(top50_social$mean,
                         names.arg = top50_social$country,
                         las = 2,
                         col = colors_green,
                         ylim = c(0,1.2),
                         main = expression(bold("Top 50 Paesi per Social Support medio")),
                         ylab = "Supporto sociale",
                         cex.names = 0.8)
lines(bar_positions, top50_social$min, type = "o", col = "red", lwd = 2, pch = 19)
lines(bar_positions, top50_social$max, type = "o", col = "blue", lwd = 2, pch = 19)
legend("topleft",
       legend = c("Minimo", "Massimo"),
       col = c("red", "blue"),
       pch = 19,
       lty = 1,
       lwd = 2,
       bty = "n")

# Social_support bottom 50
bottom50_social <- social_summary[order(social_summary$mean), ][1:50, ]
par(mar = c(10, 5, 4, 2))

bar_positions <- barplot(bottom50_social$mean,
                         names.arg = bottom50_social$country,
                         las = 2,
                         col = colors_green,
                         ylim = c(0,1),
                         main = expression(bold("Bottom 50 Paesi per Social Support medio")),
                         ylab = "Supporto sociale",
                         cex.names = 0.8)
lines(bar_positions, bottom50_social$min, type = "o", col = "red", lwd = 2, pch = 19)
lines(bar_positions, bottom50_social$max, type = "o", col = "blue", lwd = 2, pch = 19)
legend("topleft",
       legend = c("Minimo", "Massimo"),
       col = c("red", "blue"),
       pch = 19,
       lty = 1,
       lwd = 2,
       bty = "n")

# Healthn summary
health_summary <- aggregate(healthy_life_expectancy_at_birth ~ country, data = df,
                            FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                                min = min(x, na.rm = TRUE),
                                                max = max(x, na.rm = TRUE)))
health_summary <- data.frame(
  country = health_summary$country,
  mean = health_summary$healthy_life_expectancy_at_birth[, "mean"],
  min = health_summary$healthy_life_expectancy_at_birth[, "min"],
  max = health_summary$healthy_life_expectancy_at_birth[, "max"]
)

# Health top 50
top50_health <- health_summary[order(-health_summary$mean), ][1:50, ]
par(mar = c(10, 5, 4, 2))
bar_positions <- barplot(top50_health$mean,
                         names.arg = top50_health$country,
                         las = 2,
                         col = colors_green,
                         ylim = c(0,90),
                         main = expression(bold("Top 50 Paesi per Aspettativa di Vita Sana alla Nascita")),
                         ylab = "Aspettativa di vita sana",
                         cex.names = 0.8)
lines(bar_positions, top50_health$min, type = "o", col = "red", lwd = 2, pch = 19)
lines(bar_positions, top50_health$max, type = "o", col = "blue", lwd = 2, pch = 19)
legend("topleft",
       legend = c("Minimo", "Massimo"),
       col = c("red", "blue"),
       pch = 19,
       lty = 1,
       lwd = 2,
       bty = "n")

# Health bottom 50
bottom50_health <- health_summary[order(health_summary$mean), ][1:50, ]
par(mar = c(10, 5, 4, 2))
bar_positions <- barplot(bottom50_health$mean,
                         names.arg = bottom50_health$country,
                         las = 2,
                         col = colors_green,
                         ylim = c(0,80),
                         main = expression(bold("Bottom 50 Paesi per Aspettativa di Vita Sana alla Nascita")),
                         ylab = "Aspettativa di vita sana",
                         cex.names = 0.8)
lines(bar_positions, bottom50_health$min, type = "o", col = "red", lwd = 2, pch = 19)
lines(bar_positions, bottom50_health$max, type = "o", col = "blue", lwd = 2, pch = 19)
legend("topleft",
       legend = c("Minimo", "Massimo"),
       col = c("red", "blue"),
       pch = 19,
       lty = 1,
       lwd = 2,
       bty = "n"))

# Freedom summary
freedom_summary <- aggregate(freedom_to_make_life_choices ~ country, data = df,
                             FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                                 min = min(x, na.rm = TRUE),
                                                 max = max(x, na.rm = TRUE)))

freedom_summary <- data.frame(
  country = freedom_summary$country,
  mean = freedom_summary$freedom_to_make_life_choices[, "mean"],
  min = freedom_summary$freedom_to_make_life_choices[, "min"],
  max = freedom_summary$freedom_to_make_life_choices[, "max"]
)

top50_freedom <- freedom_summary[order(-freedom_summary$mean), ][1:50, ]
bottom50_freedom <- freedom_summary[order(freedom_summary$mean), ][1:50, ]

# Top 50 Freedom 
par(mar = c(10, 5, 4, 2))
bar_positions <- barplot(top50_freedom$mean,
                         names.arg = top50_freedom$country,
                         las = 2,
                         col = colors_green,
                         ylim = c(0, 1.2),
                         main = expression(bold("Top 50 Paesi per Libertà di Scelta nella Vita")),
                         ylab = "Libertà di scelta nella vita",
                         cex.names = 0.8)
lines(bar_positions, top50_freedom$min, type = "o", col = "red", lwd = 2, pch = 19)
lines(bar_positions, top50_freedom$max, type = "o", col = "blue", lwd = 2, pch = 19)

legend("topleft",
       legend = c("Minimo", "Massimo"),
       col = c("red", "blue"),
       pch = 19,
       lty = 1,
       lwd = 2,
       bty = "n")

# Bottom 50 summary
par(mar = c(10, 5, 4, 2))
bar_positions <- barplot(bottom50_freedom$mean,
                         names.arg = bottom50_freedom$country,
                         las = 2,
                         col = colors_green,
                         ylim = c(0, 1.2),
                         main = expression(bold("Bottom 50 Paesi per Libertà di Scelta nella Vita")),
                         ylab = "Libertà di scelta nella vita",
                         cex.names = 0.8)
lines(bar_positions, bottom50_freedom$min, type = "o", col = "red", lwd = 2, pch = 19)
lines(bar_positions, bottom50_freedom$max, type = "o", col = "blue", lwd = 2, pch = 19)

legend("topleft",
       legend = c("Minimo", "Massimo"),
       col = c("red", "blue"),
       pch = 19,
       lty = 1,
       lwd = 2,
       bty = "n")

# Generosity
generosity_summary <- aggregate(generosity ~ country, data = df,
                                FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                                    min = min(x, na.rm = TRUE),
                                                    max = max(x, na.rm = TRUE)))

generosity_summary <- data.frame(
  country = generosity_summary$country,
  mean = generosity_summary$generosity[, "mean"],
  min = generosity_summary$generosity[, "min"],
  max = generosity_summary$generosity[, "max"]
)

top50_generosity <- generosity_summary[order(-generosity_summary$mean), ][1:50, ]
bottom50_generosity <- generosity_summary[order(generosity_summary$mean), ][1:50, ]

# Generosity top 50
par(mar = c(10, 5, 4, 2))
bar_positions <- barplot(top50_generosity$mean,
                         names.arg = top50_generosity$country,
                         las = 2,
                         col = colors_green,
                         ylim = c(0,0.8),
                         main = expression(bold("Top 50 Paesi per Generosità Media")),
                         ylab = "Generosità",
                         cex.names = 0.8)
lines(bar_positions, top50_generosity$min, type = "o", col = "red", lwd = 2, pch = 19)
lines(bar_positions, top50_generosity$max, type = "o", col = "blue", lwd = 2, pch = 19)

legend("topleft",
       legend = c("Minimo", "Massimo"),
       col = c("red", "blue"),
       pch = 19,
       lty = 1,
       lwd = 2,
       bty = "n")

# Generosity bottom
par(mar = c(10, 5, 4, 2))
bar_positions <- barplot(bottom50_generosity$mean,
                         names.arg = bottom50_generosity$country,
                         las = 2,
                         col = colors_green,
                         ylim = c(min(bottom50_generosity$mean) - 0.05, max(bottom50_generosity$mean) + 0.05),
                         main = expression(bold("Bottom 50 Paesi per Generosità Media")),
                         ylab = "Generosità",
                         cex.names = 0.8)
lines(bar_positions, bottom50_generosity$min, type = "o", col = "red", lwd = 2, pch = 19)
lines(bar_positions, bottom50_generosity$max, type = "o", col = "blue", lwd = 2, pch = 19)


