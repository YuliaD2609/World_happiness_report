#df <- read.csv(file.choose(), header = TRUE, sep = ",")

vars <- c("log_gdp_per_capita",
          "social_support",
          "healthy_life_expectancy_at_birth",
          "freedom_to_make_life_choices",
          "generosity",
          "perceptions_of_corruption",
          "positive_affect",
          "negative_affect")

var_labels <- c(
  log_gdp_per_capita = "PIL pro capite",
  social_support = "Supporto sociale",
  positive_affect = "Emozioni positive",
  negative_affect = "Emozioni negative",
  healthy_life_expectancy_at_birth = "Aspettativa di vita sana",
  freedom_to_make_life_choices = "Libertà di scelta nella vita",
  generosity = "Generosità",
  perceptions_of_corruption = "Percezione della corruzione"
)
par(mfrow = c(2, 2),
    mar = c(4, 4, 2.5, 1))  

for (var in vars) {
  
  lab <- var_labels[var]
  df_plot <- df[!is.na(df[[var]]) & !is.na(df$happiness_score), ]
  
  plot(df_plot[[var]],
       df_plot$happiness_score,
       main = "",
       xlab = lab,
       ylab = "Punteggio di felicità",
       col = rgb(27/255, 158/255, 119/255, 0.4),
       pch = 16,
       cex = 0.5)
  
  title(
    main = paste("Relazione tra", lab, "e punteggio di felicità"),
    line = 1.5,
    cex.main = 0.9
  )
  
  lm_model <- lm(happiness_score ~ df_plot[[var]], data = df_plot)
  abline(lm_model, col = "#00441b", lwd = 2, lty = 2)
  
  grid(col = "gray80", lty = "dotted")
}

par(mfrow = c(1, 1))


# Matrice di scatterplot
pairs(df[vars], labels = var_labels[vars], cex = 0.1, pch = 16, col="darkgreen",
      main = "Scatterplot matrix delle variabili")

