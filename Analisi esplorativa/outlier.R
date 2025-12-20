df <- read.csv(file.choose(), header = TRUE, sep = ",")

# Variabili
vars <- c("log_gdp_per_capita",
          "social_support",
          "positive_affect",
          "negative_affect",
          "healthy_life_expectancy_at_birth",
          "freedom_to_make_life_choices",
          "generosity",
          "perceptions_of_corruption")

# Etichette descrittive
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

# Conteggio outlier sulle variabili 
outlier_counts <- sapply(vars, function(v) {
  sum(abs(scale(df[[v]])) > 3, na.rm = TRUE)
})
print(outlier_counts)

# Elenco dettagliato degli outlier
outliers_list <- lapply(vars, function(v) {
  df[which(abs(scale(df[[v]])) > 3), c("country", "year", v)]
})
names(outliers_list) <- vars
outliers_list

# Impostazioni grafiche
par(mfrow = c(2, 4),
    mar = c(4, 4, 3, 1))

# Boxplot sugli z-score
for (v in vars) {
  
  z <- as.numeric(scale(df[[v]]))  # standardizzazione corretta
  
  boxplot(z,
          main = var_labels[v],
          col = "#c7e9c0",
          border = "#238B45",
          outcol = "red",
          outpch = 19,
          ylab = "Valori standardizzati (z-score)",
          horizontal = FALSE)
  
  abline(h = 0, lty = 2, col = "gray40")
}

