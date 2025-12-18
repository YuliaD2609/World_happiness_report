df <- read.csv(file.choose(), header = TRUE, sep = ",")

vars_sc <- c("log_gdp_per_capita",
             "social_support",
             "positive_affect",
             "negative_affect",
             "healthy_life_expectancy_at_birth",
             "freedom_to_make_life_choices",
             "generosity",
             "perceptions_of_corruption")

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


apply(df[vars_sc], 2, function(x) sum(abs(scale(x)) > 3, na.rm = TRUE))

outliers_list <- lapply(vars_sc, function(v) which(abs(scale(df[[v]])) > 3))
names(outliers_list) <- vars_sc
outliers_list

lapply(vars_sc, function(v) df[which(abs(scale(df[[v]])) > 3), c("country","year",v)])

par(mfrow = c(2, 4),  # 2 righe x 4 colonne
    mar = c(4, 4, 3, 1))

for (v in vars_sc) {
  boxplot(df[[v]],
          main = var_labels[v],
          col = "#c7e9c0",
          border = "#238B45",
          outcol = "red",
          outpch = 19,
          ylab = "Valori standardizzati",
          horizontal = FALSE)
  
  abline(h = 0, lty = 2, col = "gray40")
}
