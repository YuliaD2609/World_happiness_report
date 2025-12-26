df <- read.csv(file.choose(), header = TRUE, sep = ",")

var_labels <- c(
  log_gdp_per_capita_sc = "PIL pro capite",
  social_support_sc = "Supporto sociale",
  positive_affect_sc = "Emozioni positive",
  negative_affect_sc = "Emozioni negative",
  healthy_life_expectancy_at_birth_sc = "Aspettativa di vita sana",
  freedom_to_make_life_choices_sc = "Libertà di scelta nella vita",
  generosity_sc = "Generosità",
  perceptions_of_corruption_sc = "Percezione della corruzione"
)

# Media annuale della felicità
media_annuale <- aggregate(happiness_score ~ year, df, mean)

ts_media <- ts(media_annuale$happiness_score,
               start = min(media_annuale$year),
               end   = max(media_annuale$year),
               frequency = 1)

plot(ts_media,
     type = "o",
     pch = 19,
     col = "#238B45",
     xlab = "Anno",
     ylab = "Felicità (media annuale)",
     main = "Serie Temporale della Felicità")


trend <- lm(media_annuale$happiness_score ~ media_annuale$year)
summary(trend)

#multivariata
vars_sc <- c(
  "happiness_score_sc",
  "log_gdp_per_capita_sc",
  "social_support_sc",
  "generosity_sc",
  "positive_affect_sc",
  "negative_affect_sc",
  "freedom_to_make_life_choices_sc",
  "healthy_life_expectancy_at_birth_sc",
  "perceptions_of_corruption_sc"
)

media_annuale <- aggregate(
  df[, vars_sc],
  by = list(year = df$year),
  FUN = mean,
  na.rm = TRUE
)

plot(media_annuale$year,
     media_annuale$happiness_score_sc,
     type = "o",
     pch = 16,
     lwd = 2,
     col = "darkgreen",
     ylim = range(media_annuale[, vars_sc]),
     xlab = "Anno",
     ylab = "Valori medi (standardizzati)",
     main = "Serie temporali delle variabili (2005–2022)")

lines(media_annuale$year, media_annuale$log_gdp_per_capita_sc,
      type = "o", pch = 16, col = "blue")

lines(media_annuale$year, media_annuale$social_support_sc,
      type = "o", pch = 16, col = "orange")

lines(media_annuale$year, media_annuale$generosity_sc,
      type = "o", pch = 16, col = "purple")

lines(media_annuale$year, media_annuale$positive_affect_sc,
      type = "o", pch = 16, col = "red")

lines(media_annuale$year, media_annuale$negative_affect_sc,
      type = "o", pch = 16, col = "brown")

lines(media_annuale$year, media_annuale$freedom_to_make_life_choices_sc,
      type = "o", pch = 16, col = "darkolivegreen")

lines(media_annuale$year, media_annuale$healthy_life_expectancy_at_birth_sc,
      type = "o", pch = 16, col = "darkcyan")

lines(media_annuale$year, media_annuale$perceptions_of_corruption_sc,
      type = "o", pch = 16, col = "grey40")

legend("topright",
       inset = c(0.35, 0),
       cex = (1),
       legend = var_labels,
       col = c("darkgreen", "blue", "orange", "purple",
               "red", "brown", "darkolivegreen", "darkcyan", "grey40"),
       pch = c(16, 17, 15, 18, 16, 16, 17, 15, 18),
       lty = 1,
       lwd = 2,
       bty = "n")

