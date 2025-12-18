df_gen_nemotron <- read.csv(file.choose(), header = TRUE, sep = ",")
names(df_gen_nemotron) <- gsub(" ", "_", names(df_gen_nemotron))

post_n_missing <- sapply(df_gen_nemotron[numeric_vars], function(x) sum(is.na(x)))
post_missing_df_gen_nemotron <- data.frame(var = names(post_n_missing), n_missing = as.integer(post_n_missing))
print(post_missing_df_gen_nemotron)
View(df_gen_nemotron)

#ricostruzione sc
df_gen_nemotron$happiness_score_sc <- as.numeric(scale(df_gen_nemotron$happiness_score))
df_gen_nemotron$log_gdp_per_capita_sc <- as.numeric(scale(df_gen_nemotron$log_gdp_per_capita))
df_gen_nemotron$social_support_sc <- as.numeric(scale(df_gen_nemotron$social_support))
df_gen_nemotron$healthy_life_expectancy_at_birth_sc <- as.numeric(scale(df_gen_nemotron$healthy_life_expectancy_at_birth))
df_gen_nemotron$freedom_to_make_life_choices_sc <- as.numeric(scale(df_gen_nemotron$freedom_to_make_life_choices))
df_gen_nemotron$generosity_sc <- as.numeric(scale(df_gen_nemotron$generosity))
df_gen_nemotron$perceptions_of_corruption_sc <- as.numeric(scale(df_gen_nemotron$perceptions_of_corruption))
df_gen_nemotron$positive_affect_sc <- as.numeric(scale(df_gen_nemotron$positive_affect))
df_gen_nemotron$negative_affect_sc <- as.numeric(scale(df_gen_nemotron$negative_affect))


sapply(df_gen_nemotron[c(numeric_vars)], function(x) sum(is.na(x)))



#Statistica descrittiva
# Seleziona solo le variabili numeriche
num_df_gen_nemotron <- df_gen_nemotron[sapply(df_gen_nemotron, is.numeric)]

# Calcolo delle statistiche descrittive:
# Media, deviazione standard, minimo e massimo per ciascuna variabile numerica
descrittive <- data.frame(
  Variabile = names(num_df_gen_nemotron),
  Media = sapply(num_df_gen_nemotron, mean, na.rm = TRUE),
  Deviazione_Standard = sapply(num_df_gen_nemotron, sd, na.rm = TRUE),
  Minimo = sapply(num_df_gen_nemotron, min, na.rm = TRUE),
  Massimo = sapply(num_df_gen_nemotron, max, na.rm = TRUE)
) ##Qui però sono rimossi i NA

# Visualizza la tabella delle statistichee
print(descrittive)




# Calcolo media, min e max per paese
happiness_summary_gen <- aggregate(happiness_score ~ country, data = df_gen_nemotron,
                                   FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                                       min = min(x, na.rm = TRUE),
                                                       max = max(x, na.rm = TRUE)))

#Trasformazione in dataframe leggibile
happiness_summary_gen <- data.frame(
  country = happiness_summary_gen$country,
  mean = happiness_summary_gen$happiness_score[, "mean"],
  min = happiness_summary_gen$happiness_score[, "min"],
  max = happiness_summary_gen$happiness_score[, "max"]
)

#Selezione dei 50 paesi con felicità media più alta
top50 <- happiness_summary_gen[order(-happiness_summary_gen$mean), ][1:50, ]

#Imposta margini (più spazio per i nomi sotto)
par(mar = c(10, 5, 4, 2))   # margine inferiore aumentato

#Definizione dell'intervallo Y (nessun overflow)
y_min <- min(top50$min) - 0.2
y_max <- max(top50$max) + 0.5



numeric_vars <- c("log_gdp_per_capita",
                  "social_support",
                  "healthy_life_expectancy_at_birth",
                  "freedom_to_make_life_choices",
                  "generosity",
                  "perceptions_of_corruption",
                  "positive_affect",
                  "negative_affect"
)

# Numero totale di missing values per paese
na_mat <- is.na(df_gen_nemotron[, numeric_vars])

missing_by_country_initial <- aggregate(na_mat,
                                        by=list(country=df_gen_nemotron$country),
                                        FUN=sum)
missing_by_country_initial

# somma di missing values su tutte le variabili numeriche
missing_by_country_initial$total_missing <- rowSums(missing_by_country_initial[numeric_vars])

# ordine decrescente eliminando i paesi con 0 missing values
missing_by_country_initial <- missing_by_country_initial[missing_by_country_initial$total_missing > 0, ]
missing_by_country_initial <- missing_by_country_initial[order(-missing_by_country_initial$total_missing), ]

colors_green <- colorRampPalette(c("#00441b", "#238b45", "#74c476", "#c7e9c0", "#f7fcf5"))(50)

#barplot
bar_positions <- barplot(top50$mean,
                         names.arg = top50$country,
                         las = 2,
                         col = colors_green,
                         ylim = c(0, 11),
                         main = expression(bold("Top 50 Paesi per Felicità Media (2005–2023)")),
                         ylab = "Happiness Score",
                         cex.names = 0.8,
)

#Aggiungi linee per min e max
lines(bar_positions, top50$min, type = "o", col = "red", lwd = 2, pch = 19)
lines(bar_positions, top50$max, type = "o", col = "blue", lwd = 2, pch = 19)

#Legenda chiara e compatta
legend("topright",
       legend = c("Minimo", "Massimo"),
       col = c("red", "blue"),
       pch = 19,
       lty = 1,
       lwd = 2,
       bty = "n")




bottom50 <- happiness_summary_gen[order(happiness_summary_gen$mean), ][1:50, ]

# Imposta margini e intervallo Y
par(mar = c(10, 5, 4, 2))
y_min <- min(bottom50$min) - 0.2
y_max <- max(bottom50$max) + 0.5

# Barplot (dal più basso al più alto)
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


#Serie temporale della felicità

media_annuale_nemotron_gen <- aggregate(happiness_score ~ year, df_gen_nemotron, mean)

ts_media_gen <- ts(media_annuale_nemotron_gen$happiness_score,
                   start = min(media_annuale_nemotron_gen$year),
                   end   = max(media_annuale_nemotron_gen$year),
                   frequency = 1)

plot(ts_media_gen,
     type = "o",
     pch = 19,
     col = "#238B45",
     xlab = "Anno",
     ylab = "Felicità (media annuale)",
     main = "Serie Temporale della Felicità")


trend <- lm(media_annuale_nemotron_gen$happiness_score ~ media_annuale_nemotron_gen$year)
summary(trend)



#Scatterplots

vars <- c("log_gdp_per_capita",
          "social_support",
          "healthy_life_expectancy_at_birth",
          "freedom_to_make_life_choices",
          "generosity",
          "perceptions_of_corruption",
          "positive_affect",
          "negative_affect")

vars_sc
vars_sc %in% names(df_gen_nemotron)


# Ciclo per generare scatterplot + grafico dei residui
plot_scatter_gen <- function(df, vars) {
  
  par(mfrow = c(2, 4),       # 2 righe x 4 colonne
      mar = c(4, 4, 3, 1))   # margini compatti
  
  for (var in vars) {
    
    df_plot <- df[!is.na(df[[var]]) & !is.na(df$happiness_score), ]
    
    plot(df_plot[[var]],
         df_plot$happiness_score,
         main = var,
         xlab = var,
         ylab = "Happiness Score",
         col = rgb(27/255, 158/255, 119/255, 0.4),
         pch = 16,
         cex = 0.5)
    
    lm_model <- lm(happiness_score ~ df_plot[[var]], data = df_plot)
    abline(lm_model, col = "#00441b", lwd = 2, lty = 2)
    
    grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")
  }
  
  par(mfrow = c(1, 1))  # reset layout
}

plot_residuals_gen <- function(df, vars) {
  
  par(mfrow = c(2, 4),
      mar = c(4, 4, 3, 1))
  
  for (var in vars) {
    
    df_plot <- df[!is.na(df[[var]]) & !is.na(df$happiness_score), ]
    
    lm_model <- lm(happiness_score ~ df_plot[[var]], data = df_plot)
    
    plot(lm_model$fitted.values,
         lm_model$residuals,
         main = paste("Residui:", var),
         xlab = "Valori stimati",
         ylab = "Residui",
         col = rgb(1, 0, 0, 0.6),
         pch = 16,
         cex = 0.5)
    
    abline(h = 0, col = "blue", lty = 2, lwd = 2)
    grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")
  }
  
  par(mfrow = c(1, 1))
}

# Scatterplot (2x4)
plot_scatter_gen(df_gen_nemotron, vars)

# Grafici dei residui (2x4)
plot_residuals_gen(df_gen_nemotron, vars)



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

media_annuale_nemotron <- aggregate(
  df_gen_nemotron[, vars_sc],
  by = list(year = df_gen_nemotron$year),
  FUN = mean,
  na.rm = TRUE
)


plot(media_annuale_nemotron$year,
     media_annuale_nemotron$happiness_score_sc,
     type = "o",
     pch = 16,
     lwd = 2,
     col = "darkgreen",
     ylim = range(media_annuale_nemotron[, vars_sc]),
     xlab = "Anno",
     ylab = "Valori medi (standardizzati)",
     main = "Serie temporali delle variabili (2005–2022) nemotron")

lines(media_annuale_nemotron$year, media_annuale_nemotron$log_gdp_per_capita_sc,
      type = "o", pch = 16, col = "blue")

lines(media_annuale_nemotron$year, media_annuale_nemotron$social_support_sc,
      type = "o", pch = 16, col = "orange")

lines(media_annuale_nemotron$year, media_annuale_nemotron$generosity_sc,
      type = "o", pch = 16, col = "purple")

lines(media_annuale_nemotron$year, media_annuale_nemotron$positive_affect_sc,
      type = "o", pch = 16, col = "red")

lines(media_annuale_nemotron$year, media_annuale_nemotron$negative_affect_sc,
      type = "o", pch = 16, col = "brown")

lines(media_annuale_nemotron$year, media_annuale_nemotron$freedom_to_make_life_choices_sc,
      type = "o", pch = 16, col = "darkolivegreen")

lines(media_annuale_nemotron$year, media_annuale_nemotron$healthy_life_expectancy_at_birth_sc,
      type = "o", pch = 16, col = "darkcyan")

lines(media_annuale_nemotron$year, media_annuale_nemotron$perceptions_of_corruption_sc,
      type = "o", pch = 16, col = "grey40")

legend("topright",
       inset = c(0.35, 0),
       cex = (0.7),
       legend = c("Happiness Score",
                  "Log GDP",
                  "Social Support",
                  "Generosity",
                  "Positive Affect",
                  "Negative Affect",
                  "Freedom of Choice",
                  "Healthy Life Expectancy",
                  "Perception of Corruption"),
       col = c("darkgreen", "blue", "orange", "purple",
               "red", "brown", "darkolivegreen", "darkcyan", "grey40"),
       pch = c(16, 17, 15, 18, 16, 16, 17, 15, 18),
       lty = 1,
       lwd = 2,
       bty = "n")


