df_gen <- read.csv(file.choose(), header = TRUE, sep = ",")

post_n_missing <- sapply(df_gen[numeric_vars], function(x) sum(is.na(x)))
post_missing_df_gen <- data.frame(var = names(post_n_missing), n_missing = as.integer(post_n_missing))
print(post_missing_df_gen)
View(df_gen)

#ricostruzione sc
df_gen$log_gdp_per_capita_sc <- as.numeric(scale(df_gen$log_gdp_per_capita))
df_gen$social_support_sc <- as.numeric(scale(df_gen$social_support))
df_gen$healthy_life_expectancy_at_birth_sc <- as.numeric(scale(df_gen$healthy_life_expectancy_at_birth))
df_gen$freedom_to_make_life_choices_sc <- as.numeric(scale(df_gen$freedom_to_make_life_choices))
df_gen$generosity_sc <- as.numeric(scale(df_gen$generosity))
df_gen$perceptions_of_corruption_sc <- as.numeric(scale(df_gen$perceptions_of_corruption))
df_gen$positive_affect_sc <- as.numeric(scale(df_gen$positive_affect))
df_gen$negative_affect_sc <- as.numeric(scale(df_gen$negative_affect))


sapply(df_gen[c(numeric_vars)], function(x) sum(is.na(x)))



#Statistica descrittiva
# Seleziona solo le variabili numeriche
num_df_gen <- df_gen[sapply(df_gen, is.numeric)]

# Calcolo delle statistiche descrittive:
# Media, deviazione standard, minimo e massimo per ciascuna variabile numerica
descrittive <- data.frame(
  Variabile = names(num_df_gen),
  Media = sapply(num_df_gen, mean, na.rm = TRUE),
  Deviazione_Standard = sapply(num_df_gen, sd, na.rm = TRUE),
  Minimo = sapply(num_df_gen, min, na.rm = TRUE),
  Massimo = sapply(num_df_gen, max, na.rm = TRUE)
) ##Qui però sono rimossi i NA

# Visualizza la tabella delle statistichee
print(descrittive)




# Calcolo media, min e max per paese
happiness_summary_gen <- aggregate(happiness_score ~ country, data = df_gen,
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
na_mat <- is.na(df_gen[, numeric_vars])

missing_by_country_initial <- aggregate(na_mat,
                                        by=list(country=df_gen$country),
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
legend("topleft",
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

media_annuale_gen <- aggregate(happiness_score ~ year, df_gen, mean)

ts_media_gen <- ts(media_annuale_gen$happiness_score,
               start = min(media_annuale_gen$year),
               end   = max(media_annuale_gen$year),
               frequency = 1)

plot(ts_media_gen,
     type = "o",
     pch = 19,
     col = "#238B45",
     xlab = "Anno",
     ylab = "Felicità (media annuale)",
     main = "Serie Temporale della Felicità")


trend <- lm(media_annuale_gen$happiness_score ~ media_annuale_gen$year)
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

# Ciclo per generare scatterplot + grafico dei residui
for (var in vars) {
  
  # Rimozione valori mancanti
  df_gen_plot <- df_gen[!is.na(df_gen[[var]]) & !is.na(df_gen$happiness_score), ]
  
  #scatterplot
  plot(df_gen_plot[[var]],
       df_gen_plot$happiness_score,
       main = paste("Relazione tra", var, "e punteggio di felicità generato"),
       xlab = var,
       ylab = "Punteggio di felicità",
       col = rgb(27/255, 158/255, 119/255, 0.4),
       pch = 16,
       cex = 0.5)
  
  # Modello lineare
  lm_model_gen <- lm(happiness_score ~ df_gen_plot[[var]], data = df_gen_plot)
  abline(lm_model_gen, col = "#00441b", lwd = 2, lty = 2)
  grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")
  
  # Calcolo e stampa informazioni sintetiche
  cat("lm model: \n")
  print(lm_model_gen$coefficients)
  cat("\n")
  
  cat("Media residui:\n")
  print(median(lm_model_gen$residuals))
  cat("\nVarianza residui:\n")
  print(var(lm_model_gen$residuals))
  cat("\nDeviazione standard residui:\n")
  print(sd(lm_model_gen$residuals))
  cat("\n")
  
  cor_val <- cor(df_gen_plot[[var]], df_gen_plot$happiness_score, use = "complete.obs")
  cov_val <- cov(df_gen_plot[[var]], df_gen_plot$happiness_score, use = "complete.obs")
  
  cat("Variabile:", var, "\n")
  cat("Correlazione (Pearson):", round(cor_val, 3), "\n\n")
  print(summary(lm_model_gen))
  
  #Grafico residui
#  maintext <- paste("Diagramma dei residui generato ", var)
#  maintext
#  plot(lm_model_gen$fitted.values, lm_model_gen$residuals,
#       main = maintext,
#       xlab = "Valori stimati (Medie)",
#       ylab = "Residui",
#       col = rgb(1, 0, 0, 0.6),
#       pch = 4,   # crocette
#       cex = 1.2)
  
#  abline(h = 0, col = "blue", lty = 2, lwd = 2)  # linea orizzontale a 0
#  grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")
}
