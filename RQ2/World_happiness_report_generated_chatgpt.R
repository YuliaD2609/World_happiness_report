df_gen_chatgpt <- read.csv(file.choose(), header = TRUE, sep = ",")
names(df_gen_chatgpt) <- gsub(" ", "_", names(df_gen_chatgpt))
par(mfrow = c(1,1))

vars_sc <- c("log_gdp_per_capita_sc",
             "social_support_sc",
             "positive_affect_sc",
             "negative_affect_sc",
             "healthy_life_expectancy_at_birth_sc",
             "freedom_to_make_life_choices_sc",
             "generosity_sc",
             "perceptions_of_corruption_sc"
)

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

# Variabili numeriche
numeric_vars <- names(df_gen_chatgpt)[sapply(df_gen_chatgpt, is.numeric)]
numeric_vars

# Variabili categoriche (fattori o caratteri)
categorical_vars <- names(df_gen_chatgpt)[sapply(df_gen_chatgpt, function(x) is.factor(x) || is.character(x))]
categorical_vars

vars_sc
vars_sc %in% names(df_gen_chatgpt)

post_n_missing <- sapply(df_gen_chatgpt[numeric_vars], function(x) sum(is.na(x)))
post_missing_df_gen_chatgpt <- data.frame(var = names(post_n_missing), n_missing = as.integer(post_n_missing))
print(post_missing_df_gen_chatgpt)
View(df_gen_chatgpt)

#ricostruzione sc
df_gen_chatgpt$happiness_score_sc <- as.numeric(scale(df_gen_chatgpt$happiness_score))
df_gen_chatgpt$log_gdp_per_capita_sc <- as.numeric(scale(df_gen_chatgpt$log_gdp_per_capita))
df_gen_chatgpt$social_support_sc <- as.numeric(scale(df_gen_chatgpt$social_support))
df_gen_chatgpt$healthy_life_expectancy_at_birth_sc <- as.numeric(scale(df_gen_chatgpt$healthy_life_expectancy_at_birth))
df_gen_chatgpt$freedom_to_make_life_choices_sc <- as.numeric(scale(df_gen_chatgpt$freedom_to_make_life_choices))
df_gen_chatgpt$generosity_sc <- as.numeric(scale(df_gen_chatgpt$generosity))
df_gen_chatgpt$perceptions_of_corruption_sc <- as.numeric(scale(df_gen_chatgpt$perceptions_of_corruption))
df_gen_chatgpt$positive_affect_sc <- as.numeric(scale(df_gen_chatgpt$positive_affect))
df_gen_chatgpt$negative_affect_sc <- as.numeric(scale(df_gen_chatgpt$negative_affect))

sapply(df_gen_chatgpt[c(numeric_vars)], function(x) sum(is.na(x)))


#Statistica descrittiva
# Seleziona solo le variabili numeriche
num_df_gen_chatgpt <- df_gen_chatgpt[sapply(df_gen_chatgpt, is.numeric)]

# Calcolo delle statistiche descrittive:
# Media, deviazione standard, minimo e massimo per ciascuna variabile numerica
descrittive <- data.frame(
  Variabile = names(num_df_gen_chatgpt),
  Media = sapply(num_df_gen_chatgpt, mean, na.rm = TRUE),
  Deviazione_Standard = sapply(num_df_gen_chatgpt, sd, na.rm = TRUE),
  Minimo = sapply(num_df_gen_chatgpt, min, na.rm = TRUE),
  Massimo = sapply(num_df_gen_chatgpt, max, na.rm = TRUE)
) ##Qui però sono rimossi i NA

# Visualizza la tabella delle statistichee
print(descrittive)




# Calcolo media, min e max per paese
happiness_summary_gen <- aggregate(happiness_score ~ country, data = df_gen_chatgpt,
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
na_mat <- is.na(df_gen_chatgpt[, numeric_vars])

missing_by_country_initial <- aggregate(na_mat,
                                        by=list(country=df_gen_chatgpt$country),
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
                         ylab = "Punteggio della felicità",
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
                         ylab = "Punteggio della felicità",
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

media_annuale_chatgpt_gen <- aggregate(happiness_score ~ year, df_gen_chatgpt, mean)

ts_media_gen <- ts(media_annuale_chatgpt_gen$happiness_score,
               start = min(media_annuale_chatgpt_gen$year),
               end   = max(media_annuale_chatgpt_gen$year),
               frequency = 1)

plot(ts_media_gen,
     type = "o",
     pch = 19,
     col = "#238B45",
     xlab = "Anno",
     ylab = "Felicità (media annuale)",
     main = "Serie Temporale della Felicità")


trend <- lm(media_annuale_chatgpt_gen$happiness_score ~ media_annuale_chatgpt_gen$year)
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
plot_scatter_gen <- function(df, vars, var_labels) {
  par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))
  for (var in vars) {
    lab <- var_labels[[var]]
    df_plot <- df[!is.na(df[[var]]) & !is.na(df$happiness_score), ]
    plot(
      df_plot[[var]],
      df_plot$happiness_score,
      main = lab,
      xlab = lab,
      ylab = "Punteggio della felicità",
      col = rgb(27/255, 158/255, 119/255, 0.4),
      pch = 16,
      cex = 0.5
    )
    lm_model <- lm(happiness_score ~ df_plot[[var]], data = df_plot)
    abline(lm_model, col = "#00441b", lwd = 2, lty = 2)
    grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")
  }
  par(mfrow = c(1, 1))
}

plot_residuals_gen <- function(df, vars, var_labels) {
  par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))
  for (var in vars) {
    lab <- var_labels[[var]]
    df_plot <- df[!is.na(df[[var]]) & !is.na(df$happiness_score), ]
    lm_model <- lm(happiness_score ~ df_plot[[var]], data = df_plot)
    plot(
      lm_model$fitted.values,
      lm_model$residuals,
      main = paste("Residui:", lab),
      xlab = "Valori stimati",
      ylab = "Residui",
      col = rgb(1, 0, 0, 0.6),
      pch = 16,
      cex = 0.5
    )
    abline(h = 0, col = "blue", lty = 2, lwd = 2)
    grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")
  }
  par(mfrow = c(1, 1))
}

# Scatterplot (2x4)
plot_scatter_gen(df_gen_chatgpt, vars, var_labels)

# Grafici dei residui (2x4)
plot_residuals_gen(df_gen_chatgpt, vars, var_labels)



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

media_annuale_chatgpt <- aggregate(
  df_gen_chatgpt[, vars_sc],
  by = list(year = df_gen_chatgpt$year),
  FUN = mean,
  na.rm = TRUE
)


plot(media_annuale_chatgpt$year,
     media_annuale_chatgpt$happiness_score_sc,
     type = "o",
     pch = 16,
     lwd = 2,
     col = "darkgreen",
     ylim = range(media_annuale_chatgpt[, vars_sc]),
     xlab = "Anno",
     ylab = "Valori medi (standardizzati)",
     main = "Serie temporali delle variabili (2005–2022) chatgpt")

lines(media_annuale_chatgpt$year, media_annuale_chatgpt$log_gdp_per_capita_sc,
      type = "o", pch = 16, col = "blue")

lines(media_annuale_chatgpt$year, media_annuale_chatgpt$social_support_sc,
      type = "o", pch = 16, col = "orange")

lines(media_annuale_chatgpt$year, media_annuale_chatgpt$generosity_sc,
      type = "o", pch = 16, col = "purple")

lines(media_annuale_chatgpt$year, media_annuale_chatgpt$positive_affect_sc,
      type = "o", pch = 16, col = "red")

lines(media_annuale_chatgpt$year, media_annuale_chatgpt$negative_affect_sc,
      type = "o", pch = 16, col = "brown")

lines(media_annuale_chatgpt$year, media_annuale_chatgpt$freedom_to_make_life_choices_sc,
      type = "o", pch = 16, col = "darkolivegreen")

lines(media_annuale_chatgpt$year, media_annuale_chatgpt$healthy_life_expectancy_at_birth_sc,
      type = "o", pch = 16, col = "darkcyan")

lines(media_annuale_chatgpt$year, media_annuale_chatgpt$perceptions_of_corruption_sc,
      type = "o", pch = 16, col = "grey40")

legend("topright",
       inset = c(0.35, 0),
       cex = (1),
       legend = c("Punteggio della Felicità",
                  "PIL pro capite",
                  "Supporto sociale",
                  "Generosità",
                  "Emozioni positive",
                  "Emnozioni negative",
                  "Libertà di scelta nella vita",
                  "Aspettativa di vita sana",
                  "Percezione della corruzione"),
       col = c("darkgreen", "blue", "orange", "purple",
               "red", "brown", "darkolivegreen", "darkcyan", "grey40"),
       pch = c(16, 16, 16, 16, 16, 16, 16, 16, 16),
       lty = 1,
       lwd = 2,
       bty = "n")



# Frequenza assoluta
freq_ass_chatgpt <- table(cut(df_gen_chatgpt$happiness_score, breaks = 40, right = FALSE))
cat("Frequenza assoluta: ", freq_ass_chatgpt)
# Frequenza relativa
freq_rel_chatgpt <- prop.table(freq_ass_chatgpt)
cat("Frequenza relativa: ", freq_rel_chatgpt)
# Frequenza relativa cumulata
freq_rel_cum_chatgpt <- cumsum(freq_rel_chatgpt)
cat("Frequenza relativa cumulata: ", freq_rel_cum_chatgpt)

# Minimo, media, mediana, quantili
summary(df_gen_chatgpt$happiness_score)

par(mgp = c(4, 0, -1))

barplot(freq_ass_chatgpt,
        main = "Distribuzione di frequenza assoluta del punteggio di felicità chatgpt",
        xlab = "Classi di felicità",
        ylab = "Frequenza assoluta",
        las = 2,
        ylim = c(-10,220),
        col = "#56B117")

barplot(freq_rel_chatgpt,
        main = "Distribuzione di frequenza relativa del punteggio di felicità chatgpt",
        xlab = "Classi di felicità",
        ylab = "Frequenza relativa",
        las = 2,
        col = "#56B117")

par(mgp = c(3, 0.5, 0))

plot(freq_rel_cum_chatgpt,
     type = "b",
     pch = 19,
     col = "#238B45",
     xaxt = "n",
     xlab = "Classi di felicità",
     ylab = "Frequenza relativa cumulata",
     main = "Funzione di distribuzione empirica continua della Felicità chatgpt")



axis(1, at = 1:length(freq_rel_cum_chatgpt), labels = names(freq_rel_cum_chatgpt), las = 2, cex.axis = 0.6)


# Istogramma
hist_data_chatgpt <- hist(df_gen_chatgpt$happiness_score,
                  breaks = 40,                         
                  col = "#56B117",                     
                  border = "white",                    
                  main = "Distribuzione della variabile Happiness Score chatgpt",
                  xlab = "Punteggio di felicità",
                  ylab = "Frequenza",
                  cex.main = 1.3,
                  cex.lab = 1,
                  cex.axis = 0.9)

# Aggiunta curva 
dens_chatgpt <- density(df_gen_chatgpt$happiness_score)
scale_factor_chatgpt <- max(hist_data_chatgpt$counts) / max(dens_chatgpt$y)     

lines(dens_chatgpt$x, dens_chatgpt$y * scale_factor_chatgpt, 
      col = "red", 
      lwd = 2)

legend("topright",
       legend = c("Istogramma", "Densità"),
       fill = c("#56B117", NA),
       border = c("white", NA),
       lty = c(NA, 1),
       col = c("black", "red"),
       bty = "n",
       cex = 0.9)


# Kernel density plot

dens_chatgpt <- density(df_gen_chatgpt$happiness_score)

plot(dens_chatgpt,
     main = "Stima kernel density plot di felicità chatgpt",
     xlab = "Felicità",
     ylab = "Densità",
     lwd = 2,
     col = "#238B45")

polygon(dens_chatgpt,
        col = rgb(35/255, 139/255, 69/255, 0.3),
        border = "#238B45")





library(dplyr)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tmap)

# Calcolo media felicità per paese


library(dplyr)

happiness_mean_chatgpt <- df_gen_chatgpt %>%
  mutate(country = case_when(
    country == "Bosnia and Herzegovina" ~ "Bosnia and Herz.",
    country == "Central African Republic" ~ "Central African Rep.",
    country == "Congo (Brazzaville)" ~ "Congo",
    country == "Congo (Kinshasa)" ~ "Dem. Rep. Congo",
    country == "Dominican Republic" ~ "Dominican Rep.",
    country == "Eswatini" ~ "eSwatini",
    country == "Hong Kong S.A.R. of China" ~ "Hong Kong",
    country == "Ivory Coast" ~ "Côte d'Ivoire",
    country == "State of Palestine" ~ "Palestine",
    country == "Taiwan Province of China" ~ "Taiwan",
    country == "Turkiye" ~ "Turkey",
    country == "United States" ~ "United States of America",
    TRUE ~ country
  ))

happiness_mean_chatgpt <- happiness_mean_chatgpt %>%
  group_by(country) %>%
  summarise(mean_happiness = mean(happiness_score, na.rm = TRUE))


world <- ne_countries(scale = "medium", returnclass = "sf")

# Join tra la geometria e la media calcolata
world_happiness_chatgpt <- world %>%
  left_join(happiness_mean_chatgpt, by = c("name" = "country"))

setdiff(happiness_mean_chatgpt$country, world$name)

# Mappa statica
tmap_mode("plot") 
map_static_chatgpt <- tm_shape(world_happiness_chatgpt) +
  tm_polygons(
    col = "mean_happiness",
    palette = "YlGnBu",
    title = "Media Felicità",
    colorNA = "lightgray",
    textNA = "Nessun dato"
  ) +
  tm_layout(
    legend.title.size = 0.7,
    legend.text.size = 0.55,
    frame = FALSE,
    legend.position = c("left", "center")
  )

map_static_chatgpt 


