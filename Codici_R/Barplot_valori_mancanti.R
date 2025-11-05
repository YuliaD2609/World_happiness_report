df <- read.csv(file.choose(), header = TRUE, sep = ",")

numeric_vars <- c("log_gdp_per_capita",
                  "social_support",
                  "healthy_life_expectancy_at_birth",
                  "freedom_to_make_life_choices",
                  "generosity",
                  "perceptions_of_corruption",
                  "positive_affect",
                  "negative_affect"
)

# Numero totale di NA per paese
na_mat <- is.na(df[, numeric_vars])

missing_by_country_initial <- aggregate(na_mat,
                                        by=list(country=df$country),
                                        FUN=sum)
missing_by_country_initial

# somma NA su tutte le variabili numeriche
missing_by_country_initial$total_missing <- rowSums(missing_by_country_initial[numeric_vars])

# ordine decrescente eliminando i paesi con 0 valori mancanti
missing_by_country_initial <- missing_by_country_initial[missing_by_country_initial$total_missing > 0, ]
missing_by_country_initial <- missing_by_country_initial[order(-missing_by_country_initial$total_missing), ]

#scala di colori
colors_green <- colorRampPalette(c("#00441b", "#238b45", "#74c476", "#c7e9c0", "#f7fcf5"))(nrow(missing_by_country_initial))

#barplot
bp <- barplot(missing_by_country_initial$total_missing,
              names.arg = missing_by_country_initial$country,
              las = 2,
              main = expression(bold("Totale valori mancanti per Paese")),
              ylab = "Numero di NA",
              cex.names = 0.7,
              col = colors_green)

# valori unici sopra le barre
unique_vals <- unique(missing_by_country_initial$total_missing)

for(val in unique_vals){
  idx <- which(missing_by_country_initial$total_missing == val)
  
  # posizione centrale 
  central_idx <- idx[ceiling(length(idx)/2)]
  
  # numero sulla barra centrale
  text(x = bp[central_idx],
       y = missing_by_country_initial$total_missing[central_idx] + 1,
       labels = val,
       cex = 0.4,
       col = "black")
}




# 1. Calcolo del numero di NA per ogni colonna
missing_counts <- colSums(is.na(df))

# 2. Creazione di un dataframe
missing_df <- data.frame(
  Variabile = names(missing_counts),
  Valori_Mancanti = missing_counts
)

# 3. Esclusione delle variabili standardizzate (quelle con "_sc")
missing_df <- missing_df[!grepl("_sc$", missing_df$Variabile), ]

# 4. Ordinamento decrescente
missing_df <- missing_df[order(-missing_df$Valori_Mancanti), ]

# 5. Scala di colori verde
colors_green <- colorRampPalette(c("#00441b", "#238b45", "#74c476", "#c7e9c0", "#f7fcf5"))(nrow(missing_df))

# 6. Creazione barplot e salvataggio delle posizioni
bp <- barplot(missing_df$Valori_Mancanti,
              names.arg = NA,          # disattiva nomi di default
              col = colors_green,
              border = NA,
              main = "Distribuzione dei valori mancanti per variabile",
              ylab = "Numero di valori mancanti")
# 7. Etichette inclinate sotto le colonne
par(xpd = TRUE)
text(x = bp,
     y = par("usr")[3] - max(missing_df$Valori_Mancanti) * 0.05,  # piccolo margine sotto l’asse
     labels = missing_df$Variabile,
     srt = 45,          # rotazione di 45°
     adj = 1,
     cex = 0.7)
par(xpd = FALSE)

# 8. Aggiunta dei valori numerici sopra le colonne
text(x = bp,
     y = missing_df$Valori_Mancanti,
     labels = missing_df$Valori_Mancanti,
     pos = 3, cex = 0.8, col = "black", font = 2)

