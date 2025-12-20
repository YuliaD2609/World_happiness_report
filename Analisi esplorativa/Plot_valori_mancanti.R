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

# Numero totale di missing values per paese 
na_mat <- is.na(df[, numeric_vars]) 
missing_by_country_initial <- aggregate(na_mat, by=list(country=df$country), FUN=sum) 
cat("Numero di missing values per paese") 
missing_by_country_initial 
# Somma di missing values su tutte le variabili numeriche 
missing_by_country_initial$total_missing <- rowSums(missing_by_country_initial[numeric_vars]) 
# Ordine decrescente eliminando i paesi con 0 missing values 
missing_by_country_initial <- missing_by_country_initial[missing_by_country_initial$total_missing > 0, ] 
missing_by_country_initial <- missing_by_country_initial[order(-missing_by_country_initial$total_missing), ]

cols <- colorRampPalette(
  c("#00441b", "#238b45", "#74c476", "#c7e9c0")
)(nrow(missing_by_country_initial))



par(mfrow = c(1, 1))


# Barplot
bp <- barplot(missing_by_country_initial$total_missing,
              names.arg = missing_by_country_initial$country,
              las = 2,
              main = expression(bold("Totale valori mancanti per Paese")),
              ylab = "Numero di NA",
              ylim = c(0,26),
              cex.names = 0.7,
              col = cols)

# valori unici sopra le barre
unique_vals <- unique(missing_by_country_initial$total_missing)

for(val in unique_vals){
  idx <- which(missing_by_country_initial$total_missing == val)
  
  # Posizione centrale 
  central_idx <- idx[ceiling(length(idx)/2)]
  text(x = bp[central_idx],
       y = missing_by_country_initial$total_missing[central_idx] + 1,
       labels = val,
       cex = 0.7,
       col = "black")
}


# Calcolo del numero di NA per ogni colonna
missing_counts <- colSums(na_mat)

# Creazione dataframe
missing_df <- data.frame(
  Variabile = names(missing_counts),
  Valori_Mancanti = missing_counts
)

# Esclusione variabili standardizzate e ordinamento decrescente
missing_df <- missing_df[!grepl("_sc$", missing_df$Variabile), ]
missing_df <- missing_df[order(-missing_df$Valori_Mancanti), ]

var_labels <- c(
  perceptions_of_corruption = "Percezione della corruzione",
  generosity = "Generosità",
  healthy_life_expectancy_at_birth = "Aspettativa di vita sana",
  freedom_to_make_life_choices = "Libertà di scelta nella vita",
  positive_affect = "Emozioni positive",
  log_gdp_per_capita = "PIL pro capite",
  negative_affect = "Emozioni negative",
  social_support = "Supporto sociale"
)

cols <- colorRampPalette(
  c("#00441b", "#238b45", "#74c476", "#c7e9c0")
)(length(missing_by_country_initial))


# Barplot
bp <- barplot(missing_df$Valori_Mancanti,
              names.arg = NA,
              ylim = c(0, 150),
              col = cols,
              border = NA,
              main = "Distribuzione dei valori mancanti per variabile",
              ylab = "Numero di valori mancanti")


par(xpd = TRUE)
text(x = bp,
     y = par("usr")[3] - 10,           
     labels = var_labels,
     srt = 15,                         
     adj = 1,
     cex = 0.7)
par(xpd = FALSE)


text(x = bp,
     y = missing_df$Valori_Mancanti + 3,  
     labels = missing_df$Valori_Mancanti,
     cex = 0.8, 
     col = "black", 
     font = 2)



