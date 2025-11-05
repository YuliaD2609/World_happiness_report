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
colors_green <- colorRampPalette(c("#00441b", "#238b45", "#74c476", "#c7e9c0", "#f7fcf5" ))(nrow(missing_by_country_initial))

barplot(missing_by_country_initial$total_missing,
        names.arg = missing_by_country_initial$country,
        las = 2,
        main = "Totale valori mancanti per Paese",
        ylab = "Numero di NA",
        cex.names = 0.7,
        col = colors_green)








# 1. Creazione dataframe
df <- read.csv(file.choose(), header = TRUE, sep = ",")

# 2. Caricamento librerie
library(dplyr)
library(ggplot2)

# 3. Calcolo dei valori mancanti per colonna
missing_counts <- colSums(is.na(df))

# 4. Creazione del dataframe e rimozione delle colonne con "_sc"
missing_df <- data.frame(
  Variabile = names(missing_counts),
  Valori_Mancanti = missing_counts
) %>%
  filter(!grepl("_sc$", Variabile)) %>%       # <-- Esclude le colonne standardizzate
  arrange(desc(Valori_Mancanti))

# 5. Visualizzazione tabellare di controllo
print(missing_df)

# 6. Creazione del barplot
ggplot(missing_df, aes(x = reorder(Variabile, -Valori_Mancanti),
                       y = Valori_Mancanti,
                       fill = Valori_Mancanti)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Valori_Mancanti), vjust = -0.3, size = 3.5) +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  labs(title = "Distribuzione dei valori mancanti per variabile",
       x = "Variabile",
       y = "Numero di valori mancanti") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

