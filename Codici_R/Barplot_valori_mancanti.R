# ---------------------------------------------------------
# ANALISI DEI VALORI MANCANTI NEL DATASET
# ---------------------------------------------------------

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
  labs(title = "Distribuzione dei valori mancanti per variabile (solo originali)",
       x = "Variabile",
       y = "Numero di valori mancanti") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

