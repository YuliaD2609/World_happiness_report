#creazione dataframe
df <- read.csv(file.choose(), header = TRUE, sep = ",")

# Numero di colonne (attributi/variabili)
length(df)  

# Numero di righe e colonne (dimensioni del dataset)
dim(df)
# Nomi delle colonne per capire che variabili abbiamo
names(df)

# Struttura del dataframe: tipo di variabili (numeric, factor, character)
str(df)

# Visualizziamo alcune righe di esempio
head(df)

# Visualizzazione tabellare (apre una finestra in RStudio)
View(df)

# Esempio di riga specifica (la 10)
df[10,]

# Tutti i paesi unici presenti nel dataset
unique(df$country)

# Numero totale di paesi unici
length(unique(df$country))



# Verifica valori mancanti
colSums(is.na(df))
#La percezione di corruzione è palese da levare, generosity anche troppi.

# Statistiche descrittive, primo quartile, mediana, media, terzo quartile, min e max e relativi NA
summary(df)


# Suddividiamo le variabili in numeriche e categoriche
#Tutte sono numeriche eccetto country e cntry_code

# Variabili numeriche
numeric_vars <- names(df)[sapply(df, is.numeric)]
numeric_vars

# Variabili categoriche (fattori o caratteri)
categorical_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
categorical_vars





#Statistica descrittiva
# Seleziona solo le variabili numeriche
num_df <- df[sapply(df, is.numeric)]

# Calcolo delle statistiche descrittive:
# Media, deviazione standard, minimo e massimo per ciascuna variabile numerica
descrittive <- data.frame(
  Variabile = names(num_df),
  Media = sapply(num_df, mean, na.rm = TRUE),
  Deviazione_Standard = sapply(num_df, sd, na.rm = TRUE),
  Minimo = sapply(num_df, min, na.rm = TRUE),
  Massimo = sapply(num_df, max, na.rm = TRUE)
) ##Qui però sono rimossi i NA

# Visualizza la tabella delle statistichee
print(descrittive)

