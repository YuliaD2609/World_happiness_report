#creazione dataframe
df <- read.csv(file.choose(), header = TRUE, sep = ",")

#Numero di colonne DF, brava yu complimenti pochissime
length(df)

dim(df)

#quali sono queste colonne
names(df)

#Ad esempio vediamo la riga 10 cosa possiamo ottenere
df[10,]

#in generale
head(df)

View(df)

#country
unique(df$country)

# Verifica valori mancanti
colSums(is.na(df))
#La percezione di corruzione è palese da levare, generosity anche troppi.

# Statistiche descrittive, primo quartile, mediana, media, terzo quartile, min e max e relativi NA
summary(df)


