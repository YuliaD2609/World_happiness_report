df <- read.csv(file.choose(), header = TRUE, sep = ",")

# Varianza campionaria
var <- var(df$happiness_score, na.rm = TRUE)
var

# Deviazione standard campionaria
sd <- sd(df$happiness_score, na.rm = TRUE)
sd

# Coefficiente di variazione
cv <- (sd_h / mean(df$happiness_score, na.rm = TRUE))
cv