df <- read.csv(file.choose(), header = TRUE, sep = ",")

# Varianza campionaria
var <- var(df$happiness_score, na.rm = TRUE)
var

# Deviazione standard campionaria
sd_h <- sd(df$happiness_score, na.rm = TRUE)
sd_h

# Coefficiente di variazione
cv <- (sd_h / mean(df$happiness_score, na.rm = TRUE))
cv
