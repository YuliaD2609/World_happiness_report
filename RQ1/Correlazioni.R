# Lettura dati
df <- read.csv(file.choose(), header = TRUE, sep = ",")

# correlazione
# Variabili
vars <- c("log_gdp_per_capita",
          "freedom_to_make_life_choices",
          "positive_affect",
          "social_support",
          "healthy_life_expectancy_at_birth",
          "generosity",
          "perceptions_of_corruption",
          "negative_affect")

# Data frame risultati
cor_results <- data.frame(
  Variabile = character(),
  Correlazione = numeric(),
  Segno = character(),
  stringsAsFactors = FALSE
)

for (v in vars) {
  
  df_tmp <- df[!is.na(df[[v]]) & !is.na(df$happiness_score), ]
  
  cor_val <- cor(df_tmp[[v]],
                 df_tmp$happiness_score,
                 method = "pearson")
  
  segno <- ifelse(cor_val > 0, "Positiva", "Negativa")
  
  cor_results <- rbind(cor_results,
                       data.frame(Variabile = v,
                                  Correlazione = round(cor_val, 3),
                                  Segno = segno))
}

cor_results

cov_results <- data.frame(
  Variabile = character(),
  Covarianza = numeric(),
  Segno = character(),
  stringsAsFactors = FALSE
)

for (v in vars) {
  
  df_tmp <- df[!is.na(df[[v]]) & !is.na(df$happiness_score), ]
  
  cov_val <- cov(df_tmp[[v]],
                 df_tmp$happiness_score)
  
  segno <- ifelse(cov_val > 0, "Positiva", "Negativa")
  
  cov_results <- rbind(cov_results,
                       data.frame(Variabile = v,
                                  Covarianza = round(cov_val, 3),
                                  Segno = segno))
}

cov_results

# Diagramma di Pareto
#install.packages("qcc")
library(qcc)

par(mfrow = c(1, 1))
    

# Valori assoluti delle correlazioni
pareto_cor <- abs(cor_results$Correlazione)
names(pareto_cor) <- cor_results$Variabile

# Diagramma di Pareto
pareto.chart(
  pareto_cor,
  ylab = "Influenza (|correlazione|)",
  main = "Diagramma di Pareto – Influenza delle variabili sulla felicità",
  col = "darkgreen",
  cex.names = 0.8
)



