df <- read.csv(file.choose(), header = TRUE, sep = ",")

Allvars <- c("log_gdp_per_capita",
          "social_support",
          "healthy_life_expectancy_at_birth",
          "freedom_to_make_life_choices",
          "generosity",
          "perceptions_of_corruption",
          "positive_affect",
          "negative_affect",
          "happiness_score")

df_country <- aggregate(df[, Allvars],
                        by = list(country = df$country),
                        FUN = mean)
df_country_cc <- df_country[complete.cases(df_country[, Allvars]), ]

# Parametri
alpha <- 0.05                 # errore di I tipo
delta <- 1                    # dimensione dell'effetto sotto H1
n <- nrow(df_country) 

# Valori della statistica
z <- seq(-4, 6, length.out = 1000)

# Densità sotto H0 e H1
dens_h0 <- dnorm(z, mean = 0, sd = 1)
dens_h1 <- dnorm(z, mean = delta, sd = 1)

# Valore critico (test a una coda)
z_crit <- qnorm(1 - alpha)
z_crit

# Grafico
plot(z, dens_h0, type = "l", lwd = 2, col = "blue",
     xlab = "Valore della statistica",
     ylab = "Densità di probabilità",
     main = "Compromesso tra errore di Tipo I e Tipo II")

lines(z, dens_h1, lwd = 2, col = "red")

# Area errore di tipo I (alpha)
z_alpha <- z[z >= z_crit]
polygon(c(z_alpha, rev(z_alpha)),
        c(dnorm(z_alpha, 0, 1), rep(0, length(z_alpha))),
        col = rgb(0,0,1,0.25),
        border = NA)

# Area errore di tipo II (beta)
z_beta <- z[z < z_crit]
polygon(c(z_beta, rev(z_beta)),
        c(dnorm(z_beta, delta, 1), rep(0, length(z_beta))),
        col = rgb(1,0,0,0.25),
        border = NA)

# Linea del valore critico
abline(v = z_crit, lwd = 2, lty = 3)

# Legenda
legend("topright",
       inset = 0.02,          # più vicina all'angolo
       cex = 0.75,            # rende la legenda più piccola
       x.intersp = 0.6,       # spazio orizzontale compatto
       y.intersp = 0.8,       # spazio verticale compatto
       legend = c("H0", "H1", "Errore di tipo I (α)", "Errore di tipo II (β)"),
       col = c("blue", "red", rgb(0,0,1,0.25), rgb(1,0,0,0.25)),
       lty = c(1, 1, NA, NA),
       lwd = c(2, 1, NA, NA),
       pch = c(NA, NA, 15, 15),
       pt.cex = 1.3,
       bty = "n")


vars <- c("log_gdp_per_capita",
          "social_support",
          "healthy_life_expectancy_at_birth",
          "freedom_to_make_life_choices",
          "generosity",
          "perceptions_of_corruption",
          "positive_affect",
          "negative_affect")

results <- list()

for (v in vars) {
  
  cat("\n Variabile:", v, "\n")
  
  df_tmp <- df_country_cc[, c(v, "happiness_score")]
  
  model <- lm(happiness_score ~ df_tmp[[v]], data = df_tmp)
  summ <- summary(model)
  
  beta_hat <- summ$coefficients[2, 1]
  se <- summ$coefficients[2, 2]
  t_stat <- summ$coefficients[2, 3]
  p_bilateral <- summ$coefficients[2, 4]
  
  # p-value unilaterale (direzione positiva)
  p_unilateral <- ifelse(beta_hat > 0,
                         p_bilateral / 2,
                         1 - p_bilateral / 2)
  
  ci <- confint(model, level = 0.95)[2, ]
  
  cat("Beta stimato:", round(beta_hat, 3), "\n")
  cat("Errore standard:", round(se, 3), "\n")
  cat("Statistica t:", round(t_stat, 3), "\n")
  cat("p-value (bilaterale):", round(p_bilateral, 5), "\n")
  cat("p-value (unilaterale):", round(p_unilateral, 5), "\n")
  cat("IC 95%: [", round(ci[1],3), ",", round(ci[2],3), "]\n")
  
  if (p_bilateral < alpha) {
    cat("Effetto statisticamente significativo\n")
  } else {
    cat("Effetto NON significativo\n")
  }
  
  results[[v]] <- list(
    model = model,
    summary = summ,
    ci = ci
  )
}
