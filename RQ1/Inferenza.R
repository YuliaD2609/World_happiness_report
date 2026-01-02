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
alpha <- 0.0001
n <- nrow(df_country)    # numero di paesi
sigma <- 0.53            # residual standard error
beta_true <- 0.30        # effetto reale (es. GDP)

# Distribuzioni
x <- seq(-4, 6, length=1000)

# H0: beta = 0
f_H0 <- dnorm(x, mean = 0, sd = sigma)

# H1: beta = beta_true
f_H1 <- dnorm(x, mean = beta_true, sd = sigma)

# Valore critico
z_alpha <- qnorm(1 - alpha/2)
x_int <- beta_true / 2
x_int

# Grafico
plot(x, f_H0, type="l", lwd=2, col="blue",
     ylab="Densità", xlab="Valore della stima",
     main="Compromesso tra errore di Tipo I e Tipo II")
lines(x, f_H1, lwd=2, col="red")

abline(v = alpha, col="black", lty=2)

polygon(c(x_int, x[x > x_int], max(x)),
        c(0, dnorm(x[x > x_int],0,sigma), 0),
        col=rgb(0,0,1,0.25), border=NA)

# Errore di Tipo II (β)
polygon(c(min(x), x[x < x_int], x_int),
        c(0, dnorm(x[x < x_int],beta_true,sigma), 0),
        col=rgb(1,0,0,0.25), border=NA)

legend("topright",
       legend=c("H0", "H1", "Regione critica"),
       col=c("blue","red","black"),
       lwd=c(2,2,1),
       lty=c(1,1,2))


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
  cat("\nVariabile:", v, "\n")
  df_tmp <- df_country[!is.na(df_country[[v]]) & !is.na(df_country$happiness_score), ]
  
  model <- lm(df_tmp$happiness_score ~ df_tmp[[v]])
  
  results[[v]] <- list(
    summary = summary(model),
    confint = confint(model, level = 0.95), # stima dell'incertezza
    residuals = residuals(model)
  )
  
  # Intervallo di confidenza
  ci <- confint(model, level = 0.95)[2,]
  cat("IC 95%: [", round(ci[1],3), ",", round(ci[2],3), "]\n")
  
  beta_hat <- results[[v]]$summary$coefficients[2,1]
  se <- results[[v]]$summary$coefficients[2,2]
  pval <- results[[v]]$summary$coefficients[2,4]
  ci <- results[[v]]$confint[2,]
  
  cat("Beta stimato:", round(beta_hat, 3), "\n")
  cat("Errore standard:", round(se, 3), "\n")
  cat("p-value:", round(pval, 5), "\n")
  cat("IC 95%: [", round(ci[1],3), ",", round(ci[2],3), "]\n")
}








# Parametri
alpha <- 0.05                 # errore di I tipo
delta <- 1                    # dimensione dell'effetto sotto H1

# Valori della statistica
z <- seq(-4, 6, length.out = 1000)

# Densità sotto H0 e H1
dens_h0 <- dnorm(z, mean = 0, sd = 1)
dens_h1 <- dnorm(z, mean = delta, sd = 1)

# Valore critico (test a una coda)
z_crit <- qnorm(1 - alpha)

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
       legend = c("H0", "H1", "Errore di tipo I (α)", "Errore di tipo II (β)"),
       col = c("blue", "red", rgb(0,0,1,0.25), rgb(1,0,0,0.25)),
       lty = c(1, 1, NA, NA),
       lwd = c(2, 1, NA, NA),
       pch = c(NA, NA, 15, 15),
       pt.cex = 2,
       bty = "n")

