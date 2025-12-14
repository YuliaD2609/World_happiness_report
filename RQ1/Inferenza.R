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
alpha <- 0.05
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

abline(v = x_int, col="black", lty=2)

# Area alpha
x_alpha <- x[x > x_int]
polygon(c(x_int, x_alpha, max(x)),
        c(0, dnorm(x_alpha,0,sigma), 0),
        col=rgb(0,0,1,0.2), border=NA)

# Area beta
x_beta <- x[x < x_int]
polygon(c(min(x), x_beta, x_int),
        c(0, dnorm(x_beta,beta_true,sigma), 0),
        col=rgb(1,0,0,0.2), border=NA)

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
  
  df_tmp <- df_country[!is.na(df_country[[v]]) & !is.na(df_country$happiness_score), ]
  
  model <- lm(df_tmp$happiness_score ~ df_tmp[[v]])
  
  results[[v]] <- list(
    summary = summary(model),
    confint = confint(model, level = 0.95),
    residuals = residuals(model)
  )
}
