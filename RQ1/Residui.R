df <- read.csv(file.choose(), header = TRUE, sep = ",")

vars <- c(
  "log_gdp_per_capita",
  "social_support",
  "healthy_life_expectancy_at_birth",
  "freedom_to_make_life_choices",
  "generosity",
  "perceptions_of_corruption",
  "positive_affect",
  "negative_affect"
)

var_labels <- c(
  log_gdp_per_capita = "PIL pro capite",
  social_support = "Supporto sociale",
  healthy_life_expectancy_at_birth = "Aspettativa di vita sana",
  freedom_to_make_life_choices = "Libertà di scelta nella vita",
  generosity = "Generosità",
  perceptions_of_corruption = "Percezione della corruzione",
  positive_affect = "Emozioni positive",
  negative_affect = "Emozioni negative"
)

par(mfrow = c(1, 1))

for (var in vars) {
  
  df_plot <- df[!is.na(df[[var]]) & !is.na(df$happiness_score), ]
  
  plot(df_plot[[var]],
       df_plot$happiness_score,
       main = paste("Relazione tra", var_labels[var], "e punteggio di felicità"),
       xlab = var_labels[var],
       ylab = "Punteggio di felicità",
       col = rgb(27/255, 158/255, 119/255, 0.4),
       pch = 16,
       cex = 0.5)
  
  lm_model <- lm(as.formula(paste("happiness_score ~", var)), data = df_plot)
  abline(lm_model, col = "#00441b", lwd = 2, lty = 2)
  grid(col = "gray80", lty = "dotted")
  
  cat("Variabile:", var, "\n")
  cat("Coefficienti:\n")
  print(coef(lm_model))
  
  cat("Mediana residui:", median(residuals(lm_model)), "\n")
  cat("Varianza residui:", var(residuals(lm_model)), "\n")
  cat("Deviazione standard residui:", sd(residuals(lm_model)), "\n")
  
  cor_val <- cor(df_plot[[var]], df_plot$happiness_score, use = "complete.obs")
  cat("Correlazione (Pearson):", round(cor_val, 3), "\n")
  
  print(summary(lm_model))
  
  plot(lm_model$fitted.values, residuals(lm_model),
       main = paste("Residui:", var_labels[var]),
       xlab = "Valori stimati",
       ylab = "Residui",
       col = rgb(1, 0, 0, 0.6),
       pch = 19,
       cex = 0.5)
  
  abline(h = 0, col = "blue", lty = 2, lwd = 2)
  grid(col = "gray80", lty = "dotted")
}


# Modello di regressione lineare
model <- lm(
  happiness_score ~ log_gdp_per_capita +
    social_support +
    healthy_life_expectancy_at_birth +
    freedom_to_make_life_choices +
    generosity +
    perceptions_of_corruption +
    positive_affect +
    negative_affect,
  data = df
)
# residual standard error
summary(model)$sigma

# Distribuzione normale
# stima parametri
res <- residuals(model)


# Sequenza di valori
x <- seq(min(res), max(res), length = 200)
mu <- mean(res)
sigma <- sd(res)

hist(res, prob=TRUE, col="lightgray", main="Residui")
lines(dnorm(x, mean(res), sd(res)), col="red", lwd=2)

# Densità normale
dn=dnorm(x, mu, sigma)
curve(dnorm(x,mean=0, sd =0.5) ,from=-4, to=4, xlab="x",
      ylab="f(x)",main="Distribuzione di densità normale", col = "blue")
curve(dnorm(x,mean=0, sd=1) ,from=-4, to=4, xlab="x",ylab="f(x)",
      add=TRUE,col="red")
curve(dnorm(x,mean=0, sd =1.5) ,from=-4, to=4, xlab="x",ylab="f(x)",
      add=TRUE,col="green")
legend("topright",
       legend = c("sigma=0,5", "sigma=1", "sigma=1,5"),
       col = c("blue", "red", "green"),
       lwd = 2,
       lty = c(1, 2))

# Funzione di distribuzione normale
fdn=pnorm(x, mu, sigma)
curve(pnorm(x,mean=0, sd =0.5) ,from=-4, to=4, xlab="x",
      ylab="f(x)",main="Funzione di distribuzione normale",col = "blue")
curve(pnorm(x,mean=0, sd=1) ,from=-4, to=4, xlab="x",ylab="f(x)",
      add=TRUE,col="red")
curve(pnorm(x,mean=0, sd =1.5) ,from=-4, to=4, xlab="x",ylab="f(x)",
      add=TRUE,col="green")
legend("bottomright",
       legend = c("sigma=0,5", "sigma=1", "sigma=1,5"),
       col = c("blue", "red", "green"),
       lwd = 2,
       lty = c(1, 2))



