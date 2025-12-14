df <- read.csv(file.choose(), header = TRUE, sep = ",")

vars <- c("log_gdp_per_capita",
          "social_support",
          "healthy_life_expectancy_at_birth",
          "freedom_to_make_life_choices",
          "generosity",
          "perceptions_of_corruption",
          "positive_affect",
          "negative_affect")

for (var in vars) {
  
  # Rimozione valori mancanti
  df_plot <- df[!is.na(df[[var]]) & !is.na(df$happiness_score), ]
  
  plot(df_plot[[var]],
       df_plot$happiness_score,
       main = paste("Relazione tra", var, "e punteggio di felicità"),
       xlab = var,
       ylab = "Punteggio di felicità",
       col = rgb(27/255, 158/255, 119/255, 0.4),
       pch = 16,
       cex = 0.5)
  
  # Modello lineare
  lm_model <- lm(happiness_score ~ df_plot[[var]], data = df_plot)
  abline(lm_model, col = "#00441b", lwd = 2, lty = 2)
  grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")
  
  # Calcolo e stampa informazioni sintetiche
  cat("lm model: \n")
  print(lm_model$coefficients)
  cat("\n")
  
  cat("Media residui:\n")
  print(median(lm_model$residuals))
  cat("\nVarianza residui:\n")
  print(var(lm_model$residuals))
  cat("\nDeviazione standard residui:\n")
  print(sd(lm_model$residuals))
  cat("\n")
  
  cor_val <- cor(df_plot[[var]], df_plot$happiness_score, use = "complete.obs")
  cov_val <- cov(df_plot[[var]], df_plot$happiness_score, use = "complete.obs")
  
  cat("Variabile:", var, "\n")
  cat("Correlazione (Pearson):", round(cor_val, 3), "\n\n")
  print(summary(lm_model))
  
  # Residui
  maintext <- paste("Diagramma dei residui ", var)
  maintext
  plot(lm_model$fitted.values, lm_model$residuals,
       main = maintext,
       xlab = "Valori stimati (Medie)",
       ylab = "Residui",
       col = rgb(1, 0, 0, 0.6),
       pch = 19,
       cex = 0.5)
  
  abline(h = 0, col = "blue", lty = 2, lwd = 2)  # linea orizzontale a 0
  grid(nx = NULL, ny = NULL, col = "gray80", lty = "dotted")
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



