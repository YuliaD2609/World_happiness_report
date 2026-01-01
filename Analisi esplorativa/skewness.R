df <- read.csv(file.choose(), header = TRUE, sep = ",")
# Skewness e curtosi per l'asimmetria
# install.packages("moments")
library(moments)
par(mfrow = c(1,1))
sk <- skewness(df$happiness_score, na.rm = TRUE)
cat("Skewness: " , sk)
kt <- kurtosis(df$happiness_score, na.rm = TRUE)
cat("Curtosi: " , kt)

library(ggplot2)

ggplot(df, aes(x = happiness_score)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "#238b45",
                 color = "black") +
  geom_density(linewidth = 1) +
  labs(
    title = "Distribuzione del punteggio di felicità",
    subtitle = paste("Skewness =", round(sk, 3)),
    x = "Happiness score",
    y = "Densità"
  ) +
  theme_minimal()

# Divisione per anno per l'asimmetria calcolata sugli anni
skew_by_year <- tapply(df$happiness_score, df$year, skewness, na.rm = TRUE)
cat("Skewness per anno: " , skew_by_year)
years <- as.numeric(names(skew_by_year))
plot(years, skew_by_year, type="b", pch=19, col="#238b45",
     xaxt="n",
     main="Skewness della felicità nel tempo",
     xlab="Anno", ylab="Skewness")

axis(1, at=years, labels=years, las=2, cex.axis=0.7)

boxplot(skew_by_year,
        main = "Boxplot della Skewness della felicità nel tempo",
        ylab = "Skewness",
        col = "#74c476",
        border = "#00441b",
        notch = TRUE)

summary(df$happiness_score)

