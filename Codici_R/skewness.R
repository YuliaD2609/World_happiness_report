df <- read.csv(file.choose(), header = TRUE, sep = ",")
#skewness e curtosi per l'asimmetria
#install.packages("moments")
#library(moments)

sk <- skewness(df$happiness_score, na.rm = TRUE)
kt <- kurtosis(df$happiness_score, na.rm = TRUE)
sk
kt

#divisione per anno per l'asimmetria calcolata sugli anni
skew_by_year <- tapply(df$happiness_score, df$year, skewness, na.rm = TRUE)
print(skew_by_year)
years <- as.numeric(names(skew_by_year))
plot(years, skew_by_year, type="b", pch=19, col="#238b45",
     xaxt="n",
     main="Skewness della felicità nel tempo",
     xlab="Anno", ylab="Skewness")

axis(1, at=years, labels=years, las=2, cex.axis=0.7)