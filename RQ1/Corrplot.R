df <- read.csv(file.choose(), header = TRUE, sep = ",")

vars <- c("log_gdp_per_capita",
                  "social_support",
                  "positive_affect",
                  "negative_affect",
                  "healthy_life_expectancy_at_birth",
                  "freedom_to_make_life_choices",
                  "generosity",
                  "perceptions_of_corruption"
)

#matrice di correlazione
mat_corr <- cor(df[vars], use = "pairwise.complete.obs")
print(mat_corr)

# heatmap della correlazione
#install.packages("corrplot")
library(corrplot)
par(mar = c(1, 1, 8, 1))
corrplot(mat_corr, method = "color", type = "full", tl.srt = 30, tl.col = "black",  tl.cex = 0.6, main = "")
title("Heatmap della correlazione", line = 7)

#matrice di scatterplot
pairs(df[numeric_vars], cex = 0.2, pch = 20,
      main = "Scatterplot matrix delle variabili")

