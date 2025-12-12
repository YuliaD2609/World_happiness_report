df <- read.csv(file.choose(), header = TRUE, sep = ",")

vars_sc <- c("log_gdp_per_capita_sc",
             "social_support_sc",
             "positive_affect_sc",
             "negative_affect_sc",
             "healthy_life_expectancy_at_birth_sc",
             "freedom_to_make_life_choices_sc",
             "generosity_sc",
             "perceptions_of_corruption_sc",
             "happiness_score")

# senza valori mancanti
X <- df[vars_sc]
X_complete <- X[complete.cases(X), ]
# matrice trasposta
X_t <- t(X_complete)

# matrice di correlazione tra variabili
mat_corr <- cor(X_t, use = "pairwise.complete.obs", method = "pearson")

# clustering gerarchico sulle variabili
dist_var <- dist(mat_corr, method = "euclidean")
hc_var <- hclust(dist_var, method = "ward.D2")

# dendrogramma
plot(hc_var, main = "Cluster delle variabili (in base alla correlazione con la felicità)",
     xlab = "Variabili", sub = "", cex = 0.9)

# taglio in 3 cluster
clusters_var <- cutree(hc_var, k = 3)
print(clusters_var)

# heatmap per visualizzare la correlazione tra le variabili
library(pheatmap)
pheatmap(mat_corr,
         main = "Heatmap di correlazione tra le variabili",
         color = colorRampPalette(c("white", "#74c476", "#00441b"))(100),
         display_numbers = TRUE,
         number_format = "%.2f")
