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

X <- df[vars_sc]

# matrice delle distanze
dist_euclidea <- dist(X, method = "euclidean")
dist_euclidea[1:10] #si stampano solo le prime 10 per avere una visione iniziale dei valori
mat_dist <- as.matrix(dist_euclidea)

# matrice di similarità
mat_similarity <- 1 - mat_dist / max(mat_dist)
round(mat_similarity[1:5, 1:5], 3)

# matrice di correlazione con Pearson
mat_corr <- cor(X, use = "pairwise.complete.obs", method = "pearson")

# clustering gerarchico
hc_var <- hclust(dist_euclidea, method = "ward.D2")

# clusters
clusters_var <- cutree(hc_var, k = 3)
print(clusters_var)

# dendrogramma
plot(hc_var, main = "Clustering delle variabili",
     xlab = "", sub = "", cex = 0.6)

# heatmap delle correlazioni
library(pheatmap)
corr_vars <- cor(X, use = "pairwise.complete.obs")
pheatmap(mat_corr,
         main = "Correlazione tra variabili",
         color = colorRampPalette(c("white", "#a1d99b", "#00441b"))(100))
