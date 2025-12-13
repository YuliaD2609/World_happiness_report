df <- read.csv(file.choose(), header = TRUE, sep = ",")

library(pheatmap)
vars_sc <- c("log_gdp_per_capita_sc",
             "social_support_sc",
             "positive_affect_sc",
             "negative_affect_sc",
             "healthy_life_expectancy_at_birth_sc",
             "freedom_to_make_life_choices_sc",
             "generosity_sc",
             "perceptions_of_corruption_sc"
)
X <- df[vars_sc]

# rimozione di righe con NA
X_complete <- X[complete.cases(X), ]
X_scaled <- scale(X_complete)

# matrice delle distanze
dist_euclidea <- dist(X, method = "euclidean")
dist_euclidea[1:10] #si stampano solo le prime 10 per avere una visione iniziale dei valori
mat_dist <- as.matrix(dist_euclidea)

# matrice di similarità
mat_similarity <- 1 - mat_dist / max(mat_dist)
round(mat_similarity[1:5, 1:5], 3)

# Clustering gerarchico
hc_var <- hclust(dist_var, method = "complete")

# calcolo PCA
pca <- prcomp(X_scaled, center = TRUE, scale. = TRUE)
#per determinare il contributo delle componenti
round(pca$rotation, 3)
summary(pca)

# screeplot
plot(pca, type = "l", main = "Screeplot PCA (medie per Paese e anno)")

# Dendrogramma
plot(hc_var, main = "Cluster delle variabili (in base alla correlazione con la felicità)",
     xlab = "Variabili", sub = "", cex = 0.9)

clusters_var <- cutree(hc_var, k = 3)
print(clusters_var)

# --- Heatmap per visualizzare la correlazione tra le variabili ---
pheatmap(mat_corr,
         main = "Heatmap di correlazione tra le variabili",
         color = colorRampPalette(c("white", "#74c476", "#00441b"))(100),
         display_numbers = TRUE,
         number_format = "%.2f")

cor_happiness <- cor(X_complete$happiness_score, X_complete[, -which(names(X_complete) == "happiness_score")])
print(round(cor_happiness, 3))
