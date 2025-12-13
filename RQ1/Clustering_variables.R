df <- read.csv(file.choose(), header = TRUE, sep = ",")

library(pheatmap)
all_sc <- c("log_gdp_per_capita_sc",
            "social_support_sc",
            "positive_affect_sc",
            "negative_affect_sc",
            "healthy_life_expectancy_at_birth_sc",
            "freedom_to_make_life_choices_sc",
            "generosity_sc",
            "perceptions_of_corruption_sc",
            "happiness_score_sc"
)

vars_sc <- c("log_gdp_per_capita_sc",
             "social_support_sc",
             "positive_affect_sc",
             "negative_affect_sc",
             "healthy_life_expectancy_at_birth_sc",
             "freedom_to_make_life_choices_sc",
             "generosity_sc",
             "perceptions_of_corruption_sc"
)

X <- df[all_sc]

# rimozione di righe con NA
X_complete <- X[complete.cases(X), ]
X_scaled <- scale(X_complete)

# matrice di correlazione
mat_cor <- cor(X_scaled)
round(mat_cor, 3)
pheatmap(mat_cor,
         main = "Matrice di correlazione",
         fontsize = 7,
         angle_col = 45,
         display_numbers = TRUE,
         number_format = "%.2f")

X <- df[vars_sc]

# rimozione di righe con NA
X_complete <- X[complete.cases(X), ]
X_scaled <- scale(X_complete)
# calcolo PCA
pca <- prcomp(X_scaled, center = TRUE, scale. = TRUE)
summary(pca)
# per determinare il contributo delle componenti
round(pca$rotation, 3)
# screeplot
plot(pca, type = "l",  main="Screeplot PCA")

# si prendono in considerazione i primi
scores <- pca$x[,1:3]
# matrice delle distanze
dist_euclidea <- dist(scores, method = "euclidean")
dist_euclidea[1:10] # si stampano solo le prime 10 per avere una visione iniziale dei valori
mat_dist <- as.matrix(dist_euclidea)
rownames(mat_dist) <- rownames(X_scaled)
colnames(mat_dist) <- rownames(X_scaled)
round(mat_dist[1:10, 1:10], 3)

# matrice di similarità
mat_similarity <- 1 - mat_dist / max(mat_dist)
rownames(mat_similarity) <- rownames(X_scaled)
colnames(mat_similarity) <- rownames(X_scaled)
round(mat_similarity[1:10, 1:10], 3)

# matrice di covarianza
mat_cov <- cov(X_scaled)
round(mat_cov, 3)
pheatmap(mat_cov,
         main = "Matrice di covarianza",
         fontsize = 8,
         angle_col = 45,
         display_numbers = TRUE,
         number_format = "%.2f")

# matrice di non omogeneità
non_omogeneity <- apply(X_scaled, 1, var)
summary(non_omogeneity)

# clustering gerarchico
dist_vars <- dist(1 - mat_cor) 
hc <- hclust(dist_vars, method="complete")

par(mfrow = c(1, 2)) 
# dendrogramma
plot(hc, main="Dendrogramma",
     xlab="Variabili", ylab="Distanza", cex=0.6)
rect.hclust(hc, k=2, border="red")
rect.hclust(hc, k=3, border="green")
par(mfrow = c(1, 1))

library(cluster)

sil_width <- numeric()

for (k in 2:8) {
  km <- kmeans(scores, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(scores))
  sil_width[k] <- mean(sil[, 3])
}

plot(2:8, sil_width[2:8], type = "b",
     xlab = "Numero di cluster (k)",
     ylab = "Silhouette media",
     main = "Silhouette Method")


# taglio a 2 cluster
clusters <- cutree(hc, k=2)
clusters

plot(scores[,1], scores[,2],
     col=clusters, pch=19, cex=0.7,
     xlab="PC1", ylab="PC2",
     main="Clusters")

